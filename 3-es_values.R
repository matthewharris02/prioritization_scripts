# Total value of ES within priority areas
# 0. Set up ====
## 0.1 Load libraries ====
print("Loading libraries")
library(data.table)
library(arrow)
library(tidyverse)
library(terra)
library(glue)
library(ggplot2)

## 0.2 Useful variables ====
dir_wd <- "/mnt/sda/MH_restoration"
dir_wd <- "C:/Users/matthewh/LOCAL/projects_local/restoration/"
# dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_src <- dir_wd
dir_in <- file.path(dir_wd, "raw")



# Shared options
source(file.path(dir_src, "script_tools/1.1-OPTIONS_20.R"))
source(file.path(dir_src, "script_tools/3.0-helper_functions.R"))
# Directory-related variables

# HACKY: overwrite dir_* vars with dir_id to make it easier for different runs

dir_id <- "20"
# dir_id <- ""
dir_out <- file.path(dir_wd, "work_in_progress", paste0(RES, "km", ifelse(dir_id == "", "", paste0("_", dir_id))))


dir_features <- file.path(dir_out, "features")
dir_pu <- file.path(dir_out, "planning_units")
dir_proc <- file.path(dir_out, "processed")
dir_inter <- file.path(dir_out, "intermediate_outputs")


dir_analyze <- file.path(dir_out, "analysis", "compare")

c(dir_analyze) |>
    walk(\(x) if(!dir.exists(x)) { dir.create(x, recursive = TRUE)})

# RUNID
runid = ""

# Load in features etc.
# drop_features: Select which features to drop
#   !! each ones should be a string
#   !! To include all/exclude none leave empty `c()` or as `NULL`
drop_feature <-  c()

exclude_feature <- str_flatten(drop_feature, "|") # Create regex string to exclude vars
if (is.null(drop_feature)) {exclude_feature <- "^$"} # Work-around for matching nothing so that if drop_features is empty, it selects them all

variables <- read_csv(file.path(dir_in, "preprocess_info.csv")) |>
    filter(grepl("ft_*", var)) |> # select only ft_ variables
    filter(!grepl(exclude_feature, var)) |> # exclude the variables in drop_feature
    select(var) |>
    unlist(use.names = FALSE)



# 1. Load data ====
solution <- load_sol2("default", type = "csv") |>
    select(id, final) |>
    rename(rank = final)

grid_cell <- open_dataset(file.path(dir_proc, "global_cells"),
                          partitioning = c("ISONUM"))  |>
    collect() |>
    left_join(solution, by = "id") |>
    setDT()


# MAIN ====
# Calculate cumulative sum of ES values by rank
cumul_es <- function(DT, var) {
    dt <- grid_cell[, .(sum = sum(.SD, na.rm = TRUE)), by = rank, .SDcols = c(var)
              ][order(-rank)
                ][, cumsum := cumsum(sum)]
    setnames(
        dt,
      2:3,
        c(glue("{var}_sum"), glue("{var}_cumsum"))
    )
}

# fts <- c("ft_carbon", "ft_usefulplants", "ft_ramsar")

cumul_es_ft <- lapply(variables, \(x) cumul_es(grid_cell, x) |> tibble()) |>
    reduce(\(x, y) left_join(x, y, by = "rank"))

cumul_es2 <- cumul_es_ft |>
    pivot_longer(
        cols = ends_with(c("_sum", "_cumsum")),
        names_to = c("ft", ".value"),
        names_pattern = "ft_(.*)_(.+)"
    )



labeller_ft <- c(
    "carbon" = "Carbon ()",
    "coastal" = "Coastal protection ()",
    "iucnrichness" = "Rarity-weighted richness ()",
    "kba" = "KBAs (area)",
    "ramsar" = "Ramsar sites (area)",
    "saltmarshes" = "Saltmarshes (area)",
    "waterquality" = "Water quality",
    "usefulplants" = "Useful plants ()",
    "mangroves" = "Mangroves (area)"
)


# 20 budgets
x_lim <- c(20, 1)
x_breaks <- c(20, 15, 10, 5, 1)
x_breaks_minor <- seq(20, 1, -2)

# 10 budgets
x_lim <- c(10, 1)
x_breaks <- c(10, 5, 1)
x_breaks_minor <- seq(10, 1, -2)

p <- ggplot(cumul_es2, aes(x = rank)) +
    geom_point(aes(y = cumsum)) +
    # geom_col(aes(y = sum)) +
    facet_wrap(~ft, scales = 'free_y',
               # strip.position = "left",
               labeller = as_labeller(labeller_ft)) +
    scale_y_continuous(
        # sec.axis = sec_axis(sum/cumsum)
    ) +
    scale_x_reverse(
        lim = x_lim,
        breaks = x_breaks,
        minor_breaks = x_breaks_minor
    ) +
    labs(
        title = "Cumulative sum of ecosystem service values",
        # y = "Cumulative sum of feature value",
        x = "Priority ranking"
    ) +
    theme(
        strip.background =  element_blank(),
        strip.placement = "outside",
        strip.text = element_text(
            size = 14
        ),
        axis.text.y =  element_text(
            size = 11
        ),
        plot.title =  element_text(
            hjust = 0.5,
            size = 15,
            face = 'bold'
        )
    )

p

ggsave(
    filename = file.path(dir_analyze,
                         paste0(
                             "plot_cumul_ft",
                             ifelse(runid == "", "_default", paste0("_", runid)),
                             ifelse(dir_id == "", "", paste0("_", dir_id)),
                             ".png"
                             )),
    plot = p,
    units = "cm",
    width = 25,
    height = 25
)



