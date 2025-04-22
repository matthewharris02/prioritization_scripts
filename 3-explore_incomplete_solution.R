#
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
source(file.path(dir_src, "script_tools/1.1-OPTIONS.R"))
source(file.path(dir_src, "script_tools/3.0-helper_functions.R"))
# Directory-related variables
dir_analyze <- file.path(dir_out, "analysis")



## Prioritzr-related options ====
solver <- "cbc"        # Which solver: cbc, (lp)symphony
opt_gap <- 0.01       # Choose gap for solver
opt_threads <- 1      # Choose number of threads (ONLY for CBC solver)
runid = ""


info_str <- paste0("{solver}_{RES}km_{opt_gap}g_{opt_threads}t_{budget}b_",
                   ifelse(runid == "", "default", runid))


## 0.5 Template raster ====
rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)


grid_cell <- open_dataset(file.path(dir_proc, "global_cells"),
                          partitioning = c("ISONUM"))  |>
    collect() |>
    left_join(solution, by = "id") |>
    setDT()

# 1 Load single partial solution ====
budget <- 0.5
info_str_g <- glue(info_str)
solution <- load_sol_partial("default", dir_out, info_str_g, type = "csv")|>
    select(id, solution_1) |>
    rename(rank = solution_1)

# 2 Load arbitrary number of solutions ====
solutions <- list()
max_budget <- 0.5

# adjustment to make max 20
adj <- 20 - 20 * max_budget

for (budget in seq(0.05, max_budget, 0.05)) {
    info_str_g <- glue(info_str)
    solution <- load_sol_partial("default", dir_out, info_str_g, type = "csv")|>
        select(id, solution_1) |>
        rename(rank = solution_1)
    solutions[[budget * 20]] <- solution
}

joined_solution <- solutions |>
    reduce(left_join, by = "id") |>
    setDT()

combined_solution <- joined_solution[, `:=` (final = rowSums(.SD)),
                               .SDcols = !c("id")
                               ][, .(id, final)
                                 ][, final := fifelse(final != 0, final + adj, 0)]
combined_solution <- combined_solution |>
    left_join(select(grid_cell, c("id", "x", "y")), by = "id") |>
    write_csv(file.path(dir_analyze,
                        glue::glue("solution_partial_{solver}_{RES}km_{opt_gap}g_{opt_threads}t_{max_budget}maxBudget_",
                                   ifelse(runid == "", "default", runid),
                                   ".csv")
                                   ))

## 7.2 Convert matrix to raster ====
r <- rast(
    combined_solution[, c("x", "y", "final")],
    crs = crs(EPSG),
    extent = ext(rast_template
    )
)

writeRaster(r,
            file.path(dir_analyze,
                      glue::glue("solution_partial_{solver}_{RES}km_{opt_gap}g_{opt_threads}t_{max_budget}maxBudget_",
                                 ifelse(runid == "", "default", runid),
                                 ".tif")
                                   ),
            overwrite = TRUE)
