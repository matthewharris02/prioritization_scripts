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
# dir_wd <- "/mnt/sda/MH_restoration"
dir_wd <- "C:/Users/matthewh/LOCAL/projects_local/restoration/"
# dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_src <- dir_wd
dir_in <- file.path(dir_wd, "raw")



# Shared options
source(file.path(dir_src, "script_tools/1.1-OPTIONS.R"))
source(file.path(dir_src, "script_tools/3.0-helper_functions.R"))
# Directory-related variables

# HACKY: overwrite dir_* vars with dir_id to make it easier for different runs

# dir_id <- ""



# Helper function to create named list of directory variables for multiple dir_ids
create_info <- function(dir_out) {
    tribble(
        ~vars,           ~dirs,
        "dir_out",        dir_out,
        "dir_features",   file.path(dir_out, "features"),
        "dir_pu",         file.path(dir_out, "planning_units"),
        "dir_proc",       file.path(dir_out, "processed"),
        "dir_inter",      file.path(dir_out, "intermediate_outputs"),
        "dir_analyze",    file.path(dir_out, "analysis", "compare"),
    ) |>
        deframe()
}


dir_id1 <- ""
dir_out1 <- file.path(dir_wd, "work_in_progress", paste0(RES, "km", ifelse(dir_id1 == "", "", paste0("_", dir_id1))))
info_1 <- create_info(dir_out1)


dir_id2 <- "25_03_old_full"
dir_out2 <- file.path(dir_wd, "work_in_progress", paste0(RES, "km", ifelse(dir_id2 == "", "", paste0("_", dir_id2))))
info_2 <- create_info(dir_out2)


c(info_1, info_2) |>
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

grid_cell1 <- open_dataset(file.path(info_1["dir_proc"], "global_cells"),
                          partitioning = c("ISONUM"))  |>
    collect() |>
    setDT()

grid_cell2 <- open_dataset(file.path(info_2["dir_proc"], "global_cells"),
                          partitioning = c("ISONUM"))  |>
    collect() |>
    setDT()



comb <- grid_cell1[grid_cell2, on =.(x=x, y=y)]



comb <- comb[, c_c := ft_coastal == i.ft_coastal]
