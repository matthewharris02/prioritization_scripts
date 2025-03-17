# Total value of ES within priority areas
# 0. Set up ====
## 0.1 Load libraries ====
print("Loading libraries")
library(data.table)
library(arrow)
library(tidyverse)
library(terra)

## 0.2 Useful variables ====
dir_wd <- "/mnt/sda/MH_restoration"
# dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_src <- dir_wd

# Shared options
source(file.path(dir_src, "script_tools/1.1-OPTIONS_20.R"))
source(file.path(dir_src, "script_tools/3.0-helper_functions.R"))

# RUNID
runid = ""

# Directory-related variables
dir_analyze <- file.path(dir_out, "analysis", "compare")

# 1. Load data ====
solution <- load_sol1("default", type = "csv") |> 
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
    grid_cell[, .(sum = sum(.SD, na.rm = TRUE)), by = rank, .SDcols = c(var)
              ][order(-rank)
                ][, cumsum := cumsum(sum)]
}

carbon <- cumul_es(grid_cell, "ft_carbon")

plot(carbon$rank, carbon$cumsum, xlim=rev(range(carbon$rank)))

up <- cumul_es(grid_cell, "ft_usefulplants")
plot(up$rank, up$cumsum, xlim=rev(range(up$rank)))

ramsar <- cumul_es(grid_cell, "ft_ramsar")
plot(ramsar$rank, ramsar$cumsum, xlim=rev(range(ramsar$rank)))


iucn <- cumul_es(grid_cell, "ft_iucnrichness")
plot(iucn$rank, iucn$cumsum, xlim=rev(range(iucn$rank)))

water <- cumul_es(grid_cell, "ft_waterquality")
plot(water$rank, water$cumsum, xlim=rev(range(water$rank)))

