library(arrow)
library(data.table)
library(terra)
library(tidyverse)


dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
# dir_wd <- "/home/matthewh@internal.wcmc/projects_active/p09217_RestorationPotentialLayer/global2024_v2"

## PROJECTION ====
# Set EPSG to the EPSG code (e.g., EPSG:43267, ESRI:54009)
# Set PROJ to the text that you want to label the files with (e.g., moll, laea)
EPSG = "ESRI:54009" # EPSG code for the CRS
PROJ = "moll" # Label ONLY
RES = 20 # Resolution: relative to 1km/30arsec
EXT = c(-18040000, 18040000, -9020000, 9020000) # IF PROJ changes, change

# Constants ====

## Directory-related variables ====
# NOTE: paths should NOT end with a slash as file.path() will sort this
# Directory set-up:
#   BASE
#     - raw                 | raw input data (i.e., downloaded data)
#     - script_tools        | these scripts
#     - work_in_progress    | output data (both from pre-processing, and final)
dir_in <- file.path(dir_wd, "raw")
dir_out <- file.path(dir_wd, paste0("work_in_progress/", RES, "km"))



pu_vals <- open_dataset(file.path(dir_out, "processed", "global_cells"),
                        partitioning = c("ISONUM"))
ncp_present <- names(pu_vals)[grepl("ncp", names(pu_vals))]

for (ncp in ncp_present) {
    pu_vals_ncp <- pu_vals |>
        select(c("id", "ISONUM", ncp)) |>
        collect() |>
        setDT() |>
        mutate(across(
            starts_with("ncp"),
            ~scales::rescale(.x,
                                to = c(0,1),
                                from = c(0, max(.x, na.rm = TRUE)))))

    pu_vals2 <- pu_vals_ncp |>
        dcast(
            id ~ ISONUM,
            value.var = c(ncp)
        )

    setnames(pu_vals2, names(pu_vals2)[-1], paste0(ncp, "_", names(pu_vals2)[-1]))
    write_parquet(pu_vals2, file.path(dir_out, "processed/split", paste0(ncp, ".parquet")))
}


# combined <- list.files(file.path(dir_out, "processed/split/"), full.names = TRUE) |>
#     lapply(function(filename) { read_parquet(filename)}) |>
#     reduce(left_join, by = "id")
