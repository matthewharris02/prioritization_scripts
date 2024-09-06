library(terra)
library(data.table)
library(arrow)
library(tidyverse)


create_cells <- function(){
    pu_rast <- c("countries", "lulc") |>
        map(~file.path(dir_out, "planning_units", fn_template(.x))) |>
        map(~rast(.x)) |>
        rast()

    hfp_int <- rast(file.path(dir_out, "planning_units", str_glue("intermediateHFP_{hfp_lower}_{hfp_upper}_excludeNotNat_{RES}km_{PROJ}.tif")))
    ecoregions <- rast(file.path(dir_out, "planning_units", fn_template("ecoregions_withIceRock")))

    ncp_present <- list.files(file.path(dir_out, "ncp"),
                       pattern = "_mask.tif$",
                       full.names = TRUE)
    ncp_names <- ncp_present |>
        basename() |>
        str_remove(".tif") |>
        str_extract("(ncp_[A-Za-z]+)")
    ncp_rast <- ncp_present |>
            map(~rast(.x)) |>
        rast()

    all_rast <- c(pu_rast, hfp_int, ecoregions, ncp_rast)
    names(all_rast) <- c("ISONUM", "lulc", "hfp", "ecoregions", ncp_names)
    pu_vals <- as.data.frame(all_rast,
                             xy = TRUE,
                             na.rm = FALSE) |>
        setDT()

    pu_vals <- pu_vals[!is.na(ISONUM),][hfp == 1,]


    pu_vals <- pu_vals[, id := 1:.N]
    # [, , by = .(ISONUM)]
    setcolorder(pu_vals, "id", before = 1)
    # setnames(pu_vals, names(pu_vals), c("id", "x", "y", "ISONUM", "lulc", "hfp", ncp_names))

    # Option to write as partioned dataset, but not sure there is a point
    write_dataset(pu_vals,
                  file.path(dir_out, "processed", "global_cells"),
                  partitioning = c("ISONUM"))

    # Alternative write as single parquet
    # write_parquet(pu_vals, file.path(dir_out, "processed", "global_cells.parquet"))
    # fwrite(pu_vals, file.path(dir_out, "processed/global_cells.csv"), verbose = TRUE)
}
