require(tidyverse)
require(terra)


create_restorable_land <- function(hfp, lulc, hfp_lower, hfp_upper) {
    rcl_intermediate <- tibble(
        from    = c(0,         hfp_lower, hfp_upper),
        to      = c(hfp_lower, hfp_upper, 51),
        becomes = c(0,         1,         0)
    )
    intermediate_hfp <- classify(hfp, rcl_intermediate) |>
        writeRaster(file.path(dir_out, "planning_units",
                              fn_template("hfp_intermediate")),
                    overwrite = TRUE)

    rcl_lulc <- tibble(
        from    = c(0,  39, 89,  199),
        to      = c(39, 89, 199, 200),
        becomes = c(1,  0,  1,   NA)
    )

    lulc_exclude <- lulc |>
        classify(rcl_lulc) |>
        writeRaster(file.path(dir_out, "planning_units",
                              fn_template("cop_excludeNotNat")),
                    overwrite = TRUE)

    intermediate_hfp_lulc_exclude <- intermediate_hfp * lulc_exclude

    # Make sure output name includes the HFP bounds for easy identification
    out_name <- fn_template(paste0("intermediateHFP_", hfp_lower, "_",hfp_upper, "_excludeNotNat"))
    writeRaster(intermediate_hfp_lulc_exclude,
                file.path(dir_out, "planning_units", out_name),
                overwrite = TRUE)
}

