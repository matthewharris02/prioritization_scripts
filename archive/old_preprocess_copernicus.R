# Old pre-processing of Copernicus LULC


print("* Processing LULC *")
fns_lulc <- variables |>
    select(var, fn_raw) |>
    filter(grepl("lulc", var)) |>
    filter(var != "lulc_discrete") |>
    deframe()

# Convert fractional cover to correct resolution
for (fn_lulc in names(fns_lulc)) {
    ifile <- file.path(dir_in, pu_fn[fn_lulc])
    ofile <- file.path(dirs["dir_inter"], fn_template(fn_lulc))
    args <- gdalwarp_args("average", ifile, ofile, EPSG, RES, EXT, args = "-wm 2G -co GDAL_CACHEMAX=8000")
    print(args)
    system2(gdalwarp_path, args, wait = TRUE)
}

# Create binary 'other excluded land' (non-converted) raster
# pwater <- rast(file.path(dirs["dir_inter"], fn_template("lulc_pwater")))
# swater <- rast(file.path(dirs["dir_inter"], fn_template("lulc_swater")))
# moss <-   rast(file.path(dirs["dir_inter"], fn_template("lulc_moss")))
snow <-   rast(file.path(dirs["dir_inter"], fn_template("lulc_snow")))
# bare <-   rast(file.path(dirs["dir_inter"], fn_template("lulc_bare")))

# [Note to self: Faster through R than gdal_calc here]
# Include/restorable = 1, exclude = 0
lulc_other <- snow |>
    classify(data.frame(
        from    = c(0,  50),
        to      = c(50, Inf),
        becomes = c(1,  0)
    ),
    right = FALSE # so >= 50
    )
writeRaster(lulc_other, file.path(dirs["dir_pu"], fn_template("lulc_other")), overwrite = TRUE)