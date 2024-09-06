require(tidyverse)
require(terra)
require(sf)

# Process countries layer ====
process_countries <- function() {
    countries <- st_read(file.path(dir_in, pu_fn["countries"]))
    countries_key <- tibble(
        ISO3CD = unique(countries$iso3cd)
    ) |>
        arrange(ISO3CD) %>%
        mutate(
            ISONUM = 1:nrow(.)
        ) |>
        write_csv(file.path(dir_out, "planning_units/global_countries_key.csv"))

    countries_rast <- countries |>
        st_transform(st_crs(EPSG)) |>
        left_join(countries_key, by = join_by("iso3cd" == "ISO3CD")) |>
        rasterize(rast_template, field = "ISONUM") |>
        writeRaster(file.path(dir_out, "planning_units",
                              fn_template("countries")),
                    overwrite = TRUE)

    countries_mask <- countries_rast |>
        classify(cbind(-Inf, Inf, TRUE)) |>
        writeRaster(file.path(dir_out, "planning_units",
                              fn_template("countries_mask")),
                    overwrite = TRUE)
}

pu_area <- function(pu) {
    area <- cellSize(pu, unit = "km") |>
        writeRaster(file.path(dir_out, "planning_units", fn_template("pu_area")),
                    overwrite = TRUE)
}
# Process Human Footprint ====
process_hfp <- function(gdalwarp_path) {
    ifile <- file.path(dir_in, pu_fn["hfp"])
    ofile <- file.path(dir_out, "planning_units", fn_template("hfp"))
    # args_additional <- "-srcnodata 128 -dstnodata -128"
    args <- gdalwarp_args("mode", ifile, ofile, EPSG, RES, EXT) #, args = args_additional)
    system2(gdalwarp_path, args, wait = TRUE)
    print("Finished HFP process...")
}
# Process Copernicus LULC ====
process_landcover <- function(gdalwarp_path) {
    ifile <- file.path(dir_in, pu_fn["lulc"])
    ofile <- file.path(dir_out, "planning_units", fn_template("lulc"))
    args <- gdalwarp_args("mode", ifile, ofile, EPSG, RES, EXT)
    system2(gdalwarp_path, args, wait = TRUE)
    print("Finished LULC process ....")
}

# process_landcover2 <- function(gdalwarp_path) {
#     gdalwarp_args <- function(method, ifile, ofile, EPSG, RES, EXT) {
#         # Method: 'near', 'bilinear', 'cubic' ... from gdalwarp
#         glue::glue('-overwrite -t_srs {EPSG} -r {method} -tr {1000*RES} {1000*RES} -te {EXT[1]} {EXT[3]} {EXT[2]} {EXT[4]} -of GTiff -co compress=lzw "{ifile}" "{ofile}"')
#     }
#     ifile <- file.path(dir_in, pu_fn["lulc"])
#     ofile <- file.path(dir_out, "planning_units", fn_template("lulc"))
#     if (!dir.exists(dirname(ofile))) dir.create(dirname(ofile))
#     args <- gdalwarp_args('near', ifile, ofile,
#                               RES = RES,
#                               EPSG = EPSG,
#                               EXT = EXT)
#     system2(gdalwarp_path, args, wait = TRUE)
# }