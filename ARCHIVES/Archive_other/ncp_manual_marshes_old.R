# Manually sort salt marshes from WCMC2017 version
ncp_manual_marshes_old <- function(fn) {
    marshes <- st_read(file.path(dir_in, fn)) |>
        st_transform(st_crs(EPSG)) |>
        rasterize(rast_template, cover = TRUE)
    marshes_area <- marshes * (RES^2 * 1000^2) # manual area as we are using equal area
    writeRaster(marshes_area, file.path(dir_out, "ncp", fn_template("ncp_saltmarshes")),
                overwrite = TRUE)
}