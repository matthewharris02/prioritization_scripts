# Prepare raster NCPs using GDAL to make it quick
prepare_ncp_r_gdal <- function(ifile, ofile, method, gdalwarp_path) {
    args <- gdalwarp_args(method, ifile, ofile,
                          RES = RES,
                          EPSG = EPSG,
                          EXT = EXT)
    system2(gdalwarp_path, args, wait = T)
}


# Convert vector NCPs to raster by the area of each grid cell covered
prepare_ncp_v_area <- function(ncp_name) {
    ncp <- st_read(file.path(dir_in, ncp_fn_area[ncp_name])) |>
        st_crop(c(
            xmin = -180,
            ymin = -90,
            xmax = 180,
            ymax = 90
        )) |>
        st_transform(st_crs(EPSG)) |>
        rasterize(rast_template, cover = TRUE)
    ncp_area <- ncp * (RES^2 * 1000^2) # manual area as we are using equal area
    writeRaster(ncp_area, file.path(dir_out, "ncp", fn_template(ncp_name)),
                overwrite = TRUE)
}

# Convert vector NCP to raster by transferring the value of the vector to the
#   raster cells
prepare_ncp_v_raw <- function(ncp_name, field, fun) {
    ncp <- st_read(file.path(dir_in, ncp_fn_other[ncp_name])) |>
        st_transform(st_crs(EPSG)) |>
        select(all_of(field)) |>
        rasterize(rast_template,
                  field = field,
                  fun = fun
        )

    writeRaster(ncp, file.path(dir_out, "ncp", fn_template(ncp_name)),
                overwrite = TRUE)
}

# Convert marshes from vector where attribute is area to raster
# Done manually because it is weird data
ncp_manual_marshes <- function() {
    ncp <- st_read(file.path(dir_in, ncp_fn_other["ncp_saltmarshes"])) |>
        st_centroid() |>
        st_transform(st_crs(EPSG)) |>
        rasterize(rast_template, field = "areakm2", fun = "sum") |>
        writeRaster(file.path(dir_out, "ncp", fn_template("ncp_saltmarshes")),
                    overwrite = TRUE)

}



mask_ncp_pu <- function(mask_fn, extra = "") {
    pu_mask <- rast(file.path(dir_out, "planning_units", mask_fn))


    ncp_list <- list.files(file.path(dir_out, "ncp"),
                           recursive = T,
                           full.names = T,
                           pattern = glue::glue("*_{RES}km_{PROJ}.tif$"))

    file_names <- ncp_list |>
        basename() |>
        str_remove_all(".tif")

    ncp_list |>
        map(~rast(.x)) |>
        map(~mask(.x, pu_mask, maskvalue = c(0, NA))) |>
        walk2(
            .y = file_names,
            ~writeRaster(.x, file.path(dir_out, "ncp", paste0(.y, "_mask", extra, ".tif")), overwrite = TRUE)
        )
}


# ARCHIVE ====

# Automatic loop through NCPs and process through GDAL
prepare_ncp_r_gdal_loop <- function(ifile, ofile, methods, gdalwarp_path) {#
    if (!dir.exists(file.path(dir_out, "ncp"))) dir.create(file.path(dir_out, "ncp"))
    for (ncp in names(files)) {
        print(paste0("Processing: ", ncp, " ..."))
        ifile <- file.path(dir_in, files[ncp])
        ofile <- file.path(dir_out, "ncp", fn_template(ncp))

        if (!dir.exists(dirname(ofile))) dir.create(dirname(ofile))

        args <- gdalwarp_args(methods[ncp], ifile, ofile,
                              RES = RES,
                              EPSG = EPSG,
                              EXT = EXT)
        system2(gdalwarp_path, args, wait = T)
    }
}

prepare_ncp_r_alt <- function(ncp_name, fun = 'mean', method = 'bilinear') {
    ncp <- rast(file.path(dir_in, ncp_fn_r[ncp_name]))
    res_ncp <- res(ncp)[[1]]
    ncp_rpj <- ncp |>
        aggregate(fact = as.integer((RES*1000) / res_ncp), fun = fun, na.rm = TRUE) |>
        project(rast_template, method = method)
    writeRaster(ncp_rpj, file.path(dir_out, fn_template[ncp_name]), overwrite = TRUE)
}