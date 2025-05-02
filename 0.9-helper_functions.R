##%##########################################################################%##
#       Helper functions for pre-processing inputs                             #
#           V 30.08.2024    Matthew Harris and Vignesh Kamath                  #
##%##########################################################################%##
# Helper function for producing gdalwarp argument values
gdalwarp_args <- function(method, ifile, ofile, EPSG, RES, EXT, compress = TRUE, args = "") {
    # Arguments:
    #   Method: 'near', 'bilinear', 'cubic' ... from gdalwarp
    #   ifile: full path to and filename of the input file
    #   ofile: full path to and filename of the desired output file
    #   EPSG: EPSG code (e.g., EPSG:4326)
    #   RES: resolution in kilometres
    #   EXT: desired output extent
    #   args: additional arguments to be passed to GDAL; must be fully-formatted

    glue::glue("-overwrite -t_srs {EPSG} -r {method} ",
               "-tr {1000*RES} {1000*RES} ",
               "-te {EXT[1]} {EXT[3]} {EXT[2]} {EXT[4]} ",
               "-of GTiff {args}",
               ifelse(compress, "-co compress=lzw  ", ""),
               "'{ifile}' '{ofile}'")
}

# Prepare raster NCPs using GDAL to make it quick
prepare_ft_r_gdal <- function(ifile, ofile, method, gdalwarp_path) {
    args <- gdalwarp_args(method, ifile, ofile,
                          RES = RES,
                          EPSG = EPSG,
                          EXT = EXT)
    system2(gdalwarp_path, args, wait = T)
}


# Convert vector NCPs to raster by the area of each grid cell covered
prepare_ft_v_area <- function(ncp_name) {
    ncp <- st_read(file.path(dir_in, ft_fn_area[ncp_name])) |>
        st_crop(c(
            xmin = -180,
            ymin = -90,
            xmax = 180,
            ymax = 90
        )) |>
        st_transform(st_crs(EPSG)) |>
        rasterize(rast_template, cover = TRUE)
    ncp_area <- ncp * (RES^2 * 1000^2) # manual area as we are using equal area
    writeRaster(ncp_area, file.path(dirs["dir_ft"], fn_template(ncp_name)),
                overwrite = TRUE)
}

# Convert vector NCP to raster by transferring the value of the vector to the
#   raster cells
prepare_ft_v_raw <- function(ncp_name, field, fun) {
    ncp <- st_read(file.path(dir_in, ft_fn_other[ncp_name])) |>
        st_transform(st_crs(EPSG)) |>
        select(all_of(field)) |>
        rasterize(rast_template,
                  field = field,
                  fun = fun
        )

    writeRaster(ncp, file.path(dirs["dir_ft"], fn_template(ncp_name)),
                overwrite = TRUE)
}