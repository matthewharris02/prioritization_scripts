require(glue)
# Helper function for producing gdalwarp argument values
gdalwarp_args <- function(method, ifile, ofile, EPSG, RES, EXT, args = "") {
    # Arguments:
    #   Method: 'near', 'bilinear', 'cubic' ... from gdalwarp
    #   ifile: full path to and filename of the input file
    #   ofile: full path to and filename of the desired output file
    #   EPSG: EPSG code (e.g., EPSG:4326)
    #   RES: resolution in kilometres
    #   EXT: desired output extent
    #   args: additional arguments to be passed to GDAL; must be fully-formatted

    glue::glue('-overwrite -t_srs {EPSG} -r {method} ',
               '-tr {1000*RES} {1000*RES} ',
               '-te {EXT[1]} {EXT[3]} {EXT[2]} {EXT[4]} ',
               '-of GTiff -co compress=lzw {args} ',
               '"{ifile}" "{ofile}"')
}
