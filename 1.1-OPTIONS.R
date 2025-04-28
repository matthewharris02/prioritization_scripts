##%##########################################################################%##
#           Set up options to be shared between scripts                        #
##%##########################################################################%##
# RESOLUTION ====
# Set shared resolution for all layers
# Relative to 1km at equator (or 30 arcseconds in non-equal area projection)
RES <- 5

# PROJECTION ====
# Set EPSG to the EPSG code (e.g., EPSG:4326, ESRI:54009)
# Set PROJ to the text that you want to label the files with (e.g., moll, laea)
EPSG <- "ESRI:54009" # EPSG code for the CRS
PROJ <- "moll" # Label ONLY

# EXTENT ====
# This has been set to the maximum rounded extent within Mollweide (ESRI:54009)
#       for resolutions up to 20 km
# order: xmin, xmax, ymin, ymax; units: units of the projection
EXT <- c(-18040000, 18040000, -9020000, 9020000)

# HFP Range ====
# the upper and lower values to set the range that we define 'intermediate' HFP
hfp_lower <- 3
hfp_upper <- 50 # Set to maximum so there is no upper

# Helper function to create named list of directory variables for multiple dir_ids
create_info <- function(dir_out) {
    tribble(
        ~vars,           ~dirs,
        "dir_out",        dir_out,
        "dir_ft",         file.path(dir_out, "features"),
        "dir_pu",         file.path(dir_out, "planning_units"),
        "dir_proc",       file.path(dir_out, "processed"),
        "dir_inter",      file.path(dir_out, "intermediate_outputs"),
        "dir_analyze",    file.path(dir_out, "analysis"),
    ) |>
        deframe()
}