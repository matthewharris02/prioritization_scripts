##%##########################################################################%##
#           Set up options to be shared between scripts                        #
#           V 30.08.2024    Matthew Harris and Vignesh Kamath                  #
##%##########################################################################%##
# RESOLUTION ====
# Set shared resolution for all layers
# Relative to 1km at equator (or 30 arcseconds in non-equal area projection)
RES = 5

# PROJECTION ====
# Set EPSG to the EPSG code (e.g., EPSG:4326, ESRI:54009)
# Set PROJ to the text that you want to label the files with (e.g., moll, laea)
EPSG = "ESRI:54009" # EPSG code for the CRS
PROJ = "moll" # Label ONLY

# EXTENT ====
# This has been set to the maximum rounded extent within Mollweide (ESRI:54009)
#       for resolutions up to 20 km
# order: xmin, xmax, ymin, ymax; units: units of the projection
EXT = c(-18040000, 18040000, -9020000, 9020000)

# HFP Range ====
# the upper and lower values to set the range that we define 'intermediate' HFP
hfp_lower = 3
hfp_upper = 50 # Set to maximum so there is no upper


# Directories ====
# Directories within the working directory
dir_out <- file.path(dir_wd, "work_in_progress", paste0(RES, "km"))
dir_features <- file.path(dir_out, "features")
dir_pu <- file.path(dir_out, "planning_units")
dir_proc <- file.path(dir_out, "processed")