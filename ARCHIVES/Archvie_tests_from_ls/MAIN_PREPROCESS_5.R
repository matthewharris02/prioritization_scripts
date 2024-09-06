##%##########################################################################%##
# REQUIRED PACKAGES for entire script:
#       - sf, terra, tidyverse, arrow, patchwork, data.table
#   - Though NOTE that these packages are loaded as and when needed by each
#   - sub-script (e.g., when sourcing)
##%##########################################################################%##
library(terra)
library(tidyverse)
library(tictoc)
# Constants ====
## Directory-related variables ====
# NOTE: paths should NOT end with a slash as file.path() will sort this
#dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_wd <- "/home/matthewh@internal.wcmc/projects_active/p09217_RestorationPotentialLayer/global2024_v2"
## RESOLUTION ====
#Set shared resolution for all layers
# Relative to 1km at equator (or 30 arcseconds in non-equal area projection)
RES = 5 # Relative to 1


# Directory set-up:
#   BASE
#     - raw                 | raw input data (i.e., downloaded data)
#     - script_tools        | these scripts
#     - work_in_progress    | output data (both from pre-processing, and final)
dir_in <- file.path(dir_wd, "raw")
dir_out <- file.path(dir_wd, str_glue("work_in_progress"))


## PROJECTION ====
# Set EPSG to the EPSG code (e.g., EPSG:43267, ESRI:54009)
# Set PROJ to the text that you want to label the files with (e.g., moll, laea)
EPSG = "ESRI:54009" # EPSG code for the CRS
PROJ = "moll"

## EXTENT ====
# This has been set to the maximum rounded extent within Mollweide (ESRI:54009)
#       for resolutions up to 20 km
# order: xmin, xmax, ymin, ymax
EXT = c(-18040000, 18040000, -9020000, 9020000)

## Template raster
rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)


# Load information ====
variables <- read_csv(file.path(dir_out, "preprocess_info.csv"))


pu_fn <- variables |>
    select(var, fn_raw) |>
    filter(!grepl("ncp", var)) |>
    deframe()

ncp <- variables |>
    filter(grepl("ncp", var)) |>
    filter(!is.na(fn_raw))



# Helper function to make sure all output files named consistently :)
fn_template <- function(name, extra = "", ext = ".tif"){
    return(paste0(name, "_", RES, "km_", PROJ, extra, ext))
}


##%##########################################################################%##
# 1. Pre-processing Part I ====
##%##########################################################################%##
#  - Process all raw data to the same
setwd(dir_wd)
tic()
## 1.1 Processing the planning unit inputs ====
print("Processing PU")
source(file.path(dir_wd, "script_tools/v3/1.1-p1-planning_units_inputs.R"))
# Choose which need processing
#process_countries()
#process_hfp()
#process_landcover()
# pu_area(rast(file.path(dir_out, "planning_units", fn_template("countries"))))
## 1.2 Create restorable land planning units ====
# Load preprocessed layers from previous step
hfp <- rast(file.path(dir_out, "planning_units", fn_template("hfp")))
lulc <- rast(file.path(dir_out, "planning_units", fn_template("lulc")))

# Load function
print("Preparing restorable")
source(file.path(dir_wd, "script_tools/v3/1.2-p1-restorable_land.R"))
# Create restorable land layer from hfp and land use
# The last two arguments are the upper and low values for HFP (TODO: inclusive or exclusive)
#   that are used to define 'intermediate' HFP
hfp_lower = 3
hfp_upper = 50 # Set to maximum so there is no upper
#create_restorable_land(hfp, lulc, hfp_lower, hfp_upper)

## 1.3 Process ecoregions ====
print("Processing ecoregions")
source(file.path(dir_wd, "script_tools/v3/1.3-p1-ecoregions.R"))

## 1.4 Process NCPs ====
# NOTE: all NCP and biodiversity layers (ncp_bio) should ALREADY be rasters.
#   Processing of vectors (e.g., KBAs, salt marshes) is done separately

### Vector processing ====
# Done manually for now.
# Written in functions just so it is easier to comment it out.
print("Processing vectors")
source(file.path(dir_wd, "script_tools/v3/1.4-p1-ncp.R"))
ncp_fn_v <- ncp |>
    filter(type == "vec") |>
    select(var, fn_raw) |>
    deframe()

#ncp_manual_coastal(ncp_fn_v["ncp_coastal"])
# pu_area <- rast(file.path(dir_out, "planning_units", fn_template("pu_area")))
#ncp_manual_kba(ncp_fn_v["ncp_kba"])
#ncp_manual_ramsar(ncp_fn_v["ncp_ramsar"])
#ncp_manual_marshes(ncp_fn_v["ncp_saltmarshes"])

# Path to gdalwarp on windows; On linux I think it can just be set to "gdalwarp"
# gdalwarp_path <- "C:\\Program Files\\QGIS 3.36.1\\bin\\gdalwarp.exe"
gdalwarp_path <- "gdalwarp"
print("Processing rasters NCPs")
ncp_fn_r <- ncp |>
    filter(type == "ras") |>
    select(var, fn_raw) |>
    deframe()

ncp_method <- ncp |>
    filter(type == "ras") |>
    select(var, method) |>
    deframe()

ncp_list <- unlist(select(ncp, var), use.names = FALSE)

# Do pre-processing on the ncp_bio rasters
#prepare_ncp_r(ncp_fn_r, ncp_method, gdalwarp_path)
# Mask with planning units so only necessary ones kept
#mask_ncp_pu()


##%##########################################################################%##
# 2. Pre-processing Part 2 ====
##%##########################################################################%##
print("Creating grid cells")
source(file.path(dir_wd, "script_tools/v3/2.1-global_cells.R"))

#create_cells(ncp_list)
toc()
