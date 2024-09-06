##%##########################################################################%##
#       Script for pre-processing for spatial prioritisation                   #
#           V 05.08.2024    Matthew Harris and Vignesh Kamath                  #
##%##########################################################################%##
# REQUIRED packages:
#       - sf, terra, tidyverse, arrow, data.table, tictoc, prioritzr, glue
#   - Though NOTE that some packages are loaded as and when needed by each
#       sub-script (e.g., when sourcing).
##%##########################################################################%##
# NOTES:
#   - This script relies on multiple sub-scripts that are individually sourced.
#   - Most sub-scripts contain functions that are then called. This allows for
#       individual processing to be turned on/off easily and makes each script
#       more manageable than having one long script.
#   - You can specify whether you only want parts of this script to be ran
##%##########################################################################%##
# REQUIRED BEFORE STARTING - set OPTIONS (external script):
#   - Define the 'options' in the '0-OPTIONS.R' script
#   - This is the parameters of the solution (e.g., resolution, extent,
#       projection) that are shared amongst 0.1, 0.2 and 0.3
##%##########################################################################%##
# ENSURE CORRECT SETTINGS in section 0.1
#   - Set the working directory in this script
#   - This define what parts of the script need to be ran
#   - Set a 'runid' in order to distinguish between multiple runs
#       if it is the same RES
#   - Set gdalwarp_path
##%##########################################################################%##

##%##########################################################################%##
# 0.1 MAKE CHANGES HERE ====
## Set working directory ====
dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
# dir_wd <- "/home/matthewh@internal.wcmc/projects_active/p09217_RestorationPotentialLayer/global2024_v2"
## Set run-id ====
runid = ""
## Options for choosing what to pre-process ====
pp_pu = TRUE
pp_restorable = TRUE
pp_ecoregions = TRUE
pp_ncp_vec = TRUE
pp_ncp_ras = TRUE
pp_ncp_mask = TRUE
pp_cells = TRUE

# automatically create needed sub directories
auto_dir = TRUE

# Path to gdalwarp
# If gdal is on the user or system path, just set to "gdalwarp"
# Else, set to the full path:
#   On windows, probably: C:/OSGeo4W/bin/gdalwarp.exe
#   On linux, probably already on path :)
gdalwarp_path <- "gdalwarp"
##%##########################################################################%##
# 0.2 - SET UP ====
## Load libraries
library(terra)
library(tidyverse)
library(sf)
library(glue)
## Start timing ====
print("Starting")
start <- Sys.time()

## Load dependency scripts ====
# Load options
source(file.path(dir_wd, "script_tools/v3/0.1-OPTIONS.R"))
# Load gdalwarp_args() helper function
source(file.path(dir_wd, "script_tools/v3/gdalwarp_args.R"))
##%##########################################################################%##
# 0.3 Automatically defined variables ====
# The following variables are automatic, and use the above information
## Directory-related variables
# NOTE: paths should NOT end with a slash as file.path() will sort this
# Directory set-up:
#   BASE
#     - raw                 | raw input data (i.e., downloaded data)
#     - script_tools        | these scripts
#     - work_in_progress    | output data (both from pre-processing, and final)
dir_in <- file.path(dir_wd, "raw")
out_subdir <- ifelse(runid == "", paste0(RES, "km"), paste0(RES, "km_", runid))
dir_out <- file.path(dir_wd, "work_in_progress", out_subdir)

if (auto_dir) {
    if (!dir.exists(dir_out)) {dir.create(dir_out, recursive = TRUE)}
    sub_dirs <- c("output", "processed", "logs", "planning_units", "ncp")
    file.path(dir_out, sub_dirs) |>
        walk(~if(!dir.exists(.x)) { dir.create(.x, recursive = TRUE)})
}

##%##########################################################################%##
# 0.4 Load information ====
variables <- read_csv(file.path(dir_in, "preprocess_info.csv"))

pu_fn <- variables |>
    select(var, fn_raw) |>
    filter(!grepl("ncp", var)) |>
    deframe()

ncps <- variables |>
    filter(grepl("ncp", var)) |>
    filter(!is.na(fn_raw))


## Helper functions ====
# Helper function to make sure all output files named consistently :)
fn_template <- function(name, extra = "", ext = ".tif"){
    return(paste0(name, "_", RES, "km_", PROJ, extra, ext))
}


## Template raster
rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)
sf_use_s2(FALSE) ## To avoid intersecting polygon errors
##%##########################################################################%##
# 1. Pre-processing Part I ====
##%##########################################################################%##
#  - Process all raw data to the same
setwd(dir_wd)

## 1.1 Processing the planning unit inputs ====
if (pp_pu) {
    print("Processing PU ====")
    source(file.path(dir_wd, "script_tools/v3/1.1-p1-planning_units_inputs.R"))
    # Choose which need processing
    process_countries()
    process_hfp(gdalwarp_path)
    process_landcover(gdalwarp_path)
}
## 1.2 Create restorable land planning units ====
# Load preprocessed layers from previous step
if (pp_restorable) {
    hfp <- rast(file.path(dir_out, "planning_units", fn_template("hfp")))
    lulc <- rast(file.path(dir_out, "planning_units", fn_template("lulc")))

    # Load function
    print("Preparing restorable ====")
    source(file.path(dir_wd, "script_tools/v3/1.2-p1-restorable_land.R"))
    # Create restorable land layer from hfp and land use
    # The last two arguments are the upper and low values for HFP (TODO: inclusive or exclusive)
    #   that are used to define 'intermediate' HFP
    create_restorable_land(hfp, lulc, hfp_lower, hfp_upper)
}
## 1.3 Process ecoregions ====
if (pp_ecoregions) {
    print("Processing ecoregions ====")
    # Sourcing this script automatically runs the processing
    source(file.path(dir_wd, "script_tools/v3/1.3-p1-ecoregions.R"))
}
## 1.4 Process NCPs ====
# NOTE: all NCP and biodiversity layers (ncp_bio) should ALREADY be rasters.
#   Processing of vectors (e.g., KBAs, salt marshes) is done separately
source(file.path(dir_wd, "script_tools/v3/1.4-p1-ncp.R"))


### Vector processing ====
if (pp_ncp_vec) {
    print("Processing NCP: vectors ====")

    # Process vector NCPs that want the area coverage
    ncp_fn_area <- ncps |>
        filter(type == "vec" & method == "area") |>
        select(var, fn_raw) |>
        deframe()

    for (ncp_name in names(ncp_fn_area)) {
        prepare_ncp_v_area(ncp_name)
    }

    ncp_fn_other <- ncps |>
        filter(type == "vec" & method != "area") |>
        select(var, fn_raw) |>
        deframe()

    # Manually prepare saltmarshes as weird data
    ncp_manual_marshes()
    # Prepare vector NCPs that want attribute values
    prepare_ncp_v_raw("ncp_coastal", 'coastal_potential_cur', 'mean')


}
### Raster processing ====
if (pp_ncp_ras) {

    print("Processing NCP: rasters ====")
    ncp_fn_r <- ncps |>
        filter(type == "ras") |>
        select(var, fn_raw) |>
        deframe()


    ncp_method <- ncps |>
        filter(type == "ras") |>
        select(var, method) |>
        mutate(method = "average") |> # manually set method to 'average' for all to fix errors
        deframe()


    for (ncp in names(ncp_fn_r)) {
        print(paste0("Processing: ", ncp, " ..."))
        ifile <- file.path(dir_in, ncp_fn_r[ncp])
        ofile <- file.path(dir_out, "ncp", fn_template(ncp))
        method <- ncp_method[ncp]
        prepare_ncp_r_gdal(ifile, ofile, method, gdalwarp_path)

    }


}
### Mask NCPs to PU ====
# Mask with planning units so only necessary ones kept

if (pp_ncp_mask) {
    print("Processing NCPs: masking ====")
    ncp_list <- unlist(select(ncps, var), use.names = FALSE)
    mask_ncp_pu(fn_template(paste0("intermediateHFP_", hfp_lower, "_",hfp_upper, "_excludeNotNat")))
    # Or mask just to UN boundaries to check terrestrial (less restrictive)
    #mask_ncp_pu(fn_template("countries"), extra = "_cn")
}


##%##########################################################################%##
# 2. Pre-processing Part II ====
##%##########################################################################%##

if (pp_cells) {
    print("Creating grid cells ====")
    source(file.path(dir_wd, "script_tools/v3/2.1-global_cells.R"))
    create_cells()
}

end <- Sys.time()

# Logs
print("Finished!")
print(end - start)
end - start