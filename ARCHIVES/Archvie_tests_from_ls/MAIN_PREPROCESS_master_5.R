##%##########################################################################%##
#       Script for pre-processing for spatial prioritisation                   #
#           V 31.07.2024    Matthew Harris and Vignesh Kamath                  #
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
# REQUIRED BEFORE STARTING - set OPTIONS:
#   - Define the 'options' in the '0-OPTIONS.R' script
#   - This includes defining what parts of the script need to be ran
#   - and the parameters of the solution (e.g., resolution, extent, projection)
##%##########################################################################%##
# ENSURE CORRECT SETTINGS in section 0.1
#   - Set the working directory in this script
#   - There is an additional option *within this* script for setting a 'runid'
#       in order to distinguish between multiple runs if it is the same RES
##%##########################################################################%##

##%##########################################################################%##
# 0.1 MAKE CHANGES HERE ====
## Set working directory ====
#dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_wd <- "/home/science/p09217_RestorationPotentialLayer/global2024_v2"
## Set run-id ====
runid = ""

##%##########################################################################%##
# 0.2 - SET UP ====
## Load libraries
library(terra)
library(tidyverse)

## Start timing ====
print("Starting")
start <- Sys.time()

## Load options script ====
source(file.path(dir_wd, "script_tools/v3/0-OPTIONS_5.R"))

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
    process_hfp()
    process_landcover()
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

    # prepare_ncp_v_area("ncp_mangroves")

    ncp_fn_other <- ncps |>
        filter(type == "vec" & method != "area") |>
        select(var, fn_raw) |>
        deframe()

    # Manually prepare saltmarshes as weird data
    # ncp_manual_marshes("ncp_saltmarshes")
    # Prepare vector NCPs that want attribute values
    prepare_ncp_v_raw("ncp_coastal", 'coastal_potential_cur', 'mean')


}
### Raster processing ====
if (pp_ncp_ras) {
# Path to gdalwarp on windows; On linux I think it can just be set to "gdalwarp"
    gdalwarp_path <- "gdalwarp"
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
    create_cells(ncp_list)
}

if (pp_split){
    print("Splitting NCPs by country")
    source(file.path(dir_wd, "script_tools/v3/2.2-split_by_country_optional.R"))
}

end <- Sys.time()

# Logs
print("Finished!")
print(end - start)
end - start
