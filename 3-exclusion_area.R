# Create raster of which inputs caused exclusion
# 0. Load libraries ====
print("Loading libraries")
library(data.table)
library(arrow)
library(tidyverse)
library(scales)
library(terra)

# 1. OPTIONS and set-up ====
## 1.1 EDITABLE options ====
dir_wd <- "/mnt/sda/MH_restoration"
# dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_src <- dir_wd

### Solution-related options ====
auto_dir <- TRUE      # Automatically create needed directories?
runid <- ""           # Additional ID to distinguish runs

## 1.2 Shared options ====
# Load options file to share options with pre-processing
source(file.path(dir_src, "script_tools/1.1-OPTIONS.R"))

## 1.4 Directory-related variables ====
dir_analyze <- file.path(dir_out, "analysis", "compare")


# Helper function for file names
fn_template <- function(name, extra = "", ext = ".tif") {
    return(paste0(name, "_", RES, "km_", PROJ, extra, ext))
}

# MAIN ====
hfp <- rast(file.path(dir_pu, fn_template("hfp_mask")))
conv <- rast(file.path(dir_pu, fn_template("lulc_converted")))
other <- rast(file.path(dir_pu, fn_template("lulc_other")))
builtCrops <- rast(file.path(dir_pu, fn_template("lulc_converted_noPlant")))

# Calculate which pixels are excluded when planted trees are added
plant <- (1 - conv) - (1 - builtCrops)

# Create combined layer
comb <- (1 - hfp) * 100 + (1-conv) * 10 + (1-other)

# Reclassify exclusion reasons
comb_class <- comb |>
    classify(
        tribble(
            ~is,   ~becomes,
            0,     0,        # Included
            1,     1,         # Exc: other only
            10,    2,         # Exc: converted only
            11,    3,         # Exc: converted and other (prob palm)
            100,   4,         # Exc: HFP only
            101,   5,         # Exc: HFP and other
            110,   6          # Exc: HFP and converted (mismatch between inputs)
        ))

# Add class for planted trees retrospectively
comb_plant <- ifel( (comb_class == 2 | comb_class == 3 | comb_class == 6) & plant == 1, 7, comb_class)

# Write outputs
writeRaster(comb_class, file.path(dir_analyze, "exclusion_reason.tif"), overwrite = TRUE)
writeRaster(comb_plant, file.path(dir_analyze, "exclusion_reasonPlant.tif"), overwrite = TRUE)
