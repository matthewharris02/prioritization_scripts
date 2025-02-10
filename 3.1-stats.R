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
dir_output <- file.path(dir_out, "output", ifelse(runid == "", "default", runid))
dir_logs <- file.path(dir_out, "logs", ifelse(runid == "", "default",runid))
dir_analyze <- file.path(dir_out, "analysis", ifelse(runid == "", "default", runid))

if (auto_dir) {
    c(dir_output, dir_logs) |>
        walk(\(x) if (!dir.exists(x)) { dir.create(x, recursive = TRUE)})
}

## 1.5 Template raster ====
rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)

## 1.6 Filename template helper function ====
fn_template <- function(name, extra = "", ext = ".tif") {
    return(paste0(name, "_", RES, "km_", PROJ, extra, ext))
}

# 2. Restorable Land Stats ====
lulc_other <- rast(file.path(dir_pu, fn_template("lulc_other"))) # 0 = restorable
lulc_converted <- rast(file.path(dir_pu, fn_template("lulc_converted"))) # 0 = restorable
hfp_intermediate <- rast(file.path(dir_pu, fn_template("hfp_mask"))) # 1 = restorable
restorable <- rast(file.path(dir_pu, fn_template("restorable_land"))) # 0 = restorable

hfp_area <- global(hfp_intermediate, "sum", na.rm = TRUE)

restorable_area <- (restorable + 1)  |>
    classify(cbind(2, 0)) |>
    global("sum", na.rm = TRUE)
