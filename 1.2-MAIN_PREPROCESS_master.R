##%##########################################################################%##
#       Script for pre-processing for spatial prioritization                   #
#           V 29.08.2024    Matthew Harris and Vignesh Kamath                  #
##%##########################################################################%##
# REQUIRED packages:
#       - sf, terra, tidyverse, arrow, data.table, tictoc, prioritzr, glue
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
dir_src <- dir_wd
# dir_wd <- "/home/matthewh@internal.wcmc/projects_active/p09217_RestorationPotentialLayer/global2024_v2"
## Set run-id ====
runid <- ""
## Options for choosing what to pre-process ====
pp_countries <- TRUE
pp_hfp <- TRUE
pp_lulc <- TRUE
pp_restorable <- TRUE
pp_ecoregions <- TRUE
pp_ncp_vec <- TRUE
pp_ncp_ras <- TRUE
pp_ncp_mask <- TRUE
pp_cells <- TRUE

# automatically create needed sub directories
auto_dir <- TRUE

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
library(data.table)
library(arrow)
## Start timing ====
print("Starting")
start <- Sys.time()

## Load dependency scripts ====
# Load options
source(file.path(dir_src, "script_tools/v3/1.1-OPTIONS.R"))
# Load helper functions
#   - gdalwarp_args()      -- create gdalwarp command
#   - prepare_ncp_r_gdal() -- convert raster NCPs using gdal
#   - prepare_ncp_v_area() -- convert vector NCPs to use polygon area
#   - prepare_ncp_v_raw()  -- convert vector NCPs using vector attribute
source(file.path(dir_src, "script_tools/v3/0.0-helper_functions.R"))
##%##########################################################################%##
# 0.3 Automatically defined variables ====
# The following variables are automatic, and use the above information
# NOTE: paths should NOT end with a slash as file.path() will sort this
# Directory set-up:
#   BASE
#     - raw                 | raw input data (i.e., downloaded data)
#     - script_tools        | these scripts
#     - work_in_progress    | output data (both from pre-processing, and final)
dir_in <- file.path(dir_wd, "raw")

if (auto_dir) {
    if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
    walk(
        c(dir_features, dir_pu),
        ~if (!dir.exists(.x)) dir.create(.x, recursive = TRUE)
    )
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
fn_template <- function(name, extra = "", ext = ".tif") {
    return(paste0(name, "_", RES, "km_", PROJ, extra, ext))
}


## Template raster
rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)
sf_use_s2(FALSE) # To avoid intersecting polygon errors. *Workaround*
setwd(dir_wd)
##%##########################################################################%##
# 1. Pre-processing Part I ====
##%##########################################################################%##
#  - Process all raw data to the same

## 1.1 Countries ====
if (pp_countries) {
    print("* Processing Countries *")
    countries <- st_read(file.path(dir_in, pu_fn["countries"]))
    countries_key <- tibble(
        ISO3CD = unique(countries$iso3cd)
    ) |>
        arrange(ISO3CD) %>%
        mutate(
            ISONUM = 1:nrow(.)
        ) |>
        write_csv(file.path(dir_pu, "global_countries_key.csv"))

    # Rasterize countries vector with cell value ISO number
    countries_rast <- countries |>
        st_transform(st_crs(EPSG)) |>
        left_join(countries_key, by = join_by("iso3cd" == "ISO3CD")) |>
        rasterize(rast_template, field = "ISONUM") |>
        writeRaster(file.path(dir_pu, fn_template("countries")),
                    overwrite = TRUE)

    # Land mask for countries
    countries_mask <- countries_rast |>
        classify(cbind(-Inf, Inf, TRUE)) |>
        writeRaster(file.path(dir_pu, fn_template("countries_mask")),
                    overwrite = TRUE)
}

## 1.2 Human footprint ====
if (pp_hfp) {
    print("* Processing Human Footprint *")
    ifile <- file.path(dir_in, pu_fn["hfp"])
    ofile <- file.path(dir_pu, fn_template("hfp"))
    # args_additional <- "-srcnodata 128 -dstnodata -128"
    args <- gdalwarp_args("mode", ifile, ofile, EPSG, RES, EXT) #, args = args_additional)
    system2(gdalwarp_path, args, wait = TRUE)
}

## 1.3 LULC ====
if (pp_lulc) {
    print("* Processing LULC *")
    ifile <- file.path(dir_in, pu_fn["lulc"])
    ofile <- file.path(dir_pu, fn_template("lulc"))
    args <- gdalwarp_args("mode", ifile, ofile, EPSG, RES, EXT)
    system2(gdalwarp_path, args, wait = TRUE)
}


## 1.4 Create restorable land planning units ====
if (pp_restorable) {
    print("* Processing Restorable Land *")
    # Load preprocessed layers from previous step
    hfp <- rast(file.path(dir_pu, fn_template("hfp")))
    lulc <- rast(file.path(dir_pu, fn_template("lulc")))

    # Binary reclassification from HFP
    rcl_intermediate <- tibble(
        from    = c(0,         hfp_lower, hfp_upper),
        to      = c(hfp_lower, hfp_upper, 51),
        becomes = c(0,         1,         0)
    )

    intermediate_hfp <- classify(hfp, rcl_intermediate) |>
        writeRaster(file.path(dir_pu, fn_template("hfp_intermediate")),
                    overwrite = TRUE)

    # Binary reclassification from LULC
    rcl_lulc <- tibble(
        from    = c(0,  39, 89,  199),
        to      = c(39, 89, 199, 200),
        becomes = c(1,  0,  1,   NA)
    )

    lulc_exclude <- lulc |>
        classify(rcl_lulc) |>
        writeRaster(file.path(dir_pu, fn_template("cop_excludeNotNat")),
                    overwrite = TRUE)

    intermediate_hfp_lulc_exclude <- intermediate_hfp * lulc_exclude

    # Make sure output name includes the HFP bounds for easy identification
    out_name <- fn_template(paste0("intermediateHFP_", hfp_lower, "_", hfp_upper, "_excludeNotNat"))
    writeRaster(intermediate_hfp_lulc_exclude,
                file.path(dir_pu, out_name),
                overwrite = TRUE)
}
## 1.5 Process ecoregions ====
if (pp_ecoregions) {
    print("* Processing ecoregions *")
    # Load LULC not-natural layer as 'modified' land map
    land <- rast(file.path(dir_pu, fn_template("cop_excludeNotNat")))
    modified_mask <- land |>
        classify(cbind(0, NA))

    ecoregions <- st_read(file.path(dir_in, pu_fn["ecoregions2017"]))
    ecoregions_rast <- ecoregions |>
        st_transform(st_crs(EPSG)) |>
        rasterize(rast_template, field = "ECO_ID") |>
        mask(land) |>
        writeRaster(file.path(dir_pu, fn_template("ecoregions_withIceRock")),
                    overwrite = TRUE)

    remnant <- ecoregions_rast * modified_mask
    writeRaster(remnant,
                file.path(dir_pu, fn_template("ecoregionsremnant_withIceRock")),
                overwrite = TRUE)

    ### 1.5.1 Calculate number of pixels per ecoregion ====
    ecor_pixels <- freq(ecoregions_rast) %>%
        as_tibble() %>%
        select(-layer) %>%
        rename(potential_extent = count)

    remnant_pixels <- freq(remnant) %>%
        as_tibble() %>%
        select(-layer) %>%
        rename(realised_extent = count)


    ### 1.5.2 Calculate remnant ecoregion proportions ====
    remnant_table <- remnant_pixels |>
        full_join(ecor_pixels) |>
        rename(ECO_ID = value) |>
        mutate(
            remnant_proportion = realised_extent / potential_extent
        ) |>
        left_join(
            as.data.frame(ecoregions) |>
                select(ECO_ID, ECO_NAME, BIOME_NAME)
        ) |>
        select(ECO_NAME, ECO_ID, BIOME_NAME, realised_extent, potential_extent, remnant_proportion)

    print("CSV WRITE...")
    write_csv(remnant_table, file.path(dir_pu, "global_ecoregions_moll.csv"))

    ### 1.5.3 Create without ice and rock ====
    ecor2 <- classify(ecoregions_rast, cbind(0, NA))
    writeRaster(ecor2,
                file.path(dir_pu, fn_template("ecoregions_noIceRock")),
                overwrite = TRUE)

    remnant2 <- classify(remnant, cbind(0, NA))
    writeRaster(remnant2,
                file.path(dir_pu, fn_template("ecoregionsremnant_noIceRock")),
                overwrite = TRUE)

    rm(land, modified_mask, ecoregions, ecoregions_rast, remnant, ecor2, remnant2)
}
## 1.6 Process NCPs ====
source(file.path(dir_src, "script_tools/v3/1.4-p1-ncp.R"))


### 1.6.1 Vector processing ====
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
    marshes <- st_read(file.path(dir_in, ncp_fn_other["ncp_saltmarshes"])) |>
        st_centroid() |>
        st_transform(st_crs(EPSG)) |>
        rasterize(rast_template, field = "areakm2", fun = "sum") |>
        writeRaster(file.path(dir_out, "ncp", fn_template("ncp_saltmarshes")),
                    overwrite = TRUE)
    # Prepare vector NCPs that want attribute values
    prepare_ncp_v_raw("ncp_coastal", "coastal_potential_cur", "mean")


}
### 1.6.2 Raster processing ====
if (pp_ncp_ras) {

    print("Processing NCP: rasters ====")
    ncp_fn_r <- ncps |>
        filter(type == "ras") |>
        select(var, fn_raw) |>
        deframe()


    ncp_method <- ncps |>
        filter(type == "ras") |>
        select(var, method) |>
        # mutate(method = "average") |> # manually set method to 'average' for all to fix errors
        deframe()


    for (ncp in names(ncp_fn_r)) {
        print(paste0("Processing: ", ncp, " ..."))
        ifile <- file.path(dir_in, ncp_fn_r[ncp])
        ofile <- file.path(dir_features, fn_template(ncp))
        method <- ncp_method[ncp]
        prepare_ncp_r_gdal(ifile, ofile, method, gdalwarp_path)

    }


}
### 1.6.3 Mask NCPs to PU ====
# Mask with planning units so only necessary ones kept

if (pp_ncp_mask) {
    print("Processing NCPs: masking ====")
    # ncp_list <- unlist(select(ncps, var), use.names = FALSE)
    pu_mask <- rast(file.path(dir_pu, fn_template(paste0("intermediateHFP_", hfp_lower, "_", hfp_upper, "_excludeNotNat"))))


    ncp_list <- list.files(dir_features,
                           recursive = TRUE,
                           full.names = TRUE,
                           pattern = glue::glue("*_{RES}km_{PROJ}.tif$"))

    file_names <- ncp_list |>
        basename() |>
        str_remove_all(".tif")

    ncp_list |>
        map(~rast(.x)) |>
        map(~mask(.x, pu_mask, maskvalue = c(0, NA))) |>
        walk2(
            .y = file_names,
            ~writeRaster(.x, file.path(dir_features, paste0(.y, "_mask", extra, ".tif")), overwrite = TRUE)
        )

}


# 1.7 Pre-processing Part II ====

if (pp_cells) {
    print("* Creating grid cells *")
    pu_rast <- c("countries", "lulc") |>
        map(~file.path(dir_pu, fn_template(.x))) |>
        map(~rast(.x)) |>
        rast()

    hfp_int <- rast(file.path(dir_pu, str_glue("intermediateHFP_{hfp_lower}_{hfp_upper}_excludeNotNat_{RES}km_{PROJ}.tif")))
    ecoregions <- rast(file.path(dir_pu, fn_template("ecoregions_withIceRock")))

    ncp_present <- list.files(dir_features,
                              pattern = "_mask.tif$",
                              full.names = TRUE)
    ncp_names <- ncp_present |>
        basename() |>
        str_remove_all(fixed(".tif")) |>
        str_extract("(ncp_[A-Za-z0-9]+)")
    ncp_rast <- ncp_present |>
        map(~rast(.x)) |>
        rast()

    all_rast <- c(pu_rast, hfp_int, ecoregions, ncp_rast)
    names(all_rast) <- c("ISONUM", "lulc", "hfp", "ecoregions", ncp_names)
    pu_vals <- as.data.frame(all_rast,
                             xy = TRUE,
                             na.rm = FALSE) |>
        setDT()

    pu_vals <- pu_vals[!is.na(ISONUM), ][hfp == 1, ]


    pu_vals <- pu_vals[, id := 1:.N]
    setcolorder(pu_vals, "id", before = 1)

    write_dataset(pu_vals,
                  file.path(dir_proc, "global_cells"),
                  partitioning = c("ISONUM"))


}

end <- Sys.time()

# Logs
print("Finished!")
print(end - start)
end - start
