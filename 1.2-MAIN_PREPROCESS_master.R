##%##########################################################################%##
#       Script for pre-processing for spatial prioritization                   #
#                    Matthew Harris and Vignesh Kamath                         #
##%##########################################################################%##
# REQUIRED packages:
#       - sf, terra, tidyverse, arrow, data.table, tictoc, prioritzr, glue
##%##########################################################################%##
# REQUIRED BEFORE STARTING - set OPTIONS (external script):
#   - Define the 'options' in the '1.1-OPTIONS.R' script
#   - This is the parameters of the solution (e.g., resolution, extent,
#       projection) that are shared amongst 1.2, 1.3 and 2
##%##########################################################################%##
# ENSURE CORRECT SETTINGS in section 0.1
#   - Set the working directory in this script
#   - This defines what parts of the script need to be ran
#   - Set a 'runid' in order to distinguish between multiple runs
#       if it is the same RES
#   - Set gdalwarp_path and gdalcalc_path
##%##########################################################################%##

# Sys.setenv(PATH="/home/science/miniforge3/condabin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin")
# library(reticulate)
# use_condaenv("matthew")


##%##########################################################################%##
# 0.1 MAKE CHANGES HERE ====
## Set working directory ====
dir_wd <- "/mnt/sda/MH_restoration"
# dir_wd <- "/home/matthewh@internal.wcmc/projects_active/p09217_RestorationPotentialLayer/global2025"
# dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_src <- dir_wd
dir_src <- "C:/Users/matthewh/LOCAL/projects_local/restoration_scripts_git"
## Set run-id ====
runid <- ""
## Options for choosing what to pre-process ====
pp_countries <- TRUE
pp_hfp <- TRUE
pp_lulc <- TRUE
pp_restorable <- TRUE
pp_ecoregions <- TRUE
pp_ft_vec <- TRUE
pp_ft_ras <- TRUE
pp_ft_mask <- TRUE
pp_cells <- TRUE



# automatically create needed sub directories
auto_dir <- TRUE

# Path to gdalwarp
# If gdal is on the user or system path, just set to "gdalwarp"
# Else, set to the full path:
#   On windows, probably: C:/OSGeo4W/bin/gdalwarp.exe
#   On linux, probably already on path :)
gdalwarp_path <- "gdalwarp"
# Probably "gdal_calc" if system set correctly
# gdalcalc_path <- " /home/science/miniforge3/envs/matthew/bin/gdal_calc.py"
gdalcalc_path <- "gdal_calc"
# gdalcalc_path <- "gdal_calc.py"
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
# source(file.path(dir_src, "script_tools/1.1-OPTIONS.R"))
source(file.path(dir_src, "1.1-OPTIONS.R"))
# Load helper functions
#   - gdalwarp_args()      -- create gdalwarp command
#   - prepare_ft_r_gdal() -- convert raster features using gdal
#   - prepare_ft_v_area() -- convert vector features to use polygon area
#   - prepare_ft_v_raw()  -- convert vector features using vector attribute
# source(file.path(dir_src, "script_tools/0.9-helper_functions.R"))
source(file.path(dir_src, "0.9-helper_functions.R"))
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
        c(dir_features, dir_pu, dir_proc, dir_inter),
        ~if (!dir.exists(.x)) dir.create(.x, recursive = TRUE)
    )
}

##%##########################################################################%##
# 0.4 Load information ====
variables <- read_csv(file.path(dir_in, "preprocess_info.csv"))

pu_fn <- variables |>
    select(var, fn_raw) |>
    filter(!grepl("ft", var)) |>
    deframe()

features <- variables |>
    filter(grepl("ft", var)) |>
    filter(!is.na(fn_raw))


## Helper functions ====
# Helper function to make sure all output files named consistently
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

    # Reclassify with gdal_calc.py
    ifile <- file.path(dir_in, pu_fn["hfp"])
    ofile <- file.path(dir_inter, fn_template("hfp_binary"))

    system(glue(
        gdalcalc_path,
        ' -A "{ifile}"',
        ' --calc="(A<={hfp_lower})*(A>={hfp_upper})*0+(A>{hfp_lower})*(A<{hfp_upper})*1"',
        ' --co compress=lzw --overwrite --outfile "{ofile}"'
    ))

    # Reduce resolution with gdalwarp
    ifile <- file.path(dir_inter, fn_template("hfp_binary"))
    ofile <- file.path(dir_pu, fn_template("hfp_mask"))
    system2(
        gdalwarp_path,
        gdalwarp_args("mode", ifile, ofile, EPSG, RES, EXT),
        wait = TRUE
    )
}

## 1.3 Land Use Exclusion ====
### 1.3.1 Land Use and Land Cover Processing
if (pp_lulc) {
    print("* Processing LULC *")
    fns_lulc <- variables |>
        select(var, fn_raw) |>
        filter(grepl("lulc", var)) |>
        filter(var != "lulc_discrete") |>
        deframe()

    # Convert fractional cover to correct resolution
    for (fn_lulc in names(fns_lulc)) {
        ifile <- file.path(dir_in, pu_fn[fn_lulc])
        ofile <- file.path(dir_inter, fn_template(fn_lulc))
        args <- gdalwarp_args("average", ifile, ofile, EPSG, RES, EXT, args = "-wm 2G -co GDAL_CACHEMAX=8000")
        print(args)
        system2(gdalwarp_path, args, wait = TRUE)
    }

    # Create binary 'other excluded land' (non-converted) raster
    pwater <- rast(file.path(dir_inter, fn_template("lulc_pwater")))
    swater <- rast(file.path(dir_inter, fn_template("lulc_swater")))
    moss <- rast(file.path(dir_inter, fn_template("lulc_moss")))
    snow <- rast(file.path(dir_inter, fn_template("lulc_snow")))
    bare <- rast(file.path(dir_inter, fn_template("lulc_bare")))

    # [Note to self: Faster through R than gdal_calc here]
    lulc_other <- (pwater + swater + moss + snow + bare) |>
        classify(data.frame(
            from    = c(0,  50),
            to      = c(50, Inf),
            becomes = c(0,  1)
        ),
        right = FALSE # so >= 50
        )
    writeRaster(lulc_other, file.path(dir_pu, fn_template("lulc_other")), overwrite = TRUE)

}

### 1.3.2 Planted trees (Spatial Database on Planted Trees: SDPT)
# NOTE: loads rasterized planted trees already exported through GEE
# This ensures that it is the same extent and resolution as all the rest
#   Nearest-neighbour because it is already 5km, and it is just alignment needed
#   Need to multiply by 100 as this is 0-1 but copernicus is 0-100
plant <- (rast(file.path(dir_in, pu_fn["plant_sdpt"])) * 100) |> 
    classify(cbind(0, NA)) |> 
    project(rast_template) |> 
    writeRaster(file.path(dir_pu, fn_template("plant_sdpt")), overwrite = TRUE)

### 1.3.3 Oil Palm Plantations
# NOTE: relies on pre-processing in 0.2 and 0.3
# This ensures that it is the same extent and resolution as all the rest
#   Nearest-neighbour because it is already 5km, and it is just alignment needed
#   Need to multiply by 100 as this is 0-1 but copernicus is 0-100
ifile <- file.path(dir_in, pu_fn["plant_palm"])
ofile <- file.path(dir_pu, fn_template("plant_palm"))
args <- gdalwarp_args("near", ifile, ofile, EPSG, RES, EXT)
system2(gdalwarp_path, args, wait = TRUE)


palm <- (rast(file.path(dir_in, pu_fn["plant_palm"])) * 100) |> 
    classify(cbind(-Inf, 0, 0)) |> # Remove weird negatives
    project(rast_template) |> 
    classify(cbind(NA, 0)) |> # necessary to make the sum below work
    writeRaster(file.path(dir_pu, fn_template("plant_palm")), overwrite = TRUE)

### 1.3.4 Converted land raster
# Converted = built + crop + palm + plantations
built <- rast(file.path(dir_inter, fn_template("lulc_built")))
crops <- rast(file.path(dir_inter, fn_template("lulc_crop")))
palm <- rast(file.path(dir_pu, fn_template("plant_palm")))
plant <- rast(file.path(dir_pu, fn_template("plant_sdpt")))

# [Note to self: Faster through R than gdal_calc here]
converted <- (built + crops + palm + plant) |>
    classify(data.frame(
        from    = c(0,  50),
        to      = c(50, Inf), # Inf to catch the weird >100
        becomes = c(0,  1)
    ),
    right = FALSE # so >= 50
    )

writeRaster(converted, file.path(dir_pu, fn_template("lulc_converted")), overwrite = TRUE)

# TESTS
# built_crop <- (built + crops) |>
#     classify(data.frame(
#         from    = c(0,  50),
#         to      = c(50, Inf), # Inf to catch the weird >100
#         becomes = c(0,  1)
#     ),
#     right = FALSE # so >= 50
#     )
# 
# writeRaster(built_crop, file.path(dir_pu, fn_template("lulc_converted_noPlant")), overwrite = TRUE)
# END TEST

## 1.4 Create restorable land planning units ====
if (pp_restorable) {
    print("* Processing Restorable Land *")
    # Load preprocessed layers from previous step
    lulc <- rast(file.path(dir_pu, fn_template("lulc")))


}


# Land Use and Land Cover Exclusion
## Create converted exlcusion layer
# Exclude crops (40) and built-up (50)
# NOTE: Done separately due to computing issues

# lulc_converted


## Create other exclusion layer
# Exclude Bare ground (60), water (80, 200), snow/ice (70), moss and lichen (100)

# lulc_other


# lulc_exclude = lulc_converted + lulc_other

intermediate_hfp <- rast(file.path(dir_pu, fn_template("hfp_mask")))

restorable_land <- intermediate_hfp * lulc_exclude * sdpt_binary

# TODO: Make output name include the HFP bounds for easy identification
#   this used to work in old code, but made more general here
writeRaster(restorable_land,
            file.path(dir_pu, fn_template("restorable_land")),
            overwrite = TRUE)

## 1.5 Process ecoregions ====
if (pp_ecoregions) {
    print("* Processing ecoregions *")
    # Load LULC not-natural layer as 'modified' land map
    land <- rast(file.path(dir_pu, fn_template("lulc_converted")))
    modified_mask <- land |>
        classify(cbind(0, NA))

    ecoregions <- st_read(file.path(dir_in, pu_fn["ecoregions2017"]))
    ecoregions_rast <- ecoregions |>
        st_transform(st_crs(EPSG)) |>
        rasterize(rast_template, field = "ECO_ID") |>
        mask(land) |>
        writeRaster(file.path(dir_pu, fn_template("ecoregions")),
                    overwrite = TRUE)

    remnant <- ecoregions_rast * modified_mask
    writeRaster(remnant,
                file.path(dir_pu, fn_template("ecoregionsremnant")),
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

    rm(land, modified_mask, ecoregions, ecoregions_rast, remnant)
}

## 1.6 Process non-ecoregion Features ====
### 1.6.1 Vector processing ====
if (pp_ft_vec) {
    print("Processing features: vectors ====")

    # Process vector features that want the area coverage
    ft_fn_area <- features |>
        filter(type == "vec" & method == "area") |>
        select(var, fn_raw) |>
        deframe()

    for (ft_name in names(ft_fn_area)) {
        prepare_ft_v_area(ft_name)
    }

    ft_fn_other <- features |>
        filter(type == "vec" & method != "area") |>
        select(var, fn_raw) |>
        deframe()

    # Manually prepare saltmarshes as weird data
    marshes <- st_read(file.path(dir_in, ft_fn_other["ft_saltmarshes"])) |>
        st_centroid() |>
        st_transform(st_crs(EPSG)) |>
        rasterize(rast_template, field = "areakm2", fun = "sum") |>
        writeRaster(file.path(dir_out, "features", fn_template("ft_saltmarshes")),
                    overwrite = TRUE)
    # Prepare vector features that want attribute values
    prepare_ft_v_raw("ft_coastal", "coastal_potential_cur", "mean")


}
### 1.6.2 Raster processing ====
if (pp_ft_ras) {

    print("Processing features: rasters ====")
    ft_fn_r <- features |>
        filter(type == "ras") |>
        select(var, fn_raw) |>
        deframe()


    ft_method <- features |>
        filter(type == "ras") |>
        select(var, method) |>
        # mutate(method = "average") |> # manually set method to 'average' for all to fix errors
        deframe()


    for (ft in names(ft_fn_r)) {
        print(paste0("Processing: ", ft, " ..."))
        ifile <- file.path(dir_in, ft_fn_r[ft])
        ofile <- file.path(dir_features, fn_template(ft))
        method <- ft_method[ft]
        prepare_ft_r_gdal(ifile, ofile, method, gdalwarp_path)

    }


}
### 1.6.3 Mask features to PU ====
# Mask with planning units so only necessary ones kept

if (pp_ft_mask) {
    print("Processing features: masking ====")
    pu_mask <- rast(file.path(dir_pu, fn_template(paste0("intermediateHFP_", hfp_lower, "_", hfp_upper, "_excludeNotNat"))))


    ft_list <- list.files(dir_features,
                          recursive = TRUE,
                          full.names = TRUE,
                          pattern = glue::glue("*_{RES}km_{PROJ}.tif$"))

    file_names <- ft_list |>
        basename() |>
        str_remove_all(".tif")

    ft_list |>
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

    ft_present <- list.files(dir_features,
                             pattern = "_mask.tif$",
                             full.names = TRUE)
    ft_names <- ft_present |>
        basename() |>
        str_remove_all(fixed(".tif")) |>
        str_extract("(ft_[A-Za-z0-9]+)")
    ft_rast <- ft_present |>
        map(~rast(.x)) |>
        rast()

    all_rast <- c(pu_rast, hfp_int, ecoregions, ft_rast)
    names(all_rast) <- c("ISONUM", "lulc", "hfp", "ecoregions", ft_names)
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
