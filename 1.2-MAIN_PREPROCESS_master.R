##%##########################################################################%##
#       Script for pre-processing for spatial prioritization                   #
##%##########################################################################%##
# REQUIRED packages:
#       - sf, terra, tidyverse, arrow, data.table, prioritzr, glue
##%##########################################################################%##
# REQUIRED BEFORE STARTING - set OPTIONS (external script):
#   - Define the 'options' in the '1.1-OPTIONS.R' script
#   - This is the parameters of the solution (e.g. extent,
#       projection) that are shared amongst 1.2, 1.3 and 2
##%##########################################################################%##
# ENSURE CORRECT SETTINGS in section 0.1
#   - Set the working directory
#   - Set the resolution
#   - If wanted, change pp_* vars to select which parts of the script to run
#   - Set a 'runid' in order to distinguish between multiple runs
#       if it is the same RES
#   - Set 'dir_id' to distinguish between different versions with same RES
#       and/or different inputs
#   - Set gdalwarp_path and gdalcalc_path
##%##########################################################################%##

##%##########################################################################%##
# 0.1 MAKE CHANGES HERE ====
## Set working directory ====
dir_wd <- "/mnt/sda/restoration_opportunities"
# dir_wd <- "/home/matthewh@internal.wcmc/projects_active/p09217_RestorationPotentialLayer/global2025"
# dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_src <- dir_wd
# dir_src <- "C:/Users/matthewh/LOCAL/projects_local/restoration_scripts_git"
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

## RESOLUTION ====
# Set shared resolution for all layers
# Relative to 1km at equator (or 30 arcseconds in non-equal area projection)
RES <- 5
## Directory ID ====
#   for different solutions at the same resolution
dir_id <- ""

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
# gdalcalc_path <- "gdal_calc"
gdalcalc_path <- "gdal_calc.py"
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
source(file.path(dir_src, "script_tools/1.1-OPTIONS.R"))
# source(file.path(dir_src, "1.1-OPTIONS.R"))
# Load helper functions
#   - gdalwarp_args()      -- create gdalwarp command
#   - prepare_ft_r_gdal() -- convert raster features using gdal
#   - prepare_ft_v_area() -- convert vector features to use polygon area
#   - prepare_ft_v_raw()  -- convert vector features using vector attribute
source(file.path(dir_src, "script_tools/0.9-helper_functions.R"))
# source(file.path(dir_src, "0.9-helper_functions.R"))
##%##########################################################################%##
# 0.3 Automatically defined variables ====
# The following variables are automatic, and use the above information
# NOTE: paths should NOT end with a slash as file.path() will sort this
# Directory set-up:
#   BASE
#     - raw                 | raw input data (i.e., downloaded data)
#     - script_tools        | these scripts
#     - work_in_progress    | output data (both from pre-processing, and final)
dir_out <- file.path(dir_wd, "work_in_progress",
                     paste0(RES, "km",
                         ifelse(dir_id == "", "", paste0("_", dir_id))
                     ))

dir_in <- file.path(dir_wd, "raw")

dirs <- create_info(dir_out)

if (auto_dir) {
    if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
    walk(
        c(dirs),
        ~if (!dir.exists(.x)) dir.create(.x, recursive = TRUE)
    )
}

##%##########################################################################%##
# 0.4 Load information ====
variables <- read_csv(file.path(dirs["dir_out"], "preprocess_info.csv"))

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
        write_csv(file.path(dirs["dir_pu"], "global_countries_key.csv"))

    # Rasterize countries vector with cell value ISO number
    countries_rast <- countries |>
        st_transform(st_crs(EPSG)) |>
        left_join(countries_key, by = join_by("iso3cd" == "ISO3CD")) |>
        rasterize(rast_template, field = "ISONUM") |>
        writeRaster(file.path(dirs["dir_pu"], fn_template("countries")),
                    overwrite = TRUE)

    # Land mask for countries
    countries_mask <- countries_rast |>
        classify(cbind(-Inf, Inf, TRUE)) |>
        writeRaster(file.path(dirs["dir_pu"], fn_template("countries_mask")),
                    overwrite = TRUE)
}

## 1.2 Human footprint ====
if (pp_hfp) {
    print("* Processing Human Footprint *")

    ifile <- file.path(dir_in, pu_fn["hfp"])
    ofile <- file.path(dirs["dir_inter"], fn_template("hfp_mask", ext = ".vrt"))
    system2(
        gdalwarp_path,
        gdalwarp_args("mode", ifile, ofile, EPSG, RES, EXT, compress = FALSE),
        wait = TRUE
    )

    ifile <- ofile
    ofile <- file.path(dirs["dir_pu"], fn_template("hfp_mask"))
    system(glue("gdal_translate {ifile} {ofile} -co compress=lzw -co BIGTIFF=YES -co TILED=YES"))
}

## 1.3 Land Use Exclusion ====
if (pp_lulc) {
    print("* Processing LULC *")
    # Converted land: cropland + built-up + planted forests
    #   Pre-processed via Google Earth Engine and exported
    #   GEE tiles converted to VRT and GTiff in script 0.5
    ifile <- file.path(dir_in, pu_fn["lulc_converted"])
    ofile <- file.path(dirs["dir_inter"], fn_template("lulc_converted"))
    system2(
        gdalwarp_path,
        gdalwarp_args("average", ifile, ofile, EPSG, RES, EXT),
        wait = TRUE
    )
    converted_frac <- (rast(ofile) / 10e8) |>
        classify(
            data.frame(
                from = c(0,   0.5),
                to   = c(0.5, 1),
                becomes = c(0, 1)
            ),
            right = FALSE # so >= 50
        )
    writeRaster(converted_frac, file.path(dirs["dir_pu"], fn_template("lulc_converted")), overwrite = TRUE)
}

## 1.4 Create restorable land planning units ====
if (pp_restorable) {
    print("* Processing Restorable Land *")
    lulc_converted <- rast(file.path(dirs["dir_pu"], fn_template("lulc_converted")))
    hfp_intermediate <- rast(file.path(dirs["dir_pu"], fn_template("hfp_mask")))
    # Include/restorable = 1, exclude = NA
    restorable <- hfp_intermediate |>
        mask(lulc_converted, maskvalue = c(0), updatevalue = NA) |>
        classify(cbind(0, NA)) |>
        writeRaster(file.path(dirs["dir_pu"], fn_template("restorable_land")), overwrite = TRUE)

    # TODO: Make output name include the HFP bounds for easy identification
    #   this used to work in old code, but made more general here
}

## 1.5 Process ecoregions ====
if (pp_ecoregions) {
    print("* Processing ecoregions *")
    # Load LULC not-natural layer as 'modified' land map
    converted <- rast(file.path(dirs["dir_pu"], fn_template("lulc_converted")))

    ecoregions <- st_read(file.path(dir_in, pu_fn["ecoregions2017"]))
    ecoregions_rast <- ecoregions |>
        st_transform(st_crs(EPSG)) |>
        rasterize(rast_template, field = "ECO_ID") |>
        mask(converted) |>
        writeRaster(file.path(dirs["dir_pu"], fn_template("ecoregions")),
                    overwrite = TRUE)

    remnant <- mask(ecoregions_rast, converted, maskvalue = 0, updatevalue = NA)
    writeRaster(remnant,
                file.path(dirs["dir_pu"], fn_template("ecoregionsremnant")),
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
    write_csv(remnant_table, file.path(dirs["dir_pu"], "global_ecoregions_moll.csv"))

    rm(converted, ecoregions, ecoregions_rast, remnant)
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
        writeRaster(file.path(dirs["dir_ft"], fn_template("ft_saltmarshes")),
                    overwrite = TRUE)
    # Prepare vector features that want attribute values
    prepare_ft_v_raw("ft_coastal", "coastal_deficit_cur", "mean")
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
        ofile <- file.path(dirs["dir_ft"], fn_template(ft))
        method <- ft_method[ft]
        prepare_ft_r_gdal(ifile, ofile, method, gdalwarp_path)
    }
}
### 1.6.3 Mask features to PU ====
# Mask with planning units so only necessary ones kept

if (pp_ft_mask) {
    print("Processing features: masking ====")
    pu_mask <- rast(file.path(dirs["dir_pu"], fn_template("restorable_land")))

    ft_in <- features |>
        select(var) |>
        lapply(fn_template) |>
        lapply(\(x) file.path(dirs["dir_ft"], x))

    ft_out <- features |>
        select(var) |>
        lapply(\(x) fn_template(x, extra = "_mask")) |>
        lapply(\(x) file.path(dirs["dir_ft"], x))

    ft_in |>
        map(~rast(.x)) |>
        map(~mask(.x, pu_mask, maskvalue = c(0, NA))) |>
        walk2(
            .y = ft_out,
            ~writeRaster(.x, .y, overwrite = TRUE)
        )
}

# 1.7 Pre-processing Part II ====

if (pp_cells) {
    print("* Creating grid cells *")

    # Prepare feature names and file paths
    ft_names <- features |>
        select(var) |>
        deframe()

    ft_fns <- ft_names |>
        sapply(\(x) fn_template(x, extra = "_mask")) |>
        sapply(\(x) file.path(dirs["dir_ft"], x))

    # Prepare names and file paths for other variables
    other_names <- c("countries", "lulc_converted", "restorable_land", "ecoregions")

    other_fns <- other_names |>
        sapply(fn_template) |>
        sapply(\(x) file.path(dirs["dir_pu"], x))

    # Create combined lists for names and file paths
    all_names <- c(other_names, ft_names)
    all_fns <- c(other_fns, ft_fns)

    # Helper function to extract large raster with MakeTiles
    large_extract <- function(rast, name, ntiles = 64) {
        pritn(glue("Initializing large_extract() for {name} with {ntiles} tiles"))
        # Set up directories
        vals_dir <- file.path(dirs["dir_inter"], "temp_vals", name)
        temp_fn <- file.path(dirs["dir_inter"], "temp_tiles", name, glue("temp_{name}_.tif"))
        if (!dir.exists(dirname(temp_fn))) dir.create(dirname(temp_fn), recursive = TRUE)
        if (!dir.exists(vals_dir)) dir.create(vals_dir, recursive = TRUE)
        # Delete old files if exist
        if (dir.exists(dirname(temp_fn))) {
            existing <- list.files(dirname(temp_fn), full.names = TRUE)
            lapply(existing, file.remove)
        }

        # Ensure ntiles is square number by taking the closest, lower square number
        ntiles <- floor(ntiles ^ (1/2))^2

        # Create template for MakeTiles
        rast_tiles <- rast(
            crs = crs(EPSG),
            nrows = ntiles^(1/2),
            ncols = ntiles^(1/2),
            ext = ext(EXT)
        )

        print(glue("Making {ntiles} tiles for {name}..."))
        tiles <- rast |>
            makeTiles(rast_tiles, filename = temp_fn, overwrite = TRUE)
        print("Tiles made")
        for (i in seq(1, ntiles)) {
            print(glue("Extracting for tile #{i}..."))
            tile <- rast(tiles[i]) |>
                as.data.frame(xy = TRUE, na.rm = NA) |>
                write_parquet(file.path(vals_dir, glue("vals_{name}_{i}.parquet")))
            rm(tile)
        }
        print("All tiles extracted; now loading values")
        # Loading vals
        vals_fns <- lapply(1:ntiles, \(x) file.path(vals_dir, glue("vals_{name}_{x}.parquet")))
        vals <- rbindlist(lapply(vals_fns, read_parquet)) |>
            write_parquet(file.path(dirs["dir_proc"], glue("{name}.parquet")))
        print("Extraction complete!")
    }

    # Create multi-band raster for all variables
    all_rast <- all_fns |>
        sapply(rast) |>
        rast()

    # Set correct names for ISONUM and pu
    all_names[1] <- "ISONUM"
    all_names[3] <- "pu"
    names(all_rast) <- all_names

    # Extract values
    large_extract(all_rast, "all")
    pu_vals <- read_parquet(file.path(dirs["dir_proc"], glue("all.parquet")))

    pu_vals <- pu_vals[!is.na(ISONUM),  # Ensure within UN boundary 
                        ][!is.na(pu),   # Filter our non 'restorable land'
                        ][, id := 1:.N] # Give unique id to each pu

    setcolorder(pu_vals, "id", before = 1)

    write_dataset(pu_vals,
                  file.path(dirs["dir_proc"], "global_cells"),
                  partitioning = c("ISONUM"))
    print("Grid cell creation complete")
}

end <- Sys.time()

# Logs
print("Finished!")
print(end - start)
end - start
