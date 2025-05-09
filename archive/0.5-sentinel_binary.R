# Reclassify each sentinel 10-m LULC layer into binary raster:
# Part I: 'converted land' binary raster
#   where:
#       1 = urban or cropland
#       0 = All other land
#       NA = NA
# Part II: bare ground binary raster
#   where:
#       1 = bare ground
#       0 = all other land
# Done using R as quicker than GDAL
library(terra)
library(stringr)

setwd("/mnt/sda/restoration_opportunities/raw/sentinel/lulc2020_orig")
dir_wd <-"/mnt/sda/restoration_opportunities/raw/sentinel"
dir_in <-  file.path(dir_wd, "lulc2020_orig")

files <- list.files(dir_in, full.names = TRUE, pattern = "*.tif$")

# Part I: Converted Land binary raster ====
dir_out <- file.path(dir_wd, "convertedLand2020")
if(!dir.exists(dir_out)) dir.create(dir_out)

for (file in files) {
    print(paste0("Processing: ", basename(file)))
    new_name <- file.path(dir_out, paste0(str_remove_all(basename(file), pattern = ".tif"), "_convertedLand.tif"))
    s<- Sys.time()
    r <- file |> 
        rast() |> 
        classify(
            data.frame(
                is      = c(0, 1,2,4,5,7,8,9,10,11),
                becomes = c(NA,0,0,0,1,1,0,0,0, 0)
            )
        ) |> 
        writeRaster(new_name, gdal=c("COMPRESS=DEFLATE"), overwrite = TRUE)
    e <- Sys.time()
    e-s
    rm(s)
    gc()
}


# Part II: Snow binary raster ====

dir_out <- file.path(dir_wd, "snow2020")
if(!dir.exists(dir_out)) dir.create(dir_out)


for (file in files) {
    print(paste0("Processing: ", basename(file)))
    new_name <- file.path(dir_out, paste0(str_remove_all(basename(file), pattern = ".tif"), "_snow.tif"))
    s<- Sys.time()
    r <- file |> 
        rast() |> 
        classify(
            data.frame(
                is      = c(0,  1,2,4,5,7,8,9,10,11),
                becomes = c(NA, 0,0,0,0,0,1,0,0,0)
            )
        ) |> 
        writeRaster(new_name,
                    gdal=c("COMPRESS=DEFLATE"),
                    overwrite = TRUE,
                    NAflag = -128)
    e <- Sys.time()
    print(e-s)
    rm(s)
    gc()
}
