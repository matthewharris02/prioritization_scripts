# Pre-process the oil palm dataset
# Relies on the boundaries_withOP.shp, which was created in QGIS by:
#   - Clipping the UN Boundaries to the grid_withOP.shp
#   - Joining the grid attributes to the UN Boundaries (to keep IDs)
#   - Exporting shape file
library(sf)
library(glue)
library(terra)
dir_wd <- "/mnt/sda/restoration_opportunities"
dir_in <- file.path(dir_wd, "raw")

source(file.path(dir_wd, "script_tools/1.1-OPTIONS.R"))

grid <- st_read(file.path(dir_in, "oil_palm/oil_palm_grid/grid_withOP.shp"))

tile_ids <- grid |>
    select(ID) |>
    st_drop_geometry() |>
    unlist(use.name = FALSE)

boundaries <- st_read(file.path(dir_in, "oil_palm/boundaries_withOP/boundaries_withOP.shp"))

for (tile_id in tile_ids) {
    print(glue("Processing tile #{tile_id}"))
    file_oilpalm <- file.path(dir_in, glue("oil_palm/oil_palm_orig/L2_2019b_{tile_id}.tif"))
    rast_oilpalm <- rast(file_oilpalm)
    tile_ext <- ext(rast_oilpalm)
    tile_res <- res(rast_oilpalm)

    rast_template <- rast(
        crs = crs("EPSG:4326"),
        res = tile_res,
        ext = tile_ext
    )
    # Create land mask from boundary
    print("Producing land mask")
    file_land <- file.path(dir_in, glue("oil_palm/boundaries_grid/land_{tile_id}.tif"))
    boundaries |>
        filter(ID == tile_id) |>
        rasterize(rast_template, field = 1, background = 0) |>
        writeRaster(file_land, overwrite = TRUE)


    fn_out <- file.path(dir_in, glue("oil_palm/oil_palm_binary_mask/oil_palm_{tile_id}.tif"))
    # Create binary tile and mask
    print("Creating binary masked tile")
    system2(
        gdalcalc_path,
        glue("-A {file_oilpalm} -B {file_land}",
            " --calc='(A<3)*(B==1)*1+(A==3)*(B==1)*0+(B==0)*-128' ",
            "--outfile='{fn_out}' ",
            "--NoDataValue=-128 --co compress=lzw --type=Int8 --overwrite"
        )
    )
}
