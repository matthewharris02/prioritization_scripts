# Pre-processing of Human footprint from Williams et al. (2020)

# Reclassify with gdal_calc.py
ifile <- file.path(dir_in, pu_fn["hfp"])
ofile <- file.path(dirs["dir_inter"], fn_template("hfp_binary"))

# Include/restorable = 1, exclude = 0
system(glue(
    gdalcalc_path,
    ' -A "{ifile}"',
    ' --calc="(A<={hfp_lower})*(A>{hfp_upper})*0+(A>{hfp_lower})*(A<={hfp_upper})*1"',
    ' --co compress=lzw --overwrite --outfile "{ofile}"'
))

# Reduce resolution with gdalwarp
ifile <- file.path(dirs["dir_inter"], fn_template("hfp_binary"))
ofile <- file.path(dirs["dir_pu"], fn_template("hfp_mask"))
system2(
    gdalwarp_path,
    gdalwarp_args("mode", ifile, ofile, EPSG, RES, EXT),
    wait = TRUE
)