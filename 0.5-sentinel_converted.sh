# ---------------------------------------------------------------------------- #
#
# ---------------------------------------------------------------------------- #
dirBase='/mnt/sda/restoration_opportunities/raw/sentinel'
var="gee_converted2020"
dirOut=$dirBase/$var

filesList=$dirOut/"files_${var}.txt"
echo "Creating list of ${var} files: $filesList"
rm -f $filesList
find $dirOut -iname "*.tif" >> $filesList 
outVrt=$dirOut/"${var}_vrt.vrt"

echo "Building Virtual Raster: $outVrt"
gdalbuildvrt -input_file_list $filesList $outVrt
gdal_translate $outVrt $dirBase/"${var}.tif" -co COMPRESS=LZW -co BIGTIFF=YES -co TILED=YES