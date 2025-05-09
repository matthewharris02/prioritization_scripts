# ---------------------------------------------------------------------------- #
#
# ---------------------------------------------------------------------------- #
dirBase='/mnt/sda/restoration_opportunities/raw/sentinel'
var="convertedLand2020"
dirOut=$dirBase/$var

filesList=$dirOut/"files_${var}.txt"
echo "Creating list of ${var} files: $filesList"
rm -f $filesList
find $dirOut -iname "*.tif" >> $filesList 
outVrt=$dirOut/$var/"${var}_vrt.vrt"

echo "Building Virtual Raster: $outVrt"
gdalbuildvrt -input_file_list $filesList $outVrt