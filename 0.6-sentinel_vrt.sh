# ---------------------------------------------------------------------------- #
#
# ---------------------------------------------------------------------------- #
dirBase='/mnt/sda/restoration_opportunities/raw/sentinel'

var="convertedLand2020"

for var in ('convertedLand2020' 'snow2020'); do
    filesList=$dirOut/$var/"files_${var}.txt"
    echo "Creating list of ${var} files: $filesList"
    rm -f $filesList
    find $dirOut -iname "*.tif" >> $filesList 
    outVrt=$dirOut/$var/"${var}_vrt.vrt"

    echo "Building Virtual Raster: $outVrt"
    gdalbuildvrt -input_file_list $filesList $outVrt
done