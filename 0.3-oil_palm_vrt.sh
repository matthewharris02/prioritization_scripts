# ---------------------------------------------------------------------------- #
# 
# ---------------------------------------------------------------------------- #
dirBase='/mnt/sda/restoration_opportunities/raw/oil_palm'
dirIn="${dirBase}/oil_palm_orig"
dirOut="${dirBase}/oil_palm_binary_mask"

filesList=$dirOut/"files_oilpalm_binary.txt"
echo "Creating list of oil palm binary files: $filesList"
rm -f $filesList
find $dirOut -iname "*.tif" >> $filesList 
outVrt=$dirOut/"oilpalm_binary_mask_vrt.vrt"

echo "Building Virtual Raster: $outVrt"
gdalbuildvrt -input_file_list $filesList $outVrt