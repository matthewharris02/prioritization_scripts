# ---------------------------------------------------------------------------- #
# Convert all 634 oil palms tiles from three categories to binary
# Old categories:
#   1 = Industrial closed-canopy oil palm plantations
#   2 = Smallholder closed-canopy oil palm plantations
#   3 = Other land use and land cover not closed-canopy oil palm plantations
# New categories:
#   1 = Closed-canopy oil palm plantations
#   NA (-128) = everything else
# ---------------------------------------------------------------------------- #
year=2020

dirBase='/mnt/sda/restoration_opportunities/raw/sentinel/'
dirIn="${dirBase}/lulc${year}_orig"

dirOut="${dirBase}/lulc${year}_convertedLand"
echo "Making output directory ${dirOut}"
mkdir -p $dirOut

filesList=$dirBase/"files_lulc${year}.txt"
echo "Creating list of LULC files: $filesList"
rm -f $filesList
find $dirIn -iname "*.tif" >> $filesList
# Ensure extra new line is removed, if exists (otherwise the while loop fails)
# From: https://stackoverflow.com/a/1654042
perl -pi -e "chomp if eof" $filesList

while read -r line; do
    var="${line%.*}"
    echo "Processing $var"
    oldName="${var}.tif"
    newName="${dirOut}/${var##*/}_convertedLand.tif"
    gdal_calc.py -A $oldName --calc="(A==5)*(A==7)*0+(A==0)*-128+(A>=8)*(A==6)*(A<=4)*1" --type=Int8 --NoDataValue=-128 --outfile=$newName --co compress=lzw --overwrite
done <"${filesList}"

dirIn=$dirOut
filesList=$dirBase/"files_lulc${year}_convertedLand.txt"
echo "Creating list of LULC Converted Land files: $filesList"
rm -f $filesList
find $dirIn -iname "*.tif" >> $filesList 
outVrt=$dirIn/"lulc${year}_convertedLand_vrt.vrt"

echo "Building Virtual Raster: $outVrt"
gdalbuildvrt -input_file_list $filesList $outVrt