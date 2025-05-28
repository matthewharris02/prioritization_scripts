# ---------------------------------------------------------------------------- #
# Pre-process Human Footprint (hfp) from Gassert et al. (2023) to convert
#   to binary 'intact' raster
# Original rasters were scaled 0 to 50000, with 65535 as NA value
# New classes:
#   0 = Intact/low HFP areas
#   1 = Not-intact/high HFP areas
#   NA (-128) = No data 
# ---------------------------------------------------------------------------- #
year=2020

dirBase='/mnt/sda/restoration_opportunities/raw/hfp_gassert.etal2020'
dirIn="${dirBase}/hfp_100m_${year}"

dirOut="${dirBase}/hfp_100m_${year}_intact"
echo "Making output directory ${dirOut}"
mkdir -p $dirOut

filesList=$dirBase/"files_hfp_100m_${year}.txt"
echo "Creating list of hfp files: $filesList"
rm -f $filesList
find $dirIn -iname "*.tif" >> $filesList
# Ensure extra new line is removed (otherwise the while loop fails)
perl -pi -e "chomp if eof" $filesList

while read -r line; do
    var="${line%.*}"
    oldName="${var}.tif"
    newName="${dirOut}/${var##*/}_intactBinary.tif"
    gdal_calc.py -A $oldName --calc="(A<=3000)*0+(A>3000)*1+(A==65535)*-128" --type=Int8 --NoDataValue=-128 --outfile=$newName --co compress=lzw --overwrite
done <"${filesList}"

dirIn="${dirBase}/hfp_100m_${year}_intact"
filesList=$dirBase/"files_hfp_100m_${year}_intact.txt"
echo "Creating list of hfp binary files: $filesList"
rm -f $filesList
find $dirIn -iname "*.tif" >> $filesList 
outVrt=$dirIn/"hfp_100m_${year}_intact_vrt.vrt"

echo "Building Virtual Raster: $outVrt"
gdalbuildvrt -input_file_list $filesList $outVrt