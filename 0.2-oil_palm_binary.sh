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
dirBase='O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2/raw/oil_palm'
dirIn="${dirBase}/oil_palm_orig"
dirOut="${dirBase}/oil_palm_converted/intermediate_binary"

while read -r line; do
    var="${line%.*}"
    
    oldName="${var}.tif"
    newName="${var}_converted.tif"
    mkdir -p $dirOut

    gdal_calc -A "${dirIn}/${oldName}" --calc="(A<3)*1+(A==3)*-128" --NoDataValue=-128 --outfile="${dirOut}/${newName}" --co compress=lzw --overwrite
done <files.txt