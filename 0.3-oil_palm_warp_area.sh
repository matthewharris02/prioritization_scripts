# UNTESTED
# ---------------------------------------------------------------------------- #
# Create area-based oil palm layers using outputs from 0.2, in three steps:
#   1. Warp to desired resolution using sum
#   2. Calculate area fraction
# ---------------------------------------------------------------------------- #
dirBase='O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2/raw/oil_palm'
dirIn="${dirBase}/oil_palm_converted/intermediate_binary"
res=5000
dirOut="${dirBase}/oil_palm_converted/intermediate_${res}km"

while read -r line; do
    var="${line%.*}"
    
    oldName="${var}_converted.tif"
    newName_res="${var}_${res}km.tif"
    newName_area="${var}_${res}km_${area}.tif"
    mkdir -p $dirOut

    # Step 1: reduce resolution
    gdalwarp -overwrite -s_srs EPSG:4326 -t_srs ESRI:54009 -tr $res $res -r sum "${dirIn}/${oldName}" "${dirOut}/${newName_res}"

    # Step 2: convert sum to area
    scale_factor=$(( (res / 10)**2 ))

    gdal_calc -A "${dirOut}/${newName_res}" --calc="A/${scale_factor}" --NoDataValue=-128 --outfile="${dirOut}/${newName_area}" --co compress=lzw --overwrite
done <"${dirBase}/files.txt"