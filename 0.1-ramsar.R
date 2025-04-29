# Short script to extract Ramsar sites from WDPA
# Ensure to set:
#   - dir_wd
#   - WDPA_version -- format MonthYYYY
library(sf)

dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_in <- file.path(dir_wd, "raw")

WDPA_version <- "May2025"
# Load Ramsar data only from WDPA and then write to separate shp file
wdpa <- st_read(file.path(dir_in, glue("WDPA/WDPA_{WDPA_version}_Public.gdb")),
                layer = glue("WDPA_poly_{WDPA_version}"),
                query = glue("SELECT * FROM WDPA_poly_{WDPA_version} WHERE DESIG_ENG = 'Ramsar Site, Wetland of International Importance'")
                )
dir_out <- file.path(dir_in, glue("ramsar/ramsar_{WDPA_version}"))
if (!dir.exists(dir_out)) dir.create(dir_out)
write_sf(wdpa, file.path(dir_out, glue("ramsar_{WDPA_version}.shp")))