library(sf)

dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"

source(file.path(dir_wd, "script_tools/v3/0-OPTIONS.R"))


runid = ""
dir_in <- file.path(dir_wd, "raw")
out_subdir <- ifelse(runid == "", paste0(RES, "km"), paste0(RES, "km_", runid))
dir_out <- file.path(dir_wd, "work_in_progress/", out_subdir)


# Load Ramsar data only from WDPA and then write to separate shp file
wdpa <- st_read(file.path(dir_in, "WDPA/WDPA_Jul2024_Public.gdb"),
                layer = "WDPA_poly_Jul2024",
                query = "SELECT * FROM WDPA_poly_Jul2024 WHERE DESIG_ENG = 'Ramsar Site, Wetland of International Importance'"
                )
write_sf(wdpa,file.path(dir_in, "ramsar/ramsar_2024/ramsar_2024.shp") )


# Compare WDPA and RSIS ramsar data
ramsar_wdpa <- st_read(file.path(dir_in, "ramsar/ramsar_2024/ramsar_2024.shp"))
ramsar_rsis <- st_read(file.path(dir_in, "ramsar/rsis/features_published/features_publishedPolygon.shp"))
