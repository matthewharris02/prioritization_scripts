# 0. Load libraries ====
print("Loading libraries")
library(data.table)
library(arrow)
library(tidyverse)
library(terra)
library(matrixStats)
library(viridisLite)

dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_src <- dir_wd
source(file.path(dir_src, "script_tools/v3/1.1-OPTIONS.R"))

rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)


# Create a raster where each cell is the number of features that exist for that cell
sub_path <- "work_in_progress/OUTPUTS/inputs-5km_ls/"
grid_cell <- open_dataset(file.path(sub_path, "global_cells"),
                          partitioning = c("ISONUM")) |>
    select(-lulc, -hfp) |>
    mutate(
        across(
            .cols = !c(id, x, y),
            .fns = ~if_else(is.na(.x), 0, 1)
        )
    ) |>
    collect() |>
    rename("ncp_ecoregions" = ecoregions) |>
    setDT()



num_all <- grid_cell[, `:=` (SUM = rowSums(.SD)),
                        .SDcols = c("ncp_ecoregions", "ncp_carbon",
                                    "ncp_coastal", "ncp_iucnrichness", "ncp_kba",
                                    "ncp_mangroves", "ncp_pollination", "ncp_ramsar", "ncp_saltmarshes",
                                    "ncp_usefulplants", "ncp_waterquality")]

num_noEco <- grid_cell[, `:=` (SUM = rowSums(.SD)),
                        .SDcols = c("ncp_carbon",
                                    "ncp_coastal", "ncp_iucnrichness", "ncp_kba",
                                    "ncp_mangroves", "ncp_pollination", "ncp_ramsar", "ncp_saltmarshes",
                                    "ncp_usefulplants", "ncp_waterquality")]

num_noC <- grid_cell[, `:=` (SUM = rowSums(.SD)),
                       .SDcols = c("ncp_ecoregions",
                                   "ncp_coastal", "ncp_iucnrichness", "ncp_kba",
                                   "ncp_mangroves", "ncp_pollination", "ncp_ramsar", "ncp_saltmarshes",
                                   "ncp_usefulplants", "ncp_waterquality")]

num_noCoastal <- grid_cell[, `:=` (SUM = rowSums(.SD)),
                       .SDcols = c("ncp_ecoregions", "ncp_carbon",
                                   "ncp_iucnrichness", "ncp_kba",
                                   "ncp_pollination", "ncp_ramsar",
                                   "ncp_usefulplants", "ncp_waterquality")]


r_num_features <- rast(num_noCoastal[, c("x", "y", "SUM")],
                       crs = crs(EPSG),
                       extent = ext(rast_template))

writeRaster(r_num_features, file.path(dir_wd, "work_in_progress/OUTPUTS/analyze/num_features_noCoastal.tif"))



# Analyze solution ranking and number of features

sub_path <- "work_in_progress/OUTPUTS/"
num_features <- rast(file.path(sub_path, "analyze/num_features_full.tif"))
sol <- rast(file.path(sub_path, "5km_full_ls/solution_full_lp_5km_0.01g_1t_1b.tif"))

comb <- c(sol, num_features)
vals <- values(comb, na.rm = TRUE, dataframe = TRUE)

vals2 <- vals |>
    mutate(SUM = as.factor(SUM))

colours <- viridis(n = 12)

ggplot(vals2, aes(x = final, fill = SUM)) +
    scale_x_continuous(breaks = c(1, 5, 10, 15 ,20)) +
    geom_bar(just = 0.5) +
    scale_fill_manual(values = colours) +
    labs(
        x = "Solution Ranking",
        y = "Number of cells",
        fill = "Number of features"
    )
