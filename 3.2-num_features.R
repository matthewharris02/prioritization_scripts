# 0. Load libraries ====
print("Loading libraries")
library(data.table)
library(arrow)
library(tidyverse)
library(terra)
# library(matrixStats)
library(viridisLite)

dir_wd <- "/mnt/sda/MH_restoration"
dir_src <- dir_wd
runid <- ""           # Additional ID to distinguish runs
source(file.path(dir_src, "script_tools/1.1-OPTIONS.R"))
dir_output <- file.path(dir_out, "output", ifelse(runid == "", "default", runid))

rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)


grid_cell <- open_dataset(file.path(dir_proc, "global_cells"),
                          partitioning = c("ISONUM"))  |>
    select(-id) |>
    collect()

ft <- grid_cell |>
    select(-lulc_converted, -lulc_other, -pu, -ISONUM) |>
    mutate(
        across(
            .cols = !c( x, y),
            .fns = ~if_else(is.na(.x), 0, 1)
        )
    )





# Create a raster where each cell is the number of features that exist for that cell

num_all <- ft[, `:=` (SUM = rowSums(.SD)),
                        .SDcols = c("ecoregions","ft_carbon",
                                    "ft_coastal", "ft_iucnrichness", "ft_kba",
                                    "ft_mangroves", "ft_pollination", "ft_ramsar", "ft_saltmarshes",
                                    "ft_usefulplants", "ft_waterquality")]

num_noEco <- ft[, `:=` (SUM = rowSums(.SD)),
                        .SDcols = c("ft_carbon",
                                    "ft_coastal", "ft_iucnrichness", "ft_kba",
                                    "ft_mangroves", "ft_pollination", "ft_ramsar", "ft_saltmarshes",
                                    "ft_usefulplants", "ft_waterquality")]

num_noC <- ft[, `:=` (SUM = rowSums(.SD)),
                       .SDcols = c(
                                   "ft_coastal", "ft_iucnrichness", "ft_kba",
                                   "ft_mangroves", "ft_pollination", "ft_ramsar", "ft_saltmarshes",
                                   "ft_usefulplants", "ft_waterquality")]

num_noCoastal <- ft[, `:=` (SUM = rowSums(.SD)),
                       .SDcols = c("ft_carbon",
                                   "ft_iucnrichness", "ft_kba",
                                   "ft_pollination", "ft_ramsar",
                                   "ft_usefulplants", "ft_waterquality")]


r_num_features <- rast(num_noCoastal[, c("x", "y", "SUM")],
                       crs = crs(EPSG),
                       extent = ext(rast_template))

writeRaster(r_num_features, file.path(dir_output, "num_features_noCoastal.tif"))

r_num_features_all <- rast(num_all[, c("x", "y", "SUM")],
                       crs = crs(EPSG),
                       extent = ext(rast_template))

writeRaster(r_num_features_all, file.path(dir_output, "num_features_all.tif"))

# Analyze solution ranking and number of features
solution <- read_csv(file.path(dir_output, "solution_full_lp_5km_0.01g_1t_1b.csv"))

all <- left_join(num_all, solution, join_by(x == x, y == y)) |>
    mutate(SUM = as.factor(SUM))




colours <- viridis(n = 12)


ggplot(all, aes(x = final, fill = SUM)) +
    scale_x_continuous(breaks = seq(1,10)) +
    geom_bar(just = 0.5) +
    scale_fill_manual(values = colours) +
    labs(
        x = "Solution Ranking",
        y = "Number of cells",
        fill = "Number of features"
    )
