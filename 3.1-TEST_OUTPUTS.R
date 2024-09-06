library(data.table)
library(arrow)
library(terra)
library(tidyverse)

dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
setwd(dir_wd)
dir_out <- file.path(dir_wd, "work_in_progress/5km")
dir_out <- file.path(dir_wd, "work_in_progress/ls_aug/090824")


grid_cell <- open_dataset(file.path(dir_out, "global_cells"),
                          partitioning = c("ISONUM")) |>
    select(id, x, y) |>
    collect()


grid_cell <- open_dataset(file.path(dir_out, "ls_aug/global_cells"),
                          partitioning = c("ISONUM")) |>
    select(id, x, y, ecoregions, all_of(ncp_global), ISONUM) |>
    collect()

grid_cell$id |> unique() |> length()
grid_cell$id |> length()


n <- data.frame(table(grid_cell$id))
n2 <- n[n$Freq > 1,]

dupl <- n2$Var1 |> unlist(use.names = FALSE) |> as.integer()

sel <- grid_cell[id %in% dupl, ]


|>
    mutate(
        across(.cols = c(id, x, y, ecoregions), ~as.integer(.x)),
        across(
            .cols = c(ncp_kba, ncp_ramsar, ncp_saltmarshes),
            .fns = ~ifelse(is.na(.x), 0, .x)
        ),
    ) |>
    collect()


|>
    select(-lulc, -hfp) |>
    collect()
grid_cell <- grid_cell |>
    mutate(
        cost = 1,
        across(
            .cols = starts_with("ncp"),
            .fns = ~scales::rescale(.x,
                                    to = c(0,1),
                                    from = c(0, max(.x, na.rm = TRUE)))
        )

    )


costs <- grid_cell |>
    select(id, cost)
opt_budget = 0.3
b <- opt_budget
b_cells <- b * sum(costs[, 2], na.rm = T)



summ <- grid_cell |>
    summarise(
        across(.cols = starts_with("ncp"),
               list(
                   min = ~min(.x, na.rm = TRUE),
                   max = ~max(.x, na.rm = TRUE)
                   ),
               .names = "{.col}_{.fn}")) |>
    pivot_longer(
        cols = everything(),
        cols_vary = "slowest",
        names_to = c("ncp", ".value"),
        names_pattern = "(ncp_.*)_(.*)",
    )
#####

RES = 5

## PROJECTION ====
# Set EPSG to the EPSG code (e.g., EPSG:43267, ESRI:54009)
# Set PROJ to the text that you want to label the files with (e.g., moll, laea)
EPSG = "ESRI:54009" # EPSG code for the CRS
PROJ = "moll" # Label ONLY

## EXTENT ====
# This has been set to the maximum rounded extent within Mollweide (ESRI:54009)
#       for resolutions up to 20 km
# order: xmin, xmax, ymin, ymax; units: units of the projection
EXT = c(-18040000, 18040000, -9020000, 9020000)


rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)


grid_cell <- open_dataset(file.path(dir_out, "ls_aug/global_cells"),
                          partitioning = c("ISONUM")) |>
    select(x, y, id) |>
    collect()

solution <- fread(file.path(dir_out,"solution_id_lp_5km_0.01g_1t_0.05b.csv"))


combined_solution <- grid_cell |>
    right_join(solution, by = "id") |>
    select(-cost, -id) |>
    rast(
        crs = crs(EPSG),
        extent = ext(rast_template)
    )

writeRaster(combined_solution,
            file.path(dir_out, "solution_id_lp_5km_0.01g_1t_0.05b.tif"),
            overwrite = T)




grid_cell <- open_dataset(file.path(dir_out, "global_cells"),
                          partitioning = c("ISONUM")) |>
    select(id, x, y, ecoregions) |>
    mutate(
        across(.cols = c(id, x, y, ecoregions), ~as.integer(.x)),
    ) |>
    collect()


combined <- list.files(file.path(dir_out, "5km/processed/split/"), full.names = TRUE) |>
    lapply(function(filename) { read_parquet(filename)}) |>
    reduce(left_join, by = "id")

grid_cell <- left_join(grid_cell, combined, by = "id") |>
    mutate(
        across(
            .cols = c(starts_with("ncp_kba"), starts_with("ncp_ramsar"),
                      starts_with("ncp_saltmarshes")),
            .fns = ~ifelse(is.na(.x), 0, .x)
        ),
        cost = 1
    )