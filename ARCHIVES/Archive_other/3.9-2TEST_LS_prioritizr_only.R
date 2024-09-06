library(prioritizr)
library(data.table)
library(arrow)
library(tidyverse)
library(scales)
library(rcbc)
library(terra)

dir_wd <- "/home/matthewh@internal.wcmc/projects_active/p09217_RestorationPotentialLayer/global2024_v2"
dir_in <- file.path(dir_wd, "raw")
dir_out <- file.path(dir_wd, "work_in_progress")

RES = 5 # Relative to 1

## PROJECTION ====
# Set EPSG to the EPSG code (e.g., EPSG:43267, ESRI:54009)
# Set PROJ to the text that you want to label the files with (e.g., moll, laea)
EPSG = "ESRI:54009" # EPSG code for the CRS
PROJ = "moll"

## EXTENT ====
# This has been set to the maximum rounded extent within Mollweide (ESRI:54009)
#       for resolutions up to 20 km
# order: xmin, xmax, ymin, ymax
EXT = c(-18040000, 18040000, -9020000, 9020000)

## Template raster
rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)

print("PREPARING PU...")

grid_cell <- open_dataset(file.path(dir_out, "processed", "global_cells"),
                          partitioning = c("ISONUM")) |>
    mutate(
        across(.cols = c(id, x, y, ecoregions), ~as.integer(.x)),
        across(
            .cols = c(ncp_kba, ncp_ramsar, ncp_saltmarshes),
            .fns = ~ifelse(is.na(.x), 0, .x)
        ),
    ) |>
    select(-x, -y, -lulc, -hfp) |>
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

ncp_feats <- names(grid_cell)[grepl("ncp", names(grid_cell))]
num_feat <- length(ncp_feats)
feat_master <- tibble(
    id = 1:num_feat,
    name = ncp_feats[1:num_feat]
)

# Ecoregion targets

features <- feat_master



rij_create <- function(x, name) {
    # x: dataframe/data.table of grid cells and data
    data.frame(
        pu = x[,"id"][[1]],
        species = feat_master[feat_master$name == name,][[1]],
        amount = x[, ..name][[1]]
    )
}


rij <- ncp_feats |>
    map(~rij_create(grid_cell, .x)) |>
    bind_rows() |>
    filter(!is.na(amount))


targets <- tibble(
    feature = ncp_feats,
    sense = '=',
    target = '1',
    type = 'relative'
) |>
    mutate(target = as.numeric(target)) |>
    filter(!is.na(target))

costs <- grid_cell |>
    select(id, cost)


p <- problem(
    x = costs,
    features = features,
    cost_column = "cost",
    rij = rij) |>
    add_manual_targets(targets)

budgets <- seq(0.05, 1, 0.05)
solutions <- list()
times_p <- list() # Problem creation times
times_s <- list() # Problem solving times


# TEST

b <- budgets[1]
b_cells <- b * sum(costs[, 2], na.rm = T)
start_p <- Sys.time()
print("\nCreating problem")
p2 <- p |>
    add_min_shortfall_objective(budget = b_cells) |>
    add_cbc_solver(
        gap = 0.01,
        threads = 8,
        verbose = TRUE
    )
end_p <- Sys.time()

print("\nSolving")
# Solve problem (and time it)
start_s <- Sys.time()
s <- solve(p2, run_checks = FALSE)
end_s <- Sys.time()
print("FINISHED")
print("Time taken... ", end_s - start_s, " ...")

# Add solution (and time) to list

fwrite(s, file.path(dir_out, "processe", "solution.csv" ))