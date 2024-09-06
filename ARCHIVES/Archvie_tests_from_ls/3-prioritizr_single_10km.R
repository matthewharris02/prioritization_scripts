print("Loading libraries")
library(prioritizr)
library(data.table)
library(arrow)
library(tidyverse)
library(scales)
library(terra)

# OPTIONS ====
solver = "lpsymphony"     # Which solver: cbc, lpsymphony
opt_gap = 0.01     # Choose gap for solver
opt_threads = 1   # Choose number of threads (ONLY used for CBC solver)
opt_budget = 0.05   # Choose budget to test

# dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_wd <- "/home/matthewh@internal.wcmc/projects_active/p09217_RestorationPotentialLayer/global2024_v2"

RES = 10 # Resolution: relative to 1km/30arsec

print(str_glue("Running with {solver} solver, gap of {opt_gap}, \
               {opt_threads} threads, {opt_budget} budget\
               and a resolution of {RES} km"))


# Load package for solver ====
if (solver == "cbc") {
    library(rcbc)
} else if (solver == "lpsymphony") {
    library(lpsymphony)
}
# Constants ====

## Directory-related variables ====
# NOTE: paths should NOT end with a slash as file.path() will sort this
# Directory set-up:
#   BASE
#     - raw                 | raw input data (i.e., downloaded data)
#     - script_tools        | these scripts
#     - work_in_progress    | output data (both from pre-processing, and final)
dir_in <- file.path(dir_wd, "raw")
dir_out <- file.path(dir_wd, paste0("work_in_progress/", RES, "km"))

## PROJECTION ====
# Set EPSG to the EPSG code (e.g., EPSG:43267, ESRI:54009)
# Set PROJ to the text that you want to label the files with (e.g., moll, laea)
EPSG = "ESRI:54009" # EPSG code for the CRS
PROJ = "moll" # Label ONLY
EXT = c(-18040000, 18040000, -9020000, 9020000)

## Template raster
rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)

##%#########################################################################%##
log_file <- file(
    file.path(dir_out, "processed", str_glue("log_test_solution_{solver}_{RES}km_{opt_gap}g_{opt_threads}t.txt")),
    open = "wt")
print(str_glue("Staring logging to file"))
sink(log_file, append = TRUE)
sink(log_file, append = TRUE, type = "message")
print(str_glue("Time and date of run: {Sys.time()}"))
print(str_glue("Running with {solver} solver, gap of {opt_gap}, {opt_threads} threads, {opt_budget} budget and a resolution of {RES} km"))

# PREPARE GRID CELLS ====
print("Preparing grid cells...")
grid_cell <- open_dataset(file.path(dir_out, "processed", "global_cells"),
                          partitioning = c("ISONUM")) |>
    mutate(
        across(.cols = c(id, x, y, ecoregions), ~as.integer(.x)),
        across(
            .cols = c(ncp_kba, ncp_ramsar, ncp_saltmarshes),
            .fns = ~ifelse(is.na(.x), 0, .x)
            ),
    ) |>
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

ncp_feats <- names(grid_cell)[grepl("ncp", names(grid_cell))]
num_feat <- length(ncp_feats)
feat_master <- tibble(
    id = 1:num_feat,
    name = ncp_feats[1:num_feat]
)

# Ecoregion targets
ecoregions_data <- read_csv(file.path(dir_out, "planning_units/global_ecoregions_moll.csv"))

ecoregions_targets <- ecoregions_data |>
    mutate(
        target = (1 - remnant_proportion) |>
            scales::rescale(
                from = c(0.20, 0.75),
                to   = c(0.10, 0.30)),
        target = case_when(
            target >= 0.3 ~ 0.3,
            target < 0.3 & target > 0.1 ~ target,
            target <= 0.1 ~ 0.1
        )
    ) |>
    rename(
        relative_target = target,
        feature = ECO_NAME
    ) |>
    filter(!is.na(relative_target)) |>
    select(feature, relative_target)

## Features ====

features_ecor <- ecoregions_data |>
    filter(!is.na(realised_extent)) |> # catch those ecoregions that don't exist for some reason
    select(ECO_NAME) %>%
    mutate(
        id = (num_feat + 1):(nrow(.) + num_feat),
        .before = ECO_NAME
    ) |>
    rename(
        name = ECO_NAME
    )

features <- feat_master |>
    rbind(features_ecor)

total_feat <- dim(features)[[1]]
print(str_glue("Number of features: {total_feat}"))


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
    relative_target = '1',
) |>
    mutate(relative_target = as.numeric(relative_target)) |>
    rbind(ecoregions_targets) |>
    select(relative_target) |>
    as.matrix()

costs <- grid_cell |>
    select(id, cost)

# Create base problem
print("Creating base problem...")
p <- problem(
    x = costs,
    features = features,
    cost_column = "cost",
    rij = rij
) |>
    add_relative_targets(targets)

solutions <- list() # Solutions for each budget
times <- list() # Problem solving times

# Create budget information
b <- opt_budget
b_cells <- b * sum(costs[, 2], na.rm = T)

# Logging

# Sort problem depending on budget
p2 <- p |>
    add_min_shortfall_objective(budget = b_cells)

# Select correct solver and set options
if (solver == "cbc") {
    p2 <- p2 |>
        add_cbc_solver(
            gap = opt_gap,
            threads = opt_threads,
            verbose = TRUE)
} else if (solver == "lpsymphony") {
    p2 <- p2 |> add_lpsymphony_solver(
        gap = opt_gap,
        verbose = TRUE)
} # IF solver

# Solve problem (and time it)
print("Solving...")
start <- Sys.time()
s <- solve(p2, run_checks = FALSE)
end <- Sys.time()

# Add solution (and time) to list
print(str_glue("Solving time:"))
print(end - start)

sink()
sink(type = "message")
close(log_file)
print(str_glue("Logging ended and output to file"))
print(str_glue("Solving time:"))
print(end - start)
end - start
fwrite(s, file.path(dir_out, "output",
                    str_glue("test_solution_{solver}_{RES}km_{opt_gap}g_{opt_threads}t.csv")))
