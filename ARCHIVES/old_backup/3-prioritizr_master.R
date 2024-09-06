print("Loading libraries")
library(prioritizr)
library(data.table)
library(arrow)
library(tidyverse)
library(scales)
library(terra)

# OPTIONS ====
write_each = TRUE # If TRUE, writes solution for each budget
solver = "lp"     # Which solver: cbc, (lp)symphony
opt_gap = 0.1     # Choose gap for solver
opt_threads = 1    # Choose number of threads (ONLY for CBC solver)
auto_dir = TRUE    # Automatically create needed directories?
runid = "test"
split = FALSE

dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
# dir_wd <- "/home/matthewh@internal.wcmc/projects_active/p09217_RestorationPotentialLayer/global2024_v2"

## PROJECTION ====
# Set EPSG to the EPSG code (e.g., EPSG:43267, ESRI:54009)
# Set PROJ to the text that you want to label the files with (e.g., moll, laea)
EPSG = "ESRI:54009" # EPSG code for the CRS
PROJ = "moll" # Label ONLY
RES = 5 # Resolution: relative to 1km/30arsec
EXT = c(-18040000, 18040000, -9020000, 9020000) # IF PROJ changes, change


# Load package for solver ====
if (solver == "cbc") {
    library(rcbc)
} else if (solver == "lp") {
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
dir_logs <- file.path(dir_out, "logs", runid)

## Create directories ====
if (auto_dir == TRUE) {
    sub_dirs <- c("output", "processed", "logs", "planning_units", "ncp")
    file.path(dir_out, sub_dirs) |>
        walk(~if(!dir.exists(.x)) { dir.create(.x, recursive = TRUE)})
    if(!dir.exists(dir_logs)) { dir.create(dir_logs, recursive = TRUE)}
}


## Template raster
rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)

# PREPARE GRID CELLS ====
print("Preparing grid cells...")

if (!split) {

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

} else if (split) {
    grid_cell <- open_dataset(file.path(dir_out, "processed", "global_cells"),
                              partitioning = c("ISONUM")) |>
        select(id, x, y, ecoregions) |>
        mutate(
            across(.cols = c(id, x, y, ecoregions), ~as.integer(.x)),
        ) |>
        collect()


    combined <- list.files(file.path(dir_out, "processed/split/"), full.names = TRUE) |>
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
}


ncp_feats <- names(grid_cell)[grepl("ncp", names(grid_cell))]

# |>
#     str_extract("(ncp_[a-z]+)(_[0-9]+)", group = 1) |>
#     unique()
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

budgets <- seq(0.05, 1, 0.05)
solutions <- list() # Solutions for each budget
times <- list() # Problem solving times

info_str <- "{solver}_{RES}km_{opt_gap}g_{opt_threads}t_{budgets[i]}b"


f <- file(file.path(dir_logs, "log0_run_details.txt"),open = "wt")
sink(f, append = TRUE)
sink(f, append = TRUE, type = "message")
print(str_glue("== Details for run: {runid} == "))
print(str_glue("Solver: {solver}"))
print(str_glue("RES: {RES}"))
print(str_glue("Gap: {opt_gap}"))
print(str_glue("Threads: {opt_threads}"))
print(str_glue("Number of features: {num_feat}"))
print("List of features:")
for (ncp in ncp_feats) {
    print(str_glue("  - {ncp}"))
}
sink()
sink(type = "message")
close(f)

solution_details <- data.frame()


print("Starting each budget")

for (i in 1:length(budgets)) {
    glue::glue("= Starting budget {budgets[i]} =") |> print()

    # START LOGGING
    file <- file(
        file.path(dir_logs, str_glue("log{i}_", {info_str}, ".txt")),
        open = "wt")
    sink(file, append = TRUE)
    sink(file, append = TRUE, type = "message")

    glue::glue("= Starting budget {budgets[i]} =") |> print()
    # Create budget information
    b <- budgets[i]
    b_cells <- b * sum(costs[, 2], na.rm = T)


    # Sort problem depending on budget
    if (i == 1) { # Slightly different problem for first run
        p2 <- p |>
            add_min_shortfall_objective(budget = b_cells)
    } else { # Add locked-in constraints of previous solution
        # Format previous solution for locked-in constraint
        prev <- solutions[[i - 1]]
        prev_mod <- cbind(prev[,1:2], sapply(prev[,3], FUN = \(x)if_else(x == 1, TRUE, FALSE)))
        # Update problem
        p2 <- p |>
            add_min_shortfall_objective(budget = b_cells) |>
            add_locked_in_constraints(prev_mod[[3]])
    } # IF i

    # Select correct solver and set options
    if (solver == "cbc") {
        p2 <- p2 |>
            add_cbc_solver(
                gap = opt_gap,
                threads = opt_threads,
                verbose = TRUE
                )
    } else if (solver == "lp") {
        p2 <- p2 |>
            add_lpsymphony_solver(
                gap = opt_gap,
                verbose = TRUE
            )
    } # IF solver

    # Solve problem (and time it)
    print("Solving...")
    start <- Sys.time()
    s <- solve(p2, run_checks = FALSE)
    end <- Sys.time()


    # Add solution (and time) to list
    solutions[[i]] <- s
    times[[i]] <- as.numeric((end - start), units = "secs")

    details_list <- c("objective", "runtime", "status", "gap") |>
        map(~attr(s, .x)) |>
        unlist() |>
        append(c(times[[i]], budgets[[i]]))

    solution_details <- rbind(solution_details, details_list)

    print(str_glue("Solving took {end-start} seconds long...!"))

    if (write_each == TRUE) {
        fwrite(s, file.path(dir_out, "output", str_glue("solution_single_", info_str, ".csv")))
    } # IF write_each

    # END LOGGING
    sink()
    sink(type = "message")
    close(file)

    print(str_glue("Solving took {end-start} seconds long...!"))
} # FOR budget

# Combine solutions into one 'ranked' solution ====
print("Combining solutions...")
combined_solution <- solutions |>
    reduce(left_join, by = "id") |>
    select(-starts_with("cost")) |>
    rowwise() |>
    mutate(
        final = sum(c_across(starts_with("solution")))
    ) |>
    select(id, final) |>
    tibble() |>
    left_join(select(grid_cell, c("id", "x", "y")), by = "id") |>
    write_csv(file.path(dir_out, "output", str_glue("solution_full_", info_str, ".csv")))

# Convert matrix to raster ====
r <- rast(combined_solution[,c("x", "y", "final")],
          crs = crs(EPSG),
          extent = ext(rast_template)
)

writeRaster(r,
            file.path(dir_out, "output", str_glue("solution_full_", info_str, ".tif")),
            overwrite = T)

times_df <- write.csv(solution_details,
                      file.path(dir_logs, str_glue("details_", info_str, ".csv")))
