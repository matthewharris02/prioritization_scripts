# NOTE: this is an older script and needs more work to align with 2a
##%##########################################################################%##
#       Single budget spatial prioritisation (min_shorfall)                   #
#           V 07.08.2024    Matthew Harris and Vignesh Kamath                  #
##%##########################################################################%##
# 0. Load libraries ====
print("Loading libraries")
library(prioritizr)
library(data.table)
library(arrow)
library(tidyverse)
library(scales)
library(terra)
# 1. OPTIONS and set-up ====
dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_src <- dir_wd
# dir_wd <- "/home/matthewh@internal.wcmc/projects_active/p09217_RestorationPotentialLayer/global2024_v2"

## 1.1 Prioritzr options ====
# EDIT THESE :)
write_each <- TRUE   # If TRUE, writes solution for each budget
solver <- "lp"       # Which solver: cbc, (lp)symphony
opt_gap <- 0.01       # Choose gap for solver
opt_threads <- 1     # Choose number of threads (ONLY for CBC solver)
opt_budget <- 0.3
auto_dir <- TRUE     # Automatically create needed directories?
runid <- ""      # Additional ID to distinguish runs (NOTE: doesn't work yet
split <- TRUE        # Includes NCPs split by country

## 1.2 Shared options ====
# Load options file to share options with pre-processing
source(file.path(dir_src, "script_tools/v3/0.1-OPTIONS.R"))

# 1.3 Load package for solver ====
if (solver == "cbc") {
    library(rcbc)
} else if (solver == "lp") {
    library(lpsymphony)
}

## 1.4 Directory-related variables ====
dir_in <- file.path(dir_wd, "raw")
out_subdir <- ifelse(runid == "", paste0(RES, "km"), paste0(RES, "km_", runid))
dir_out <- file.path(dir_wd, "work_in_progress", out_subdir)

## 1.5 Template raster ====
rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)

## 1.6 Load NCP variable information ====

variables <- read_csv(file.path(dir_in, "preprocess_info.csv"))

ncp_national <- variables |>
    filter(split == "national") |>
    filter(grepl("ncp*", var)) |>
    select(var) |>
    unlist(use.names = FALSE)
ncp_global <- variables |>
    filter(split == "global") |>
    filter(grepl("ncp*", var)) |>
    select(var) |>
    unlist(use.names = FALSE)


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
        collect() |>
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
        select(id, x, y, ecoregions, all_of(ncp_global)) |>
        mutate(
            across(.cols = c(id, x, y), ~as.integer(.x)),
            cost = 1
        ) |>
        collect()

    ncp_split <- list.files(file.path(dir_out, "processed/split/"),
                            full.names = TRUE,
                            pattern = "*.parquet$") |>
        lapply(function(filename) { read_parquet(filename) }) |>
        do.call(rbind, args = _)

}


# 3. FEATURE LIST ====
## 3.1 Split feature ids ====
# Get feature ids for the split-by-country features
feat_ids_split <- list.files(file.path(dir_out, "processed/split"),
                             pattern = "*.csv", full.names = TRUE) |>
    map(~read_csv(.x)) |>
    bind_rows() |>
    rename(
        "name" = species,
        "species" = column_id
    )
feat_master <- feat_ids_split

## 3.2 Helper: add_feat() ====
# Helper function to add feature
add_feat <- function(feat, feat_master) {
    row <- data.frame(
        name = feat,
        species = max(feat_master$species, na.rm = TRUE) + 1
    )
    feat_master <- bind_rows(feat_master, row)
}

## 3.3 Add features: global ====
# Add feature for each global ncp
for (feat in ncp_global) {
    feat_master <- add_feat(feat, feat_master)
}

# Create intermediate feat_ncp to distinguish NCP from ecoregions later
feat_ncp <- feat_master

## 3.4 Ecoregion features ====
# Prepare ecoregion features
feat_ecoregions <- grid_cell |>
    select(ecoregions) |>
    arrange(ecoregions) |>
    filter(!is.na(ecoregions)) |>
    distinct() |>
    unlist() |>
    as.character()

# Add feature for each ecoregion
for (feat in feat_ecoregions) {
    feat_master <- add_feat(feat, feat_master)
}

## 3.5 List of feature ids
feat_ids <- deframe(feat_master)

# 4. RIJ ====
rij_global <- data.frame(pu = NULL, species = NULL, amount = NULL)

for (ncp in ncp_global) {
    pu_vals_ncp <- grid_cell |>
        select(id, all_of(ncp)) |>
        mutate(species = feat_ids[ncp]) |>
        rename_with(~c(ncp = "amount"), .cols = all_of(ncp)) |>
        select(id, species, amount) |>
        filter(!is.na(amount)) |>
        rename("pu" = id)
    rij_global <- bind_rows(rij_global, pu_vals_ncp)
    # last_column_id = max(rij_global$species, na.rm = TRUE)
}

rij_ecoregions <- grid_cell |>
    select(id, ecoregions) |>
    filter(!is.na(ecoregions)) |>
    mutate(
        ecoregions = as.character(ecoregions), # Convert to text to match feat_master
        amount = 1
    ) |>
    left_join(feat_master, join_by("ecoregions" == "name")) |> # Join ecoregion id with feature name
    select(-ecoregions) |>
    select(id, species, amount) |> # Re-order
    rename(
        "pu" = id
        )

rij <- rbindlist(list(ncp_split, rij_global, rij_ecoregions))
# 5. Targets ====
## 5.1  Ecoregion targets ====
ecoregions_data <- read_csv(file.path(dir_out, "planning_units/global_ecoregions_moll.csv"))

targets_ecoregions <- ecoregions_data |>
    filter(ECO_ID %in% feat_ecoregions) |>
    filter(!is.na(realised_extent)) |>
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
    select(feature, relative_target)

## All targets ====
targets <- tibble(
    feature = feat_ncp$species,
    relative_target = '1',
) |>
    mutate(relative_target = as.numeric(relative_target)) |>
    rbind(targets_ecoregions) |>
    select(relative_target) |>
    as.matrix()


# 6. Problem and solution ====
## 6.1 Costs ====
costs <- grid_cell |>
    select(id, cost)
features <- rename(feat_master, "id" = species)
## 6.2 Base problem ====
print("Creating base problem...")
p <- problem(
    x = costs,
    features = features,
    cost_column = "cost",
    rij = rij
) |>
    add_relative_targets(targets)

solutions <- list() # Solutions for each budget

## 6.3 Budget information ====
b <- opt_budget
b_cells <- b * sum(costs[, 2], na.rm = T)

## 6.4 Logging ====
times <- list() # Problem solving times
info_str <- "{runid}id_{solver}_{RES}km_{opt_gap}g_{opt_threads}t_{opt_budget}b"
info_readable <- str_glue("Running with {solver} solver, gap of {opt_gap}, \
               {opt_threads} threads, {opt_budget} budget\
               and a resolution of {RES} km")

print(info_readable)
print(str_glue("Number of features {length(features[[1]])}"))
print(glue::glue("Number of PU: {length(costs[[1]])}"))
print(str_glue("Staring logging to file"))
log_file <- file(
    file.path(dir_out, "logs", str_glue("log_", info_str, ".txt")),
    open = "wt")
sink(log_file, append = TRUE)
sink(log_file, append = TRUE, type = "message")
print(str_glue("Time and date of run: {Sys.time()}"))
print(info_readable)

print(str_glue("Number of features {length(features[[1]])}"))
print(glue::glue("Number of PU: {length(costs[[1]])}"))
## 6.5 Add budget  ====
p2 <- p |>
    add_min_shortfall_objective(budget = b_cells)

## 6.6 Add solver ====
# Select correct solver and set options
if (solver == "cbc") {
    p2 <- p2 |>
        add_cbc_solver(
            gap = opt_gap,
            threads = opt_threads,
            verbose = TRUE)
} else if (solver == "lp") {
    p2 <- p2 |> add_lpsymphony_solver(
        gap = opt_gap,
        verbose = TRUE)
}

print(p2)
# 6.6 Solve problem (and time it)
print("Solving...")
start <- Sys.time()
s <- solve(p2, run_checks = FALSE)
end <- Sys.time()

# Add solution (and time) to list
print(str_glue("Solving time:"))
print(as.numeric((end - start), units = "secs"))

sink()
sink(type = "message")
close(log_file)
print(str_glue("Logging ended and output to {log_file}"))
print(str_glue("Solving time:"))
print(as.numeric((end - start), units = "secs"))
end - start
fwrite(s, file.path(dir_out, "output",
                    str_glue("solution_", info_str, ".csv")))
