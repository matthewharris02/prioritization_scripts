##%##########################################################################%##
#       Single budget spatial prioritisation (min_shorfall)                    #
##%##########################################################################%##
#   REMINDERS:                                                                 #
#       - CHECK the dir_wd                                                     #
#       - check the file name for shared options (`1.1-OPTIONS.R`)             #
#       - Check the solution-related options (e.g., runid)                     #
##%##########################################################################%##

# 0. Load libraries ====
print("Loading libraries")
library(prioritizr)
library(data.table)
library(arrow)
library(tidyverse)
library(scales)
library(terra)
library(glue)

# 1. OPTIONS and set-up ====
## 1.1 EDITABLE options ====
dir_wd <- "/mnt/sda/MH_restoration"
# dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_src <- dir_wd
### Prioritzr-related options ====
write_each <- TRUE    # If TRUE, writes solution for each budget
solver <- "lp"        # Which solver: cbc, (lp)symphony
opt_gap <- 0.01       # Choose gap for solver
opt_threads <- 1      # Choose number of threads (ONLY for CBC solver)
opt_budget <- 0.3

### Solution-related options ====
auto_dir <- TRUE      # Automatically create needed directories?
runid <- ""           # Additional ID to distinguish runs
split <- TRUE         # Include features split by country?
opt_ecoregions <- TRUE # Include ecoregions?
# drop_features: Select which features to drop
#   !! each ones should be a string
#   !! To include all/exclude none leave empty `c()` or as `NULL`
drop_feature <-  c()

## 1.2 Shared options ====
# Load options file to share options with pre-processing
source(file.path(dir_src, "script_tools/1.1-OPTIONS.R"))

# 1.3 Load package for solver ====
if (solver == "cbc") {
    library(rcbc)
} else if (solver == "lp") {
    library(lpsymphony)
}

## 1.4 Directory-related variables ====
dir_in <- file.path(dir_wd, "raw")
dir_id <- "test"
dir_out <- file.path(dir_wd, "work_in_progress",
                     paste0(RES, "km",
                         ifelse(dir_id == "", "", paste0("_", dir_id))
                     ))

dir_in <- file.path(dir_wd, "raw")

dirs <- create_info(dir_out)

dirs["dir_output"] <- dir_output <- file.path(dirs["dir_out"], "output", ifelse(runid == "", "default", runid))
dirs["dir_logs"] <- file.path(dirs["dir_out"], "logs", ifelse(runid == "", "default",runid))

if (auto_dir) {
    c(dirs) |>
        walk(\(x) if(!dir.exists(x)) { dir.create(x, recursive = TRUE)})
}


## 1.5 Template raster ====
rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)

## 1.6 Load feature variable information ====
exclude_feature <- str_flatten(drop_feature, "|") # Create regex string to exclude vars
if (is.null(drop_feature)) {exclude_feature <- "^$"} # Work-around for matching nothing so that if drop_features is empty, it selects them all

variables <- read_csv(file.path(dirs["dir_in"], "preprocess_info.csv")) |>
    filter(grepl("ft_*", var)) |> # select only ft_ variables
    filter(!grepl(exclude_feature, var)) # exclude the variables in drop_feature

# List of all features for loading
ft_all <- unlist(select(variables, var), use.names = FALSE)

# List of features to be split nationally or left globally for selecting later
ft_national <- variables |>
    filter(split == "national") |>
    select(var) |>
    unlist(use.names = FALSE)
ft_global <- variables |>
    filter(split == "global") |>
    select(var) |>
    unlist(use.names = FALSE)



# 2. PREPARE GRID CELLS ====
print("Preparing grid cells...")

# Create variable for whether to include ecoregions
# If opt_ecoregions is FALSE, it is set to empty string
# any_of() will then ignore it and select will also ignore it
# so the column won't be loaded in
col_ecoregions <- ""
if (opt_ecoregions) {col_ecoregions <- "ecoregions"}

if (!split) {

    grid_cell <- open_dataset(file.path(dirs["dir_proc"], "global_cells"),
                              partitioning = c("ISONUM")) |>
        select(id, x, y, any_of(col_ecoregions), all_of(ft_all)) |>
        mutate(
            across(any_of(c("id", "x", "y", col_ecoregions)),
                   ~as.integer(.x)),
            across(any_of(c("ft_kba", "ft_ramsar", "ft_saltmarshes")), # Use any_of just in case one of these is excluded
                   ~ifelse(is.na(.x), 0, .x)),
        ) |>
        collect() |>
        mutate(
            cost = 1,
            across(
                .cols = starts_with("ft"),
                .fns = ~scales::rescale(.x,
                                        to = c(0, 1),
                                        from = c(0, max(.x, na.rm = TRUE)))
            )
        )

} else if (split) {


    grid_cell <- open_dataset(file.path(dirs["dir_proc"], "global_cells"),
                              partitioning = c("ISONUM")) |>
        select(id, x, y, any_of(col_ecoregions), all_of(ft_global)) |>
        mutate(
            across(.cols = c(id, x, y), ~as.integer(.x)),
            cost = 1
        ) |>
        collect()

    ft_split <- paste0(file.path(dirs["dir_proc"], "split"), "/", ft_national, ".parquet") |>
        lapply(function(filename) { read_parquet(filename) }) |>
        do.call(rbind, args = _)

    ft_split <- ft_split[, c("amount") :=
                             scales::rescale(amount,
                                             to = c(0, 1),
                                             from = c(0, max(amount, na.rm = TRUE))),
                         by = species]

}


# 3. FEATURE LIST ====
## 3.1 Split feature ids ====
# Create regex string to exclude vars
exclude_feature <- str_flatten(drop_feature, "|")
# Work-around for matching nothing so that if drop_features is empty,
#   it selects them all
if (is.null(drop_feature)) {exclude_feature <- "^$"}
feat_master <- data.frame(name = NULL, species = NULL)
# Get feature ids for the split-by-country features
if (split) {
    feat_ids_split <- list.files(file.path(dirs["dir_proc"], "split"),
                                 pattern = "*.csv", full.names = TRUE)

    feat_ids_split <- feat_ids_split[!grepl(exclude_feature, feat_ids_split)] |>
        map(~read_csv(.x)) |>
        bind_rows() |>
        rename(
            "name" = species,
            "species" = column_id
        )
    feat_master <- feat_ids_split
} else { # TODO: do we need this else? It re-defines feat_master. Or do we remove first one?
    feat_master <- data.frame(name = NULL, species = NULL)
}

## 3.2 Helper: add_feat() ====
# Helper function to add feature
add_feat <- function(feat, feat_master) {
    # If split == FALSE, then max() returns -Inf,
    #   so need to set species_id_start to 0 for the iterative id to work
    species_id_start <- ifelse(max(feat_master$species, na.rm = TRUE) == -Inf,
                               0,
                               max(feat_master$species, na.rm = TRUE))
    row <- data.frame(
        name = feat,
        species = species_id_start + 1
    )
    feat_master <- bind_rows(feat_master, row)
}

## 3.3 Add features: global ====
# Add feature for each global feature (exc. ecoregions)
for (feat in ft_global) {
    feat_master <- add_feat(feat, feat_master)
}

# Create intermediate feat_ft to distinguish ecoregions from other features later
feat_ft <- feat_master

## 3.4 Ecoregion features ====
if (opt_ecoregions) {
    # Prepare ecoregion features

    ecoregions_data <- read_csv(file.path(dirs["dir_pu"], "global_ecoregions_moll.csv"))

    feat_ecoregions <- ecoregions_data |>
        filter(!is.na(realised_extent)) |>
        select(ECO_ID) |>
        arrange(ECO_ID) |>
        filter(!is.na(ECO_ID)) |>
        distinct() |>
        unlist() |>
        as.character()

    # Add feature for each ecoregion
    for (feat in feat_ecoregions) {
        feat_master <- add_feat(feat, feat_master)
    }
}
## 3.5 List of feature ids
feat_ids <- deframe(feat_master)

# 4. RIJ ====
rij_global <- data.frame(pu = NULL, species = NULL, amount = NULL)

for (ft in ft_global) {
    pu_vals_ft <- grid_cell |>
        select(id, all_of(ft)) |>
        mutate(species = feat_ids[ft]) |>
        rename_with(~c(ft = "amount"), .cols = all_of(ft)) |>
        select(id, species, amount) |>
        filter(!is.na(amount)) |>
        rename("pu" = id)
    rij_global <- bind_rows(rij_global, pu_vals_ft)
}

if (opt_ecoregions) {
    rij_ecoregions <- grid_cell |>
        select(id, ecoregions) |>
        filter(!is.na(ecoregions)) |>
        filter(ecoregions %in% feat_ecoregions) |> 
        mutate(
            ecoregions = as.character(ecoregions), # Convert to text to match feat_master
            amount = 1
        ) |>
        left_join(feat_master, join_by("ecoregions" == "name")) |>
        select(-ecoregions) |>
        select(id, species, amount) |> # Re-order
        rename(
            "pu" = id
        )
}

if (split) {
    if (opt_ecoregions) {
        rij <- rbindlist(list(ft_split, rij_global, rij_ecoregions))
    } else {
        rij <- rbindlist(list(ft_split, rij_global))
    }
} else {
    if (opt_ecoregions) {
        rij <- rbindlist(list(rij_global, rij_ecoregions))
    } else {
        rij <- rij_global
    }
}


# 5. Targets ====
## 5.1  Ecoregion targets ====
if (opt_ecoregions) {
    ecoregions_data <- read_csv(file.path(dirs["dir_pu"], "global_ecoregions_moll.csv"))

    targets_ecoregions <- ecoregions_data |>
        filter(!is.na(realised_extent)) |>
        mutate(
            target = (1 - remnant_proportion)
        ) |>
        rename(
            relative_target = target,
            feature = ECO_NAME
        ) |>
        select(feature, relative_target)
}
## All targets ====
targets <- tibble(
    feature = feat_ft$species,
    relative_target = "1",
) |>
    mutate(relative_target = as.numeric(relative_target))

if (opt_ecoregions) {
    targets <- rbind(targets, targets_ecoregions)
}

targets <- targets |>
    select(relative_target) |>
    as.matrix()

# 6. Problem and solution ====
## 6.1 Costs ====
costs <- grid_cell |>
    select(id, cost)
features <- rename(feat_master, "id" = species)
## 6.2 Base problem ====
# Create base problem
print("Creating base problem...")
p <- problem(
    x = costs,
    features = features,
    cost_column = "cost",
    rij = rij
) |>
    add_relative_targets(targets)

b <- opt_budget
b_cells <- b * sum(costs[, 2], na.rm = T)
solutions <- list() # Solutions for each budget
times <- list() # Problem solving times

# Template string with basic info on solution for using with glue::glue later
info_str <- paste0("{solver}_{RES}km_{opt_gap}g_{opt_threads}t_{budgets[i]}b_",
                   ifelse(runid == "", "default", runid))


f <- file(file.path(dirs["dir_logs"],
              paste0(glue("log0_run_details_{solver}_{RES}km_{opt_gap}g_{opt_threads}t_"),
                     ifelse(runid == "", "default", runid), ".txt")
          ), open = "wt")
sink(f, append = TRUE)
sink(f, append = TRUE, type = "message")
print(glue::glue("== Details for run: {runid} == "))
print(glue::glue("Solver: {solver}"))
print(glue::glue("RES: {RES}"))
print(glue::glue("Gap: {opt_gap}"))
print(glue::glue("Threads: {opt_threads}"))
print(glue::glue("Number of features: {dim(features)[1]}"))
print(glue::glue("Number of PU: {dim(costs)[1]}"))
# print("List of features:")
# for (ft in ft_feats) {
#     print(glue::glue("  - {ft}"))
# }
sink()
sink(type = "message")
close(f)

solution_details <- data.frame()


print("Starting each budget")


glue::glue("= Starting budget {budgets[i]} =") |> print()

# START LOGGING
file <- file(file.path(dirs["dir_logs"], str_glue("log{i}_", {info_str}, ".txt")),
                open = "wt")
sink(file, append = TRUE)
sink(file, append = TRUE, type = "message")

glue::glue("= Starting budget {budgets[i]} =") |> print()
# Create budget information
b <- budgets[i]
b_cells <- b * sum(costs[, 2], na.rm = TRUE)

p2 <- p |>
    add_min_shortfall_objective(budget = b_cells)


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
s <- try(solve(p2, run_checks = FALSE))
if (inherits(s, "try-error")) {
    print("FAILED, running again")
    s <- try(solve(p2, run_checks = FALSE))
    if (inherits(s, "try-error")) {
        pritn("FAILED twice, ending...")
        sink()
        sink(type = "message")
        close(file)
        print("FAILED twice ... logging ended")
    }
}
end <- Sys.time()

time <- as.numeric((end - start), units = "secs")

print(glue::glue("Solving took {as.numeric((end - start), units = 'secs')} seconds long...!"))

# END LOGGING
sink()
sink(type = "message")
close(file)

print(glue::glue("Solving took {as.numeric((end - start), units = 'secs')} seconds long...!"))


combined_solution <- s |>
    left_join(select(grid_cell, c("id", "x", "y")), by = "id") |>
    write_csv(file.path(dirs["dir_output"],
                        glue::glue("solution_full_{solver}_{RES}km_{opt_gap}g_{opt_threads}t_",
                                   ifelse(runid == "", "default", runid),
                                   ".csv")
                                   ))

## 7.2 Convert matrix to raster ====
r <- rast(
    combined_solution[, c("x", "y", "final")],
    crs = crs(EPSG),
    extent = ext(rast_template
    )
)

writeRaster(r,
            file.path(dirs["dir_output"],
                        glue::glue("solution_{solver}_{RES}km_{opt_gap}g_{opt_threads}t_{budget}b",
                                   ifelse(runid == "", "default", runid),
                                   ".tif")
                                   ),
            overwrite = TRUE)