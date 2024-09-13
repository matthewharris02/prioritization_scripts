# NOTE: this is an older script and needs more work to align with 2a
# 0. Load libraries ====
require(prioritizr)
require(data.table)
require(arrow)
require(tidyverse)
require(scales)
require(rcbc)

# 1. OPTIONS and set-up ====
## 1.1 EDITABLE options ====
dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"### Prioritzr-related options ====
write_each = TRUE    # If TRUE, writes solution for each budget
solver = "lp"        # Which solver: cbc, (lp)symphony
opt_gap = 0.01       # Choose gap for solver
opt_threads = 1      # Choose number of threads (ONLY for CBC solver)

### Solution-related options ====
auto_dir = TRUE      # Automatically create needed directories?
runid = ""           # Additional ID to distinguish runs
split = TRUE         # Include NCPs split by country?
opt_ecoregions = TRUE # Include ecoregions?
# drop_features: Select which features to drop
#   !! each ones should be a string
#   !! To include all/exclude none leave empty `c()` or as `NULL`
drop_feature =  c("ncp_usefulplants")

## 1.2 Shared options ====
# Load options file to share options with pre-processing
source(file.path(dir_wd, "script_tools/v3/1.1-OPTIONS.R"))

# 1.3 Load package for solver ====
if (solver == "cbc") {
    library(rcbc)
} else if (solver == "lp") {
    library(lpsymphony)
}

## 1.4 Directory-related variables ====
dir_in <- file.path(dir_wd, "raw")
dir_output <- file.path(dir_out, "output", ifelse(runid == "", "default", runid))
dir_logs <- file.path(dir_out, "logs", ifelse(runid == "", "default",runid))

if (auto_dir) {
    c(dir_output, dir_logs) |>
        walk(\(x) if(!dir.exists(x)) { dir.create(x, recursive = TRUE)})
}


## 1.5 Template raster ====
rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)


## 1.6 Load NCP variable information ====
exclude_feature <- str_flatten(drop_feature, "|") # Create regex string to exclude vars
if (is.null(drop_feature)) {exclude_feature <- "^$"} # Work-around for matching nothing so that if drop_features is empty, it selects them all

variables <- read_csv(file.path(dir_in, "preprocess_info.csv")) |>
    filter(grepl("ncp*", var)) |> # select only ncp_ variables
    filter(!grepl(exclude_feature, var)) # exclude the variables in drop_feature

# List of all ncp features for loading
ncp_all <- unlist(select(variables, var), use.names = FALSE)

# List of ncps to be split nationally or left globally for selecting later
# ncp_national <- variables |>
#     filter(split == "national") |>
#     select(var) |>
#     unlist(use.names = FALSE)
# ncp_global <- variables |>
#     filter(split == "global") |>
#     select(var) |>
#     unlist(use.names = FALSE)



# 2. PREPARE GRID CELLS ====
print("Preparing global grid cells...")

# Create variable for whether to include ecoregions
# If opt_ecoregions is FALSE, it is set to empty string
# any_of() will then ignore it and select will also ignore it
# so the column won't be loaded in
col_ecoregions <- ""
if (opt_ecoregions) {col_ecoregions <- "ecoregions"}

grid_cell_all <- open_dataset(file.path(dir_out, "processed", "global_cells"),
                          partitioning = c("ISONUM")) |>
    mutate(
        across(.cols = c(id, x, y, ecoregions), ~as.integer(.x)),
        across(
            .cols = c(ncp_kba, ncp_ramsar, ncp_saltmarshes),
            .fns = ~ifelse(is.na(.x), 0, .x)
        ),
    ) |>
    select(-lulc, -hfp)

print("Create list of countries...")
countries <- grid_cell |>
    select(ISONUM) |>
    collect() |>
    unique() |>
    unlist(use.names = FALSE)


cn_solutions <- list()

print("Start country-by-country...")
for (country in countries) {
    print(str_glue("Preparing country {country}..."))
    grid_cell_country <- grid_cell_all |>
        filter(ISONUM == country) |>
        collect() |>
        mutate(
            cost = 1,
            across(
                .cols = starts_with("ncp"),
                .fns = ~scales::rescale(.x,
                                        to = c(0, 1),
                                        from = c(0, max(.x, na.rm = TRUE)))
            )
        )


    # FEATURES
    exclude_feature <- str_flatten(drop_feature, "|")
    # Work-around for matching nothing so that if drop_features is empty, 
    #   it selects them all
    if (is.null(drop_feature)) {exclude_feature <- "^$"}
    feat_master <- data.frame(name = NULL, species = NULL)

    # 3.2 Helper: add_feat() ====
    # Helper function to add feature
    add_feat <- function(feat, feat_master) {
        row <- data.frame(
            name = feat,
            species = max(feat_master$species, na.rm = TRUE) + 1
        )
        feat_master <- bind_rows(feat_master, row)
    }

    ## 3.3 Add features ====
    # Add feature for each ncp
    for (feat in ncp_all) {
        feat_master <- add_feat(feat, feat_master)
    }

    ## 3.5 List of feature ids
    feat_ids <- deframe(feat_master)


    # 4. RIJ ====
    rij_all <- data.frame(pu = NULL, species = NULL, amount = NULL)

    for (ncp in ncp_all) {
        pu_vals_ncp <- grid_cell_country |>
            select(id, all_of(ncp)) |>
            mutate(species = feat_ids[ncp]) |>
            rename_with(~c(ncp = "amount"), .cols = all_of(ncp)) |>
            select(id, species, amount) |>
            filter(!is.na(amount)) |>
            rename("pu" = id)
        rij_all <- bind_rows(rij_all, pu_vals_ncp)
    }

    # 5. Targets ====
    targets <- tibble(
        feature = feat_ncp$species,
        relative_target = "1",
    ) |>
        mutate(relative_target = as.numeric(relative_target))

    targets <- targets |>
        select(relative_target) |>
        as.matrix()

   


    # 6. Problem and solution ====
    ## 6.1 Costs ====
    costs <- grid_cell_country |>
        select(id, cost)
    features <- rename(feat_master, "id" = species)

    ## 6.2 Base problem ====
    # Create base problem
    print("Creating base problem...")
    p <- problem(
            x = costs,
            features = features,
            cost_column = "cost",
            rij = rij) |>
        add_relative_targets(targets)




    budgets <- seq(0.05, 1, 0.05)

    solutions <- list()
    times <- list() # Problem solving times

    solution_details <- data.frame()

    print("Starting each budget")
    for (i in 1:length(budgets)) {
        ## TODO: LOGGING
        glue::glue("= Starting budget {budgets[i]}") |> print()
        b <- budgets[i]
        b_cells <- b * sum(costs[, 2], na.rm = T)

        if (i == 1) { # Slightly different problem for first run
            p2 <- p |>
                add_min_shortfall_objective(budget = b_cells)
        } else { # Add locked-in constraints of previous solution
            # Format previous solution for locked-in constraint
            prev <- solutions[[i - 1]]
            prev_mod <- cbind(prev[, 1:2],
                              sapply(prev[, 3],
                                     FUN = \(x) if_else(x == 1, TRUE, FALSE)))
            # Update problem
            p2 <- p |>
                add_min_shortfall_objective(budget = b_cells) |>
                add_locked_in_constraints(prev_mod[[3]])                
        }

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
        # Also prepare basic solution info just-in-case
        details_list <- c("objective", "runtime", "status", "gap") |>
            map(~attr(s, .x)) |>
            unlist() |>
            append(c(times[[i]], budgets[[i]]))

        solution_details <- rbind(solution_details, details_list)
        print(glue::glue("Solving took {as.numeric((end - start), units = 'secs')} seconds long...!"))

        # TODO: IF WRITE_EACH == TRUE
    }

    combined_solution_cn <- solutions |>
        reduce(left_join, by = "id") |>
        select(-starts_with("cost")) |>
        rowwise() |>
        mutate(
            final = sum(c_across(starts_with("solution")))
        ) |>
        select(id, final)

    # TODO: WRITE EACH FOR COUNTRY 
    cn_solutions[[country]] <- combined_solution_cn
}
# Combine solutions into one 'ranked' solution ====

global_sol <- cn_solutions |>
    reduce(rbind) |>
    select(id, final) |>
    tibble() |>
    left_join(select(grid_cell, c("id", "x", "y")), by = "id") |>
    write_csv(file.path(dir_output, str_glue("solution_full_", info_str, ".csv")))

# Convert matrix to raster ====
r <- rast(global_sol[, c("x", "y", "final")],
          crs = crs(EPSG),
          extent = ext(rast_template)
)

writeRaster(r, "work_in_progress/output/test_solution.tif", overwrite = T)

times_df <- write.csv(solution_details,
                      file.path(dir_logs, glue::glue("details_", info_str, ".csv")))