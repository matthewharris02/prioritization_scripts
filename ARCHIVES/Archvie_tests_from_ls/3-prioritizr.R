require(prioritizr)
require(data.table)
require(arrow)
require(tidyverse)
require(scales)
require(rcbc)

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
    select(feature, relative_target)

ecor_targets2 <- ecoregions_targets |>
    mutate(
        sense = "=",
        .before = relative_target
    ) |>
    mutate(
        type = "relative"
    ) |>
    rename(
        target = relative_target
    )



#################%
## Features ====

features_ecor <- ecoregions_data |>
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
    sense = '=',
    target = '1',
    type = 'relative'
) |>
    mutate(target = as.numeric(target)) |>
    rbind(ecor_targets2) |>
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
print("Creating problem")
p2 <- p |>
    add_min_shortfall_objective(budget = b_cells) |>
    add_cbc_solver(
        gap = 0.01,
        threads = 8,
        verbose = TRUE
    )
end_p <- Sys.time()

print("Solving")
# Solve problem (and time it)
start_s <- Sys.time()
s <- solve(p2, run_checks = FALSE)
end_s <- Sys.time()

# Add solution (and time) to list

solutions[[i]] <- s
times_s[[i]] <- (end_s - start_s)
times_p[[i]] <- (end_p- start_p)



# END TEST


for (i in 1:length(budgets)) {
    glue::glue("= Starting budget {budgets[i]}") |> print()
    b <- budgets[i]
    b_cells <- b * sum(costs[, 2], na.rm = T)

    if (i == 1) { # Slightly different problem for first run
        start_p <- Sys.time()
        print("Creating problem")
        p2 <- p |>
            add_min_shortfall_objective(budget = b_cells) |>
            add_cbc_solver(
                gap = 0.01,
                threads = 8,
                verbose = TRUE
            )
        end_p <- Sys.time()
    } else { # Add locked-in constraints of previous solution
        # Format previous solution for locked-in constraint
        prev <- solutions[[i - 1]]
        prev_mod <- cbind(prev[,1:2], sapply(prev[,3], FUN = \(x)if_else(x == 1, TRUE, FALSE)))
        # Update problem
        start_p <- Sys.time()
        p2 <- p |>
            add_min_shortfall_objective(budget = b_cells) |>
            add_locked_in_constraints(prev_mod[[3]]) |>
            add_cbc_solver(
                gap = 0.01,
                threads = 8,
                verbose = TRUE
            )
        start_p <- Sys.time()
    }
    print("Solving")
    # Solve problem (and time it)
    start_s <- Sys.time()
    s <- solve(p2, run_checks = FALSE)
    end_s <- Sys.time()

    # Add solution (and time) to list

    solutions[[i]] <- s
    times_s[[i]] <- (end_s - start_s)
    times_p[[i]] <- (end_p- start_p)

}

# Combine solutions into one 'ranked' solution ====
combined_solution <- solutions |>
    reduce(left_join, by = "id") |>
    select(-starts_with("cost")) |>
    rowwise() |>
    mutate(
        final = sum(c_across(starts_with("solution")))
    ) |>
    select(id, final) |>
    tibble() |>
    left_join(select(grid_cell, c("id", "x", "y")), by = "id")

# Convert matrix to raster ====
r <- rast(combined_solution[,c("x", "y", "final")],
          crs = crs(EPSG),
          extent = ext(rast_template)
)

writeRaster(r, "work_in_progress/output/test_solution.tif", overwrite = T)