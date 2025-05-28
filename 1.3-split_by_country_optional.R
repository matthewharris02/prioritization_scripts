##%##########################################################################%##
#       Split features by country (i.e., assign id to each country-feature)    #
##%##########################################################################%##

##%##########################################################################%##
# 0.1 MAKE CHANGES HERE ====
## Set working directory ====
dir_wd <- "/mnt/sda/restoration_opportunities"
# dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_src <- dir_wd
# dir_wd <- "/home/matthewh@internal.wcmc/projects_active/p09217_RestorationPotentialLayer/global2024_v2"
## Set run-id ====
runid <- ""

## RESOLUTION ====
# Set shared resolution for all layers
# Relative to 1km at equator (or 30 arcseconds in non-equal area projection)
RES <- 5
## Directory ID ====
#   for different solutions at the same resolution
dir_id <- ""

# 0.2 SET UP ====
## Load libraries
library(arrow)
library(data.table)
library(terra)
library(tidyverse)

# Load options script
source(file.path(dir_src, "script_tools/1.1-OPTIONS.R"))

##%##########################################################################%##
# 0.3 Automatically defined variables ====
# The following variables are automatic, and use the above information
## Directory-related variables

dir_in <- file.path(dir_wd, "raw")
dir_id <- ""
dir_out <- file.path(dir_wd, "work_in_progress",
                     paste0(RES, "km",
                         ifelse(dir_id == "", "", paste0("_", dir_id))
                     ))

dir_in <- file.path(dir_wd, "raw")

dirs <- create_info(dir_out)
dirs["dir_split"] <- file.path(dirs["dir_proc"], "split")

if (!dir.exists(dirs["dir_split"])) { dir.create(dirs["dir_split"]) }

# 0.4 Load Information ====
start <- Sys.time()
# Read in data on variables to automatically select national vs global features
variables <- read_csv(file.path(dirs["dir_out"], "preprocess_info.csv"))
ft_split <- variables |>
    filter(split == "national") |>
    filter(grepl("ft*", var)) |>
    select(var) |>
    unlist(use.names = FALSE)

# 1.1 Load global cells ====
# Open the planning unit dataset but *don't* load all data yet
pu_vals <- open_dataset(file.path(dirs["dir_proc"], "global_cells"),
                        partitioning = c("ISONUM"))

# Initialize variables
last_column_id <- 0
# 1.2 Split by countries ====
# 'Split' by countries for each feature and output each one
for (ft in ft_split) {
    pu_vals_ft <- pu_vals |>
        select(id, ISONUM, all_of(ft)) |>
        collect() |> # Pull data into R before applying window function
        mutate(across(all_of(ft),
            ~scales::rescale(.x,
                to = c(0,1),
                from = c(0, max(.x, na.rm = TRUE))
            )
        )) |>
        mutate(
            species = paste0(ft, "_", ISONUM)
        ) |>
        rename_with(~c(ft = "amount"), .cols = all_of(ft)) |>
        select(id, species, amount) |>
        filter(!is.na(amount))

    # Create a mapping dataframe with unique numeric IDs for each column name
    mapping_ft_id <- pu_vals_ft |>
        distinct(species) |>
        mutate(
            column_id = row_number() + last_column_id
        )


    # Join the mapping dataframe to the long dataframe
    pu_vals_ft_2 <- pu_vals_ft |>
        left_join(mapping_ft_id, by = "species") |>
        mutate(species = column_id) |>   # Replace species with the numeric column_id
        select(id, species, amount) |>    # Reorder columns
        rename(pu = id)

    # Update the last_column_id to be used in the next iteration
    last_column_id <- max(mapping_ft_id$column_id, na.rm = TRUE)

    # Write mappping id to file for info
    write.csv(mapping_ft_id, file.path(dirs["dir_split"], paste0("mapping_", ft, ".csv")),
              row.names = FALSE)

    # Write feature rij matrix to file
    write_parquet(pu_vals_ft_2, file.path(dirs["dir_split"], paste0(ft, ".parquet")))
}
end <- Sys.time()
print("Finished!")
print(end - start)
end - start