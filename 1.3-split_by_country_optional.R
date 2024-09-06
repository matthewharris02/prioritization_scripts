##%##########################################################################%##
#       Split NCPs by country (i.e., assign id to each country-feature)         #
#           V 02.08.2024    Matthew Harris and Vignesh Kamath                  #
##%##########################################################################%##

##%##########################################################################%##
# 0.1 MAKE CHANGES HERE ====
## Set working directory ====
dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
# dir_wd <- "/home/matthewh@internal.wcmc/projects_active/p09217_RestorationPotentialLayer/global2024_v2"
## Set run-id ====
runid = ""
# 0.2 SET UP ====
## Load libraries
library(arrow)
library(data.table)
library(terra)
library(tidyverse)

# Load options script
source(file.path(dir_wd, "script_tools/v3/1.1-OPTIONS.R"))

##%##########################################################################%##
# 0.3 Automatically defined variables ====
# The following variables are automatic, and use the above information
## Directory-related variables

dir_in <- file.path(dir_wd, "raw")

dir_split <- file.path(dir_proc, "split")

if (!dir.exists(dir_split)) { dir.create(dir_split) }

# 0.4 Load Information ====
start <- Sys.time()
# Read in data on variables to automatically select national vs global NCPs
variables <- read_csv(file.path(dir_in, "preprocess_info.csv"))
ncp_split <- variables |>
    filter(split == "national") |>
    filter(grepl("ncp*", var)) |>
    select(var) |>
    unlist(use.names = FALSE)

# 1.1 Load global cells ====
# Open the planning unit dataset but *don't* load all data yet
pu_vals <- open_dataset(file.path(dir_proc, "global_cells"),
                        partitioning = c("ISONUM"))

# Initialize variables
last_column_id <- 0
#ncp <- "ncp_mangroves"
# 1.2 Split by countries ====
# 'Split' by countries for each ncp and output each one
for (ncp in ncp_split) {
    pu_vals_ncp <- pu_vals |>
        select(id, ISONUM, all_of(ncp)) |>
        collect() |> # Pull data into R before applying window function
        mutate(across(all_of(ncp),
                      ~scales::rescale(.x,
                                       to = c(0,1),
                                       from = c(0, max(.x, na.rm = TRUE))
                                       )
        ))|>
        mutate(
            species = paste0(ncp, "_", ISONUM)
            ) |>
        rename_with(~c(ncp = "amount"), .cols = all_of(ncp)) |>
        select(id, species, amount) |>
        filter(!is.na(amount))

    # Create a mapping dataframe with unique numeric IDs for each column name
    mapping_ncp_id <- pu_vals_ncp |>
        distinct(species) |>
        mutate(
            column_id = row_number() + last_column_id
            )


    # Join the mapping dataframe to the long dataframe
    pu_vals_ncp_2 <- pu_vals_ncp |>
        left_join(mapping_ncp_id, by = "species") |>
        mutate(species = column_id) |>   # Replace species with the numeric column_id
        select(id, species, amount) |>    # Reorder columns
        rename(pu = id)

    # Update the last_column_id to be used in the next iteration
    last_column_id <- max(mapping_ncp_id$column_id, na.rm = TRUE)

    # Write mappping id to file for info
    write.csv(mapping_ncp_id, file.path(dir_split, paste0("mapping_", ncp, ".csv")),
              row.names = FALSE)

    # Write ncp rij matrix to file
    write_parquet(pu_vals_ncp_2, file.path(dir_split, paste0(ncp, ".parquet")))
}
end <- Sys.time()
print("Finished!")
print(end - start)
end - start