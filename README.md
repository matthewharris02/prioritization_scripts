## What are these scripts for?

## How to use these scripts

The scripts beginning with `1.x` and `2` are the main scripts to be used. The `1.x` scripts are the three scripts for pre-processing all the data and the `2` scripts are for running prioritization. `0.1-ramsar.R` contains some separate processing for ramsar sites. `0.9-helper_functions.R` contains the functions used by `1.2` and `1.3`

### Pre-processing

This stage has three scripts:

-   `1.1-OPTIONS.R` -- this sets the shared options for all the scripts in `1.2`, `1.3` and `2x` so that they aren't repeated.
-   `1.2-MAIN_PREPROCESS_master.R` -- this is the primary script for pre-processing and should run almost completely automatically apart from a couple of lines at the beginning to be edited
-   `1.3-split_by_country_optional.R` -- this is used if you want to run *prioritizr* with a separate feature for each country

### Prioritization

This stage only needs one script, but there are multiple versions for different uses:

-   `2a-prioritzr_autoExc_master.R` -- main multi-budget spatial prioritization script with easy exclusion of features.

The other three scripts are older versions and need updating, but:

-   `2b-prioritzr_single_master.R` – run prioritization for a single budget

-   `2c-prioritzr_per_country.R` – run prioritization for each country separately (not globally) using the same preprocessing as 1.x

-   `2d-prioritzr_sensitivity.R` – run a single budget prioritization multiple times to compare times and solutions (though noting that past tests have led to the same solution every time if all input is the same).

## Things to consider

-   Any file paths should **not** end in a slash (`/` or `\`) -- these are automatically sorted through `file.path()`.

## Script package dependencies

The following R packages are required:

-   *tidyverse*
-   *sf*
-   *terra*
-   *arrow*
-   *data.table*
-   *prioritizr*
-   *rcbc* **or** *lpsymphony*
-   *tictoc*
-   *glue*

## Needed updates/future features

### Things that need fixing

- in 2a for reading in nco_split ids, dont' read from dir but form ncp_list
- Ensure that the directories for output and logs are empty before writing (to catch not changing run_id)

## Enhancements

-   Ensure consistency of how we list NCPs (files, or from table)
-   Stop hard-coding the planning unit mask (inc HFP)
-   Check that everything uses the extent
-   Force checking/plotting outputs at the end