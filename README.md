## What are these scripts for?

## How to use these scripts

The scripts beginning with `1.x`, `2` and `3` are the main scripts to be used. The `1.x` scripts are the three scripts for pre-processing all the data and the `2` scripts are for running prioritization. The `3` scripts are for analyzing the outputs.

`0.1-ramsar.R` contains some separate processing for ramsar sites. `0.9-helper_functions.R` contains the functions used by `1.2` and `1.3`

See below for basic descriptions of all the scripts, particularly the analysis ones.

## Pre-processing

This stage has three scripts:

- `1.1-OPTIONS.R` -- this sets the shared options for all the scripts in `1.2`, `1.3` and `2x` so that they aren't repeated.
- `1.2-MAIN_PREPROCESS_master.R` -- this is the primary script for pre-processing and should run almost completely automatically apart from a couple of lines at the beginning to be edited
- `1.3-split_by_country_optional.R` -- this is used if you want to run *prioritizr* with a separate feature for each country

## Prioritization

This stage only needs one script, but there are multiple versions for different uses:

- `2a-prioritzr_autoExc_master.R` -- main multi-budget spatial prioritization script with easy exclusion of features.

- `2b-prioritzr_single_master.R` – run prioritization for a single budget

### Archived scripts that need to be updated

- `2c-prioritzr_per_country.R` – run prioritization for each country separately (not globally) using the same preprocessing as 1.x

- `2d-prioritzr_sensitivity.R` – run a single budget prioritization multiple times to compare times and solutions (though noting that past tests have led to the same solution every time if all input is the same).

## Analysis

There are a number of scripts to aid in different parts of analyzing the outputs of the pre-processing and the solutions. They rely on `1.1-OPTIONS.R` and `3.0-helper_functions.R`.

**General analysis**:
- `3-exclusion_area.R` - create a raster with the reason for exclusion for the exclusion raster.
- `3-num_features.R` - create raster with the number of features that are available in each point
- `3-stats.R` - create some stats for restorable land
- `3-explore_processed.R` - compare the pre-processed outputs from different pre-processing

**Solution analysis**
- `3-es_values.R` - calculate and plot cumulative amount of ecosystem service by rank
- `3-sample.R` - take a random sample of the solution and run correlation and regression
- `3-explore_incomplete_solution.R` - combine and rasterize partial solutions (i.e., not all budgets) for analysis 
- `3-compare_runs.R` - compare solutions from different types of runs (i.e., different runids)

**Other**:
- `3.0-helper_functions.R` - a collection of helper functions for the analysis


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

- Ensure that the directories for output and logs are empty before writing (to catch not changing run_id)

## Enhancements

-   Check that everything uses the extent
-   Force checking/plotting outputs at the end