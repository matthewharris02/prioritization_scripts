# 0. Load libraries ====
print("Loading libraries")
library(data.table)
library(arrow)
library(tidyverse)
library(scales)
library(terra)

# 1. OPTIONS and set-up ====
## 1.1 EDITABLE options ====
dir_wd <- "/mnt/sda/MH_restoration"
# dir_wd <- "O:/f01_projects_active/Global/p09217_RestorationPotentialLayer/global2024_v2"
dir_src <- dir_wd

### Solution-related options ====
auto_dir <- TRUE      # Automatically create needed directories?
runid <- ""           # Additional ID to distinguish runs

## 1.2 Shared options ====
# Load options file to share options with pre-processing
source(file.path(dir_src, "script_tools/1.1-OPTIONS_20.R"))
source(file.path(dir_src, "script_tools/3.0-helper_functions.R"))

## 1.4 Directory-related variables ====
dir_analyze <- file.path(dir_out, "analysis", "compare")

# 2. ANALYSIS ====
# LOAD SOLUTIONS ====
base <- rast(file.path(dir_output("default"), "solution_full_lp_20km_0.01g_1t_1b.tif"))
noCarbon <- rast(file.path(dir_output("noCarbon"), "solution_full_lp_20km_0.01g_1t_1b.tif"))
noEco <- rast(file.path(dir_output("noEco"), "solution_full_lp_20km_0.01g_1t_1b.tif"))
noUsefulPlants <- rast(file.path(dir_output("noUsefulPlants"), "solution_full_lp_20km_0.01g_1t_1b.tif"))

## Cross tabulate so see how many differences ====
combined <- c(c(base, noCarbon, noEco, noUsefulPlants))
names(combined) <- c("default", "noCarbon", "noEco", "noUsefulPlants")

compare_carbon <- crosstab(subset(combined, 1:2), long = TRUE)
compare_eco <- crosstab(subset(combined, c(1, 3)), long = TRUE)
compare_eco <- crosstab(subset(combined, c(1, 3)))
compare_up <- crosstab(subset(combined, c(1, 4)), long = TRUE)

compare_all <- left_join(compare_carbon, compare_eco,
                         by = join_by("default" == default, "noCarbon" == "noEco"),
                         suffix = c(".noCarbon", ".noEco"))

## Top 20% comparison
top20 <- function(r){
    r |> classify(
        data.frame(
            from = c(1, 3),
            to = c(2, 10),
            becomes = c(1, 0)
        ),
        right = NA
)
}

base_20 <- top20(base)
carbon_20 <- top20(noCarbon)

compare <- base_20 + carbon_20
writeRaster(compare, file.path(dir_analyze, "compare_top20_carbon.tif"))


## Subtract layers ====
sol_default <- load_sol1("default")
sol_noEco <- load_sol1("noEco")
sol_newEco <- load_sol2("newEcor")

dif_default_noEco <- sol_default - sol_noEco
dif_noEco_newEco <- sol_noEco - sol_newEco

writeRaster(dif_default_noEco, file.path(dir_analyze, "dif_default_noEco.tif"))
writeRaster(dif_noEco_newEco, file.path(dir_analyze, "dif_noEco_newEco.tif"))



dif_default_noEco2 <- sol_default*100 + sol_noEco
