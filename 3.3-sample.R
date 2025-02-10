library(data.table)
library(terra)
library(arrow)
library(tidyverse)
library(sf)
library(nlme)

dir_wd <- "/mnt/sda/MH_restoration"
dir_src <- dir_wd
runid <- ""           # Additional ID to distinguish runs
source(file.path(dir_src, "script_tools/1.1-OPTIONS.R"))
dir_output <- file.path(dir_out, "output", ifelse(runid == "", "default", runid))

rast_template <- rast(
    crs = crs(EPSG),
    res = c(1000 * RES, 1000 * RES),
    ext = ext(EXT)
)



grid_cell <- open_dataset(file.path(dir_proc, "global_cells"),
                          partitioning = c("ISONUM"))  |>
    select(-id) |>
    collect()

solution <- read_csv(file.path(dir_output, "solution_full_lp_5km_0.01g_1t_1b.csv"))

all <- left_join(grid_cell, solution, join_by(x == x, y == y))

#############
# Transform features to 0-1 to match problem
all_trans <- all |>
    mutate(
        across(any_of(c("id", "x", "y", "ecoregions")),
               ~as.integer(.x)),
        across(any_of(c("ft_kba", "ft_ramsar", "ft_saltmarshes")), # Use any_of just in case one of these is excluded
               ~ifelse(is.na(.x), 0, .x)),
        across(
            .cols = starts_with("ft"),
            .fns = ~scales::rescale(.x,
                                    to = c(0, 1),
                                    from = c(0, max(.x, na.rm = TRUE)))
        ),
        across(any_of(c("ft_pollination", "ft_mangroves", "ft_coastal")),
               ~ifelse(is.na(.x), 0, .x))
    )

# Randomly select 10000 cells out of total
random <- sample(dim(all)[[1]], 10000)

selected <- all[random]
selected_trans <- all_trans[random]


# PLOTS
plot(selected$final, selected$ft_coastal)
plot(selected_trans$final, selected_trans$ft_carbon)


# CORRELATION

library(corrplot)

a <- selected_trans |>
    select(ft_carbon:final, -id, -ISONUM) |> #, -ft_coastal, -ft_mangroves, -ft_saltmarshes, -ISONUM) |>
    cor(use = 'complete.obs')

corrplot(a)

cor.test(selected$final, selected$ft_coastal)


# REGRESSION
model <- lm(final ~ ft_carbon +
                ft_coastal +
                ft_iucnrichness +
                ft_saltmarshes +
                # ft_kba +
                # ft_ramsar +
                # ft_waterquality +
                # ft_pollination +
                ft_usefulplants,
            data = selected)

summary(model)

sel_forModel <- select(selected_trans, -c(id, ISONUM, x, y, lulc_converted, lulc_other, pu, ecoregions))
base_model <- lm(final ~ ., data = sel_forModel)
forward_model <- step(base_model, direction = "forward")
