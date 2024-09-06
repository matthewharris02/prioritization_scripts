require(sf)
require(terra)
require(tidyverse)

# Load LULC not-natural layer as 'modified' land map
land <- rast(file.path(dir_out, "planning_units",
                                fn_template("cop_excludeNotNat")))
modified_mask <- land |>
    classify(cbind(0, NA))

ecoregions <- st_read(file.path(dir_in, pu_fn["ecoregions2017"]))
ecoregions_rast <- ecoregions |>
    st_transform(st_crs(EPSG)) |>
    rasterize(rast_template, field = "ECO_ID") |>
    mask(land) |>
    writeRaster(file.path(dir_out, "planning_units",
                          fn_template("ecoregions_withIceRock")),
                overwrite = TRUE)

remnant <- ecoregions_rast * modified_mask
writeRaster(remnant,
            file.path(dir_out, "planning_units",
                      fn_template("ecoregionsremnant_withIceRock")),
            overwrite = TRUE)

### Calculate number of pixels per ecoregion ====
ecor_pixels <- freq(ecoregions_rast) %>%
    as_tibble() %>%
    select(-layer) %>%
    rename(potential_extent = count)

remnant_pixels <- freq(remnant) %>%
    as_tibble() %>%
    select(-layer) %>%
    rename(realised_extent = count)


### Calculate remnant ecoregion proportions ====
remnant_table <- remnant_pixels |>
    full_join(ecor_pixels) |>
    rename(ECO_ID = value) |>
    mutate(
        remnant_proportion = realised_extent / potential_extent
    ) |>
    left_join(
        as.data.frame(ecoregions) |>
            select(ECO_ID, ECO_NAME, BIOME_NAME)
    ) |>
    select(ECO_NAME, ECO_ID, BIOME_NAME, realised_extent, potential_extent, remnant_proportion)

print("CSV WRITE...")
write_csv(remnant_table, file.path(dir_out, "planning_units", "global_ecoregions_moll.csv"))

### Create without ice and rock ====
ecor2 <- classify(ecoregions_rast, cbind(0, NA))
writeRaster(ecor2,
            file.path(dir_out, "planning_units",
                      fn_template("ecoregions_noIceRock")),
            overwrite = TRUE)

remnant2 <- classify(remnant, cbind(0, NA))
writeRaster(remnant2,
            file.path(dir_out, "planning_units",
                      fn_template("ecoregionsremnant_noIceRock")),
            overwrite = TRUE)

rm(land, modified_mask, ecoregions, ecoregions_rast, remnant, ecor2, remnant2)
