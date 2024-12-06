Last updated on: 06/12/2024 Authors: Matthew Harris and Vignesh Kamath

# 06/12/2024

- Rename all references to `ncp` to `ft` or `feature` to be clearer, more general (and more accurate). Only done for `1.x` and `2a`
- Change method for human footprint: use gdal_calc instead of `terra::classify` and reclassify before resample

# 13/09/2024

- Major changes to `2c` to make sure that it is updated to follow the `2a` structure. Not finalised or tested.

# 06/09/2024

- Fixing bug related to ncp_split in `2a` where script failed if excluding a 'split' ncp

# 30/08/2024

-   Major updates from early August (old scripts saved in Archives):

    -   Remove multiple smaller scripts to make it simpler so there is now just one main preprocessing script that calls on three functions, rather than having function for small sections that weren't repeated.

    -   Multi-budget prioritization script updated to allow easy exclusion of features.

    -   Re-naming of all files to make order more obvious and consistent.

    -   Added sub-directories within `logs` and `outputs` for run ids so that solutions aren't over-written.

-   Important note: 2b-2d have not had the above updates so don't work as of this point.