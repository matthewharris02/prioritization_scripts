dir_output <- function(dir_out, runid) file.path(dir_out, "output", ifelse(runid == "", "default", runid))

# Helper to load solution, WITH runid in filename
load_sol2 <- function(runid, dir_out, type = "tif") {
    fn <- file.path(dir_output(dir_out, runid), glue::glue("solution_full_lp_{RES}km_0.01g_1t_{runid}.{type}"))
    if (type == "tif") rast(fn)
    if (type == "csv") read_csv(fn)
}

# Load a partial (i.e., individual budget) solution
load_sol_partial <- function(runid, dir_out, info_str, type = "csv") {
    fn <- file.path(dir_output(dir_out, runid), glue::glue("solution_single_", info_str, ".{type}"))
    if (type == "tif") rast(fn)
    if (type == "csv") read_csv(fn)
}

# Helper function for file names
fn_template <- function(name, extra = "", ext = ".tif") {
    return(paste0(name, "_", RES, "km_", PROJ, extra, ext))
}
