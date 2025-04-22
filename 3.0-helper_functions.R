dir_output <- function(dir_out, runid) file.path(dir_out, "output", ifelse(runid == "", "default", runid))

# Helper to load solution, WITHOUT runid in filename
load_sol1 <- function(runid, type = "tif") {
    fn <- file.path(dir_output(dir_out, runid), glue::glue("solution_full_lp_{RES}km_0.01g_1t_1b.{type}"))
    if (type == "tif") rast(fn)
    if (type == "csv") read_csv(fn)
}

# Helper to load solution, WITH runid in filename
load_sol2 <- function(runid, type = "tif", dir_out) {
    fn <- file.path(dir_output(dir_out, runid), glue::glue("solution_full_lp_{RES}km_0.01g_1t_{runid}.{type}"))
    if (type == "tif") rast(fn)
    if (type == "csv") read_csv(fn)
}

# Helper function for file names
fn_template <- function(name, extra = "", ext = ".tif") {
    return(paste0(name, "_", RES, "km_", PROJ, extra, ext))
}