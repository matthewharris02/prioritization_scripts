require(arrow)
require(tidyverse)


open_cells <- function(fn, global = TRUE, countries = FALSE) {
    df <- open_dataset(fn, partitioning = c("countries"))
    if (global == TRUE) {
        return(df)
    }
    if (global == FALSE){
        df
    }

}