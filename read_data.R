##Your first exercise is to read in the data for all the years from 1985 to 2023. As discussedin class, you don’t want to do this manually and will need to figure out a way to do itprogrammatically. We’ve given you a skeleton of how to do this for data for one year below.Your task is to adapt this to reading in multiple datasets from all the years in question. Thisexample code is meant to be a guide and if you think of a better way to read the data in, go for it
library(data.table)
read_buoy_data <- function(year) {
  file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
  tail <- ".txt.gz&dir=data/historical/stdmet/"
  path <- paste0(file_root, year, tail)
  header <- tryCatch(scan(path, what = 'character', nlines = 1), error = function(e) NULL)
  if (is.null(header)) return(NULL)
  skip_value <- ifelse(year < 2007, 1, 2)
  buoy <- fread(path, header = FALSE, skip = skip_value, fill = TRUE)
  buoy[, Year := year]
  if (year == 2000) {
    buoy <- cbind(buoy, NA)
    header <- c(header, "new_column") 
  }
  if (ncol(buoy) < length(header)) {
    missing_cols <- length(header) - ncol(buoy)
    buoy <- cbind(buoy, matrix(NA, nrow = nrow(buoy), ncol = missing_cols))
  }
  colnames(buoy) <- c(header, "Year")[1:ncol(buoy)]
  
  return(buoy)
}
years <- 1985:2023
buoy_data_list <- lapply(years, read_buoy_data)
all_buoy_data_2 <- rbindlist(buoy_data_list, use.names = TRUE, fill = TRUE)