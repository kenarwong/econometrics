library(readr)
r_File <- paste(getwd(), "CORESTICKM159SFRBATL.csv", sep = "/")

### read_csv, for large files
CSV <- read_csv(r_File, col_names = TRUE, show_col_types = FALSE)
rows <- nrow(CSV)
print(rows)

### read.csv, slower standard method 
CSV <-read.csv(r_File, header = TRUE)
rows <- nrow(CSV)
print(rows)