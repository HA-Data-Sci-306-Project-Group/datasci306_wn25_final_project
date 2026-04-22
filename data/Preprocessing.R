# Write a function that will load a table from the IMDb files in the `data/` directory.
# The function should take the file name (without the ".csv.gz" portion) as an argument
# The function should load the appropriate `.csv.gz` file.
# Make sure that all "\\N" values (which IMDB uses to indicate missing values) are 
# turned into proper NA values in R
# The function should return the table.

# For each of the `.csv.gz` files, use your function to load the table, 
# then save it into a variable (e.g. `name_basics <- preprocess("name_basics")`) 
# and use the `write_rds` function (e.g., `write_rds(name_basics, "name_basics.rda")`.
#* Run the function on all of the `*_sample.csv.gz` files to created processed `.rda` files.
#* In your other files, you can load these using the `TABLE <- read_rds("data/FILENAME.rda")` function. 
#* 

library(readr)
library(dplyr)

preprocess <- function(table, data_dir = "data") {
  filepath <- file.path(data_dir, paste0(table, ".csv.gz"))
  
  if (!file.exists(filepath)) {
    stop("File not found: ", filepath)
  }
  
  read_csv(filepath, na = "\\N", show_col_types = FALSE)
}

files <- list.files("data", pattern = "_sample\\.csv\\.gz$", full.names = FALSE)

for (f in files) {
  name <- sub("\\.csv\\.gz$", "", f)
  df <- preprocess(name, data_dir = "data")
  write_rds(df, file.path("data", paste0(name, ".rda")))
}

# Will be used later 
# title_basics_sample <- readr::read_rds("data/title_basics_sample.rda")


# Checking if the above function and part 1 of the project works. 
# We can delete this before submitting 

# Checks with this one file
#tb <- preprocess("title_basics_sample") 
#glimpse(tb)
#dim(tb)
#names(tb)

# count NAs per column
#colSums(is.na(tb))

# look for any remaining literal "\N" in character columns
#char_cols <- tb |> select(where(is.character))
#any(grepl("^\\\\N$", unlist(char_cols), fixed = FALSE), na.rm = TRUE)

#Looks in the rows 
#tb |> filter(if_any(everything(), is.na)) |> slice_head(n = 10)







                                                                                                                                                                                       