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

preprocess <- function(table, data_dir = "data") {
  filepath <- file.path("data", paste0(table, ".csv.gz"))
  
  if (!fs::file_exists(filepath)) {
    stop("File not found: ", filepath)
  }
  
  df <- read_csv(
    filepath,
    na = "\\N",      # converts IMDB missing values to NA
    show_col_types = FALSE
  )
  
  
  return(df)
}

# Applying function on all of the `*_sample.csv.gz`
# data_dir <- "data"
# 
# sample_paths <- fs::dir_ls(
#   data_dir,
#   regexp = "_sample\\.csv\\.gz$",
#   type = "file"
# )
# 
# for (p in sample_paths) {
#   stub <- sub("\\.csv\\.gz$", "", fs::path_file(p))  # e.g. "title_basics_sample"
#   tbl  <- preprocess(stub, data_dir = data_dir)
#   readr::write_rds(tbl, fs::path(data_dir, paste0(stub, ".rda")))
# }


files <- list.files("data", pattern = "_sample\\.csv\\.gz$", full.names = FALSE)

for (f in files) {
  name <- sub("\\.csv\\.gz$", "", f)
  
  df <- preprocess(name)
  
  write_rds(df, file.path("data", paste0(name, ".rda")))
}

# Will be used later 
# title_basics_sample <- readr::read_rds("data/title_basics_sample.rda")

# Checking if the above function and part 1 of the project works 
tb <- preprocess("title_basics_sample") 
glimpse(tb)
dim(tb)
names(tb)


# Applying function to each of .csvgz and using write_rds function. 
# This is the manual way to doing it without the loop above 
# name_basics <- preprocess("name_basics_sample")
# write_rds(name_basics, "name_basics_sample.rda")
# 
# title_basics <- preprocess("titles_basics_sample")
# write_rds(title_basics, "titles_basics_sample.rda")
# 
# title_principals <- preprocess("titles_principals_sample")
# write_rds(title_principals, "titles_principals_sample.rda")
# 
# title_ratings <- preprocess("titles_ratings_sample")
# write_rds(title_ratings, "titles_ratings_sample.rda")


                                                                                                                                                                                       