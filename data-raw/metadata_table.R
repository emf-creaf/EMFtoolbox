## code to prepare `metadata_table` dataset goes here
metadata_table <- readxl::read_excel('data-raw/metadata_table.xlsx')
usethis::use_data(metadata_table, overwrite = TRUE)
