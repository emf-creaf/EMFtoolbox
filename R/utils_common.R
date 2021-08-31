#' Create temporal folder with unique name
#'
#' Allows for multiple temporal folders to be created in the same session
#'
#' The temporal folder is always the same for an R session, and that can generates problems when using
#' functions in tests and actions when deferring the temporal folder removal. To solve this we create temporal
#' folders with the tempfile name, create the folder and return the path
#'
#' @return path to the recently created temporal folder
#'
#' @examples
#' temp_folder <- emf_temp_folder()
#' fs::dir_exists(temp_folder)
#'
#' @export
emf_temp_folder <- function() {
  folder_path <- tempfile(pattern = 'emf')
  usethis::ui_info("Creating temporal folder at {folder_path}")
  fs::dir_create(folder_path)
  return(folder_path)
}

#' Parsing "pq__text" columns to flattened character vectors.
#'
#' Parsed the defined format to obtain the array columns from postgres
#'
#' We have defined a format: \code{\\{element1,element2,...,elementn\\}} to obtain the array postgres values
#' when importing to R tibbles. This functions parses this and transform it to a vector to use in yamls or
#' others.
#'
#' @param pq__text character string as obtained from public_* metadata db tables
#'
#' @return A character vector with the elements of the metadata field
#'
#' @examples
#' pq__text_to_vector_parser("{dummy,workflow,larara}")
#'
#' @export
pq__text_to_vector_parser <- function(pq__text) {
  stringr::str_remove_all(pq__text, '[{}]') %>%
    stringr::str_split(',') %>%
    purrr::flatten_chr()
}