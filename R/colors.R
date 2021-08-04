#' Return EMF resources color codes
#'
#' Get the color hex codes for the different EMF resources
#'
#' Each EMF resource has a color assigned, this function returns the codes
#'
#' @param resource Resource name from which get the color hex code.
#'
#' @return If \code{resource} is NULL (default), a tibble with two columns, resource_name and hex_color_code.
#'   If \code{resource} is not NULL, function returns directly the hex color code for the resource specified.
#'
#' @examples
#' resource_color()
#' resource_color('model')
#'
#' @export
resource_color <- function(resource = NULL) {

  resources_color_tibble <- tibble::tibble(
    resource_name = c('workflow', 'tech_doc', 'model', 'data', 'softwork'),
    hex_color_code = c('#D5E8D4', '#97D077', '#FFFFCC', '#C3ABD0', '#FFCC99')
  )

  if (is.null(resource)) {
    return(resources_color_tibble)
  }

  resources_color_tibble %>%
    dplyr::filter(resource_name == resource) %>%
    dplyr::pull(hex_color_code) %>%
    purrr::flatten_chr()

}
