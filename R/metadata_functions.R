#' Create and modify metadata files content
#'
#' Create and modify the content for EMF metadata files. Internal templates can be
#' used.
#'
#' The purpose of this function is to automatically create the contents of a metadata file for
#' any EMF resource
#'
#' @section Templates
#'   Templates can be used to create ready to use content for especific EMF resources types. TODO
#'
#' @param ... Name-value pairs for metadata fields to modify or add to the metadata file
#' @param .template Template file to use
#'
#' @return Metadata content ready to copy&paste to the \code{metadata.yml} file printed in the
#' console and returned as a \code{yml} object.
#'
#' @examples
#' # TODO
#'
#' @export
use_metadata_yml <- function(..., .template = NULL) {

  # empty template
  initial_state <- list('')

  # update template if provided
  if (!is.null(.template)) {
    # get the YAML metadata from provided template
    initial_state <- .template %>%
      rmarkdown::yaml_front_matter()
  }

  # We need the lists converted to yml, add/modify fields and copy to the clipboard
  ymlthis::as_yml(initial_state) %>%
    ymlthis::yml_replace(...) %>%
    ymlthis::use_yml()
}
