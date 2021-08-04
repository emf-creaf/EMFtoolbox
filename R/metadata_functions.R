#' Create and modify metadata files content
#'
#' Create and modify the content for EMF metadata files. Internal templates can be
#' used.
#'
#' The purpose of these functions is to automatically create the contents for the metadata file of any EMF
#' resource
#'
#' @section Templates:
#'   The easiest way to create the metadata fields for any EMF resource is using the corresponding
#'   \code{use_*_yml} function. But in specials cases can be useful to provide a custom template. This
#'   template must be a \code{yml} file with the required metadata fields. Check \code{EMFtoolbox}
#'   predetermined templates, located in the package installation folder, to see examples.
#'
#' @param ... Name-value pairs for metadata fields to modify or add to the metadata file
#' @param .template Template file to use
#' @param .write Logical indicating if the \code{metadata.yml} file must be written (TRUE) or not (FALSE)
#'
#' @return Metadata content ready to copy&paste to the \code{metadata.yml} file printed in the
#' console and returned as a \code{yml} object.
#'
#' @examples
#' use_metadata_yml()
#' use_metadata_yml(emf_type = 'workflow', tags = c('dummy', 'foo'))
#' use_workflow_yml(tags = c('dummy', 'foo'))
#' use_data_yml(tags = c('bar', 'baz'))
#' use_model_yml(edges = c('bar', 'dummy'))
#'
#' @export
use_metadata_yml <- function(
  ...,
  .template = system.file('metadata_templates', 'core.yml', package = 'EMFtoolbox'),
  .write = FALSE
) {

  # empty template
  initial_state <- list('')

  # update template if provided
  if (!is.null(.template)) {
    # get the YAML metadata from provided template
    initial_state <- .template %>%
      rmarkdown::yaml_front_matter()
  }

  # We need the lists converted to yml, add/modify fields and copy to the clipboard
  metadata_yml <- ymlthis::as_yml(initial_state) %>%
    ymlthis::yml_replace(...) %>%
    suppressMessages(ymlthis::use_yml())

  # if write is TRUE, write it and return it, if not, just return it
  if (isTRUE(.write)) {
    ymlthis::use_yml_file(metadata_yml, 'metadata.yml', build_ignore = TRUE)
  }

  return(metadata_yml)
}

#' @describeIn use_metadata_yml Equivalent to using \code{use_metadata_yml} with
#'   \code{.template = system.file('metadata_templates', 'workflow.yml', package = 'EMFtoolbox')}
#'
#' @export
use_workflow_yml <- function(..., .write = FALSE) {
  use_metadata_yml(
    ...,
    .template = system.file('metadata_templates', 'workflow.yml', package = 'EMFtoolbox'),
    .write = .write
  )
}

#' @describeIn use_metadata_yml Equivalent to using \code{use_metadata_yml} with
#'   \code{.template = system.file('metadata_templates', 'tech_doc.yml', package = 'EMFtoolbox')}
#'
#' @export
use_techdoc_yml <- function(..., .write = FALSE) {
  use_metadata_yml(
    ...,
    .template = system.file('metadata_templates', 'tech_doc.yml', package = 'EMFtoolbox'),
    .write = .write
  )
}

#' @describeIn use_metadata_yml Equivalent to using \code{use_metadata_yml} with
#'   \code{.template = system.file('metadata_templates', 'data.yml', package = 'EMFtoolbox')}
#'
#' @export
use_data_yml <- function(..., .write = FALSE) {
  use_metadata_yml(
    ...,
    .template = system.file('metadata_templates', 'data.yml', package = 'EMFtoolbox'),
    .write = .write
  )
}

#' @describeIn use_metadata_yml Equivalent to using \code{use_metadata_yml} with
#'   \code{.template = system.file('metadata_templates', 'model.yml', package = 'EMFtoolbox')}
#'
#' @export
use_model_yml <- function(..., .write = FALSE) {
  use_metadata_yml(
    ...,
    .template = system.file('metadata_templates', 'model.yml', package = 'EMFtoolbox'),
    .write = .write
  )
}

#' @describeIn use_metadata_yml Equivalent to using \code{use_metadata_yml} with
#'   \code{.template = system.file('metadata_templates', 'softwork.yml', package = 'EMFtoolbox')}
#'
#' @export
use_softwork_yml <- function(..., .write = FALSE) {
  use_metadata_yml(
    ...,
    .template = system.file('metadata_templates', 'softwork.yml', package = 'EMFtoolbox'),
    .write = .write
  )
}







