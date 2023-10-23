#' Use EMF github actions
#'
#' Create the file needed to use the desired EMF GA
#'
#' This function allows to select an EMF github action template (inst/ga_templates) to use in the resource
#' project.
#'
#' @param .template Template file name (with extension) to use. If a GA exists with the used template, it will
#'  be overwritten.
#'
#' @return invisible TRUE if success
#'
#' @examples
#' use_emf_ga('collect_metadata.yml')
#'
#' @export
use_emf_ga <- function(.template) {
  # build the path and create the directory if needed
  save_as <- fs::path(".github", "workflows", .template)
  create_directory(fs::path_dir(usethis::proj_path(save_as)))

  # copy the contents
  template_path <- system.file(fs::path('ga_templates', .template), package = 'EMFtoolbox')

  if (template_path == '') {
    cli::cli_abort(
      "Template not found at {system.file(fs::path('ga_templates'), package = 'EMFtoolbox')}. Check available templates at that location."
    )
    # usethis::ui_stop("Template not found at {system.file(fs::path('ga_templates'), package = 'EMFtoolbox')}. Check available templates at that location.")
  }

  ga_template <- base::readLines(template_path, encoding = "UTF-8", warn = FALSE)
  usethis::write_over(save_as, ga_template)
  cli::cli_alert_success("Created {.file {(.template)}} at {.path {save_as}}")
  # usethis::ui_done("Created {.template} at {usethis::ui_path(save_as)}")
  return(invisible(TRUE))
}

# create_directory function is taken from usethis:::create_directory
create_directory <- function(path) {

  if (fs::dir_exists(path)) {
    return(invisible(FALSE))
  }
  else if (fs::file_exists(path)) {
    cli::cli_abort("{.path {path}} exists but is not a directory.")
    # usethis::ui_stop("{usethis::ui_path(path)} exists but is not a directory.")
  }
  fs::dir_create(path, recurse = TRUE)
  cli::cli_alert_success("Creating {.path {path}}")
  # usethis::ui_done("Creating {usethis::ui_path(path)}")
  invisible(TRUE)
}
