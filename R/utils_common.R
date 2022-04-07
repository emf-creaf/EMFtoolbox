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
  usethis::ui_info("Creating temporal folder at {usethis::ui_path(folder_path)}")
  fs::dir_create(folder_path)
  return(folder_path)
}

#' Clone from github
#'
#' Wrapper around \code{\link[usethis]{create_from_github}} but with a twist
#'
#' This function clone a github repo in a temporal folder and set the active
#' project (usethis project) to the cloned folder. It ensures that after exiting
#' the parent function (the calling env) the previous project (if any) is
#' reactivated again
#'
#' @param repo character with repo name
#' @param org character with organization/user name
#' @param .envir environment to look after for cleaning steps (setting old
#'   project, removing temp folders...). See \code{\link[withr]{defer}}.
#'
#' @return invisible path to the temporal cloned folder
#'
#' @noRd
clone_from_github <- function(repo, org, .envir = parent.frame()) {
  temp_proj <- emf_temp_folder()
  withr::defer(fs::dir_delete(temp_proj), envir = .envir)

  # store the current project (if any)
  this_is_a_project <- !is.null(usethis::proj_sitrep()[["active_rstudio_proj"]])
  old_project <- getwd()
  if (this_is_a_project) {
    old_project <- usethis::proj_get()
  }

  # create the repo
  usethis::create_from_github(
    repo_spec = glue::glue("{org}/{repo}"),
    destdir = temp_proj,
    fork = FALSE,
    rstudio = FALSE,
    open = FALSE
  )

  # go to the cloned folder and do whatever it needs, but always back
  # again to the original one when finish (defer)
  dir <- fs::path(temp_proj, repo)
  setwd(dir)
  usethis::proj_set(dir, force = TRUE)
  withr::defer(setwd(old_project), envir = .envir)
  # this will set the active_usethis_proj to the same as the working directory
  #   - If the user was already in a project, then wd, active_usethis_proj and active_rstudio_proj
  #     will be the same
  #   - If the user wasn't in a rstudio project, then the wd and the active_usethis_proj will
  #     be the same, but the active_rstudio_proj will be NULL
  # in debug mode, see the active_*_proj with usethis::proj_sitrep()
  withr::defer(usethis::proj_set(old_project, force = TRUE), envir = .envir)

  return(invisible(dir))
}

create_folder_backup <- function(path, .envir = parent.frame()) {
  # create a temp folder for the backup
  backup_folder <- emf_temp_folder()

  # defer the removing of the folder for when the parent envir closes (or the
  # one supplied)
  withr::defer(fs::dir_delete(backup_folder), envir = .envir)

  # copy web to the temp folder
  fs::dir_copy(path, backup_folder)

  # return the temp folder path to use it later if needed
  return(fs::path(fs::dir_ls(backup_folder)))
}
