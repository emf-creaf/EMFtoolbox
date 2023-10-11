# Web updating ------------------------------------------------------------

#' Update web repository
#'
#' Update web repository with new/modified content
#'
#' This function clone the latest commit from the remote web repository (GitHub)
#' and render all pages that need update (\code{\link{render_resource_pages}}).
#' After that if any changes have been made, it updates the web repository with
#' the changes. After updating the repository, production server is updated.
#'
#' @param repo character with repo name
#' @param org character with organization/user name
#' @param commit_message Character with the commit message
#' @param github_pat Character wiht the remote token (GitHub)
#' @param remote logical indicating if executing from outside prod server,
#'   default to TRUE
#' @param .con pool::pool object with the metadata database connection info
#' @param .force logical indicating if the render shoud be forced even if
#'   everything is up-to-date
#' @param .dry_push logical to pass to  \code{\link{commit_push_repo}}
#'
#' @return invisible FALSE if no changes are to be commited and prod is not
#'   updated (unless \code{.force = TRUE}). TRUE if everything goes correctly.
#'
#' @export
update_emf_web <- function(
    repo = "emf_web", org = "emf-creaf",
    commit_message = glue::glue("{Sys.time()} automatic update"),
    github_pat = Sys.getenv("GITHUB_PAT"),
    remote = TRUE,
    prod_folder = Sys.getenv("PROD_FOLDER"),
    prod_host = Sys.getenv("PROD_HOST"),
    prod_pass = Sys.getenv("PROD_PASS"),
    .con = NULL, .force = FALSE, .dry_push = FALSE
) {
  # pretty print
  usethis::ui_line()
  usethis::ui_info("Updating EMF web")
  usethis::ui_line("-----")

  # Clone the web repo
  repo_web_dir <- clone_from_github(repo = repo, org = org)

  # Render all pages
  # pretty print
  usethis::ui_line()
  usethis::ui_info("Render pages")
  usethis::ui_line("-----")
  rendered_pages <- render_resource_pages(
    .con = .con, .force = .force, .web_path = repo_web_dir
  )
  rendered_summary(rendered_pages)

  # pretty print
  usethis::ui_line()
  usethis::ui_info("Commit and push EMF web repository ({org}/{repo})")
  usethis::ui_line("-----")
  pushed <- commit_push_repo(commit_message, github_pat, .dry_push)

  if (!pushed) {
    usethis::ui_info(
      "No changes detected, web repository ({org}/{repo}) up-to-date"
    )
    # if no force, exit gracefully
    if (!.force) {
      usethis::ui_done("EMF web up-to-date, not updating and exiting.")
      return(invisible(FALSE))
    }
  }

  # pretty print
  usethis::ui_line()
  usethis::ui_info("Copy to production")
  usethis::ui_line("-----")

  # if remote, connect to the server by ssh and execute the command in the
  # server. If not remote, execute the function here
  prod_output <- NA

  if (remote) {

    usethis::ui_info("Connecting to remote server at {prod_host}")
    # connect to the server
    prod_session <- ssh::ssh_connect(host = prod_host, passwd = prod_pass)
    withr::defer(ssh::ssh_disconnect(prod_session))

    # execute command
    prod_output <- ssh::ssh_exec_internal(
      prod_session,
      glue::glue("R -e 'EMFtoolbox::copy_emf_web(dest = \"{prod_folder}\")'")
    )

    cat(rawToChar(prod_output$stderr))

  } else {
    usethis::ui_info("Copying locally to {prod_folder}")
    prod_output <- copy_emf_web(dest = prod_folder)
  }

  if (is_na_or_null(prod_output)) {
    usethis::ui_oops("Something went wrong, check the outputs")
    return(invisible(FALSE))
  }

  usethis::ui_done("Web in production updated!!!")
  return(invisible(TRUE))
}


# metadata collection GA remote repos update ------------------------------


#' Update metadata collection GA for all resources
#'
#' Update metadata collection GA for all resources
#'
#' This function takes all resources repositories and update the collecing
#' metadata GA with the newer version from \code{\link{EMFtoolbox}}. Each
#' resource is served with the corresponding metadata GA file (normal for
#' all resources except for models and data, external or creaf, that
#' receives the corresponding GA for the type).
#'
#' @param .con metadata database connection to retrieve resources info
#' @param .dry_push logical indicating if commit changes must be hold from pushing
#' @param .github_pat GitHub PAT
#'
#' @return Named list with the resources and the update status
#'
#' @export
update_metadata_collection_ga <- function(
    .con = NULL,
    .dry_push = TRUE,
    .github_path = Sys.getenv("GITHUB_PAT")
) {

  # if .con is null, get one with env vars
  if (is.null(.con)) {
    .con <- metadata_db_con()
  }

  # get the dataframe with the repos info
  resources_info <- resource_repositories_info(.con)
  updated <- resources_info %>%
    dplyr::mutate(.dry_push = .dry_push, .github_path = .github_path) %>%
    purrr::pmap(update_repo_ga) %>%
    magrittr::set_names(., resources_info$repo)

  return(updated)
}


#' Update GA in remote repository
#'
#' Remove and update GA, pushing to the remote repository
#'
#' This function clone the specified repository, update the specified GA action
#' and pushes the changes to the specified repository.
#'
#' @param repo character with repo name
#' @param org character with org name
#' @param ga_action character with ga_action to update
#' @param .github_pat GitHub PAT
#' @param .dry_push logical indicating if the push should be done
#'
#' @return A list with the \code{repositories} and their status
#'
#' @noRd
update_repo_ga <- function(
    repo, org, ga_action,
    .github_path = Sys.getenv("GITHUB_PAT"),
    .dry_push = TRUE
) {

  # clone the repository
  clone_from_github(repo, org)

  # update ga_action
  ga_path <- fs::path(".github", "workflows", ga_action)
  if (fs::file_exists(ga_path)) {
    fs::file_delete(ga_path)
  }
  use_emf_ga(ga_action)

  # commit and push remote
  res <- commit_push_repo(
    glue::glue(
      "Update GA with the latest version [{packageVersion('EMFtoolbox')}] ({Sys.time()})"
    ),
    .github_pat,
    .dry_push
  )

  return(res)

}

#' Resources repositories info
#'
#' Data frame with resource repositories info
#'
#' @param .con conection to metadata database
#'
#' @return A data frame with repo info
#'
#' @noRd
resource_repositories_info <- function(.con = NULL) {

  workflows_repo <- public_workflows(.con = .con) %>%
    dplyr::pull(workflow)
  workflows_org <- rep("emf-creaf", length(workflows_repo))
  tech_docs_repo <- public_tech_docs(.con = .con) %>%
    dplyr::pull(tech_doc)
  tech_docs_org <- rep("emf-creaf", length(tech_docs_repo))

  softworks_info <- public_softworks(.con = .con) %>%
    dplyr::mutate(
      repo = dplyr::if_else(
        !is.na(url_source),
        url_source %>%
          stringr::str_split(pattern = '/', simplify = TRUE) %>%
          magrittr::extract(,5),
        softwork
      ),
      org = dplyr::if_else(
        !is.na(url_source),
        url_source %>%
          stringr::str_split(pattern = '/', simplify = TRUE) %>%
          magrittr::extract(,4),
        "emf-creaf"
      )
    ) %>%
    dplyr::select(softwork, repo, org)
  softworks_repo <- softworks_info$repo
  softworks_org <- softworks_info$org


  dplyr::tibble(
    repo = c(
      workflows_repo, tech_docs_repo, softworks_repo,
      "emf_external_models", "emf_creaf_models",
      "emf_external_data", "emf_creaf_data"
    ),
    org = c(
      workflows_org, tech_docs_org, softworks_org,
      rep("emf-creaf", 4)
    ),
    ga_action = c(
      rep("collect_metadata.yml", length(c(workflows_org, tech_docs_org, softworks_org))),
      "collect_metadata_external_models.yml", "collect_metadata_creaf.yml",
      "collect_metadata_external_data.yml", "collect_metadata_creaf.yml"
    )
  )
}