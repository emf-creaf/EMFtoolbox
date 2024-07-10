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
#' @param .copy_anyways loigical indicating if Hugo rendering and copy to
#'   production must be done independently of repository changes. Default to FALSE.
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
    .con = NULL, .force = FALSE, .dry_push = FALSE, .copy_anyways = FALSE
) {
  # pretty print
  cli::cli_alert_info(c(
    "i" = "Updating EMF web",
    "-----"
  ))
  # usethis::ui_line()
  # usethis::ui_info("Updating EMF web")
  # usethis::ui_line("-----")

  # Clone the web repo
  repo_web_dir <- clone_from_github(repo = repo, org = org)

  # Render all pages
  # pretty print
  cli::cli_alert_info(c(
    "i" = "Render pages",
    "-----"
  ))
  # usethis::ui_line()
  # usethis::ui_info("Render pages")
  # usethis::ui_line("-----")
  rendered_pages <- render_resource_pages(
    .con = .con, .force = .force, .web_path = repo_web_dir
  )
  rendered_summary(rendered_pages)

  # pretty print
  cli::cli_alert_info(c(
    "i" = "Commit and push EMF web repository ({org}/{repo})",
    "-----"
  ))
  # usethis::ui_line()
  # usethis::ui_info("Commit and push EMF web repository ({org}/{repo})")
  # usethis::ui_line("-----")
  pushed <- commit_push_repo(commit_message, github_pat, .dry_push)

  if (!pushed) {
    cli::cli_alert_info(
      "No changes detected, web repository ({org}/{repo}) up-to-date"
    )
    # usethis::ui_info(
    #   "No changes detected, web repository ({org}/{repo}) up-to-date"
    # )
    # if no force, exit gracefully
    if (!.force) {
      if (!.copy_anyways) {
        cli::cli_alert_success("EMF web up-to-date, not updating and exiting.")
        # usethis::ui_done("EMF web up-to-date, not updating and exiting.")
        return(invisible(FALSE))
      }
    }
  }

  # pretty print
  cli::cli_alert_info(c(
    "i" = "Copy to production",
    "-----"
  ))
  # usethis::ui_line()
  # usethis::ui_info("Copy to production")
  # usethis::ui_line("-----")

  # if remote, connect to the server by ssh and execute the command in the
  # server. If not remote, execute the function here
  prod_output <- NA

  if (remote) {

    cli::cli_alert_info("Connecting to remote server at {prod_host}")
    # usethis::ui_info("Connecting to remote server at {prod_host}")
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
    cli::cli_alert_info("Copying locally to {prod_folder}")
    # usethis::ui_info("Copying locally to {prod_folder}")
    prod_output <- copy_emf_web(dest = prod_folder)
  }

  if (is_na_or_null(prod_output)) {
    cli::cli_alert_danger("Something went wrong, check the outputs")
    # usethis::ui_oops("Something went wrong, check the outputs")
    return(invisible(FALSE))
  }

  cli::cli_alert_success("Web in production updated!!!")
  # usethis::ui_done("Web in production updated!!!")
  return(invisible(TRUE))
}



#' Update Documentation site
#'
#' This function retrieves the latest github version of the docs and update the
#' doc server
#'
#' @param dest path to the production folder
#'
#' @return invisible TRUE if all goes well, error if not
#'
#' @details This function is intended to be used in the doc server, it has not
#'   remote option.
#'
#' @export
update_docs_site <- function(dest) {
  # clone the latest version of the doc site
  clone_from_github(repo = 'emfverse_docs_site', org = 'emf-creaf')

  # build the site
  cli::cli_alert_info("Rendering site with Quarto")
  system2("quarto", args = "render")
  cli::cli_alert_success("Done")

  # copy web
  cli::cli_alert_info("Copying rendered site to production")
  origin <- "_site"
  dest_guide <- fs::path(dest, "development_guide")
  # remove dest folder contents, but recreate the development guide one!!
  fs::file_delete(fs::dir_ls(dest))
  fs::dir_create(dest_guide)
  # copy new web data
  fs::dir_copy(origin, dest, TRUE)

  # development guide (is yet another repository)
  # clone the latest version of the doc site
  clone_from_github(repo = 'development_guide', org = 'emf-creaf')

  # build the site
  cli::cli_alert_info("Rendering development guide")
  system2("quarto", args = "render")
  cli::cli_alert_success("Done")

  # copy web
  cli::cli_alert_info("Copying development guide to production")
  origin_guide <- "_book"
  # remove dest folder contents
  fs::file_delete(fs::dir_ls(dest_guide))
  # copy new web data
  fs::dir_copy(origin_guide, dest_guide, TRUE)

  # return TRUE if everything is ok
  cli::cli_alert_success("Docs site generated without errors")
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
  updated <- resources_info |>
    dplyr::mutate(.dry_push = .dry_push, .github_path = .github_path) |>
    purrr::pmap(update_repo_ga) |>
    purrr::set_names(resources_info$repo)

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

  workflows_repo <- public_workflows(.con = .con) |>
    dplyr::pull(workflow)
  workflows_org <- rep("emf-creaf", length(workflows_repo))
  tech_docs_repo <- public_tech_docs(.con = .con) |>
    dplyr::pull(tech_doc)
  tech_docs_org <- rep("emf-creaf", length(tech_docs_repo))

  softworks_info <- public_softworks(.con = .con) |>
    dplyr::mutate(
      repo = dplyr::if_else(
        !is.na(url_source),
        url_source |>
          stringr::str_split(pattern = '/', simplify = TRUE) |>
          as.data.frame() |>
          dplyr::pull(5),
        softwork
      ),
      org = dplyr::if_else(
        !is.na(url_source),
        url_source |>
          stringr::str_split(pattern = '/', simplify = TRUE) |>
          as.data.frame() |>
          dplyr::pull(4),
        "emf-creaf"
      )
    ) |>
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

#' Debugging render process
#'
#' Simulate resource render process from scratch
#'
#' This functions simulates all the workflow for rendering a resource, from the
#' metadata collection to the render_rmd. This allows for easy iteration process
#' to fix a resource that is not rendering ok, or to test different formats.
#' This function is only useful for workflows, tech_docs and software resources
#'
#' @noRd
#' @param resource resource id
#' @param resource_type resource type
#' @param test_web_path path to web folder (for render_rmd). Default to a temporal folder
#' @param test_envvars list with environmental variables needed for rendering (database...)
#' @param test_yml_path path to the resource yml file (for collect_metadata)
#' @param test_folder_path path to the testing folder in the Hugo project. This allows for
#'  later inspection and Hugo rendering
#'
#' @return TRUE if folder has been created in the \code{test_folder_path}
debug_render_resource <- function(
  resource,
  resource_type,
  test_web_path = emf_temp_folder(),
  test_envvars = list(),
  test_yml_path = fs::path(),
  test_folder_path = fs::path()
) {

  # join envvars
  envvars <- c(test_envvars, WEB_PATH = test_web_path)

  # local env
  withr::local_envvar(envvars)

  # connect to database
  test_database <- metadata_db_con()

  # on exit/error/unexpected, remove the resource from test database
  withr::defer({
    delete_resource_from_db(resource, con = test_database)
    unlink(Sys.getenv("WEB_PATH"), recursive = TRUE)
  })

  # collect metadata
  collect_metadata(
    con = test_database,
    yml_file = fs::path(test_yml_path, resource, "metadata.yml"),
    .dry = FALSE
  )

  # render rmd/quarto
  render_rmd(
    resource, resource_type,
    .con = test_database, .web_path = Sys.getenv("WEB_PATH"), .force = FALSE
  )

  # copy to web test folder
  fs::dir_copy(
    fs::path(Sys.getenv("WEB_PATH"), "content", glue::glue("{resource_type}s"), resource),
    fs::path(test_folder_path, resource),
    overwrite = TRUE
  )

  return(fs::dir_exists(fs::path(test_folder_path, resource)))
}
