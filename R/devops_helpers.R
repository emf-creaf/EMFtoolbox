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
#' @return Data frame with the resources repo info and the update status
#'
#' @noRd
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
  updated <- resource_repositories_info(.con) %>%
    dplyr::mutate(.dry_push = .dry_push, .github_path = .github_path) %>%
    purrr::pmap(update_repo_ga) %>%
    magrittr::set_names(., resource_repositories_info(.con)$repo)
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
