#' Render all resource pages
#'
#' Render all resource pages if needed
#'
#' This function starts the process to update and render all pages for resources
#' present in the metadata database.
#'
#' @param .con Connection to the metadata database
#' @param ... Arguments passed to render_resource_type
#'
#' @return A named vector for resources indicating (TRUE or FALSE) if they have
#' been updated and rendered
#'
#' @examples
#'
#' render_resource_pages()
#'
#' @export
render_resource_pages <-
  function(..., .con = NULL, .force = FALSE, .web_path = Sys.getenv("WEB_PATH")) {

    # take the resources table
    # filter the resources by ...
    # split in types(models, data, ...)
    # set the names
    # apply the specified function for each resource
    #

    # Connect to db if no .con supplied
    if (is.null(.con)) {
      .con <- metadata_db_con()
    }

    resources_table <- use_public_table('all', ..., .con = .con) %>%
      dplyr::select(id, emf_type, emf_data_type)

    resources_by_type <- list(
      workflows = resources_table %>%
        dplyr::filter(emf_type == 'workflow') %>%
        dplyr::pull(id),
      tech_docs = resources_table %>%
        dplyr::filter(emf_type == 'tech_doc') %>%
        dplyr::pull(id),
      softworks = resources_table %>%
        dplyr::filter(emf_type == "softwork") %>%
        dplyr::pull(id),
      creaf_models = resources_table %>%
        dplyr::filter(emf_type == 'model', emf_data_type == "creaf_data") %>%
        dplyr::pull(id),
      external_models = resources_table %>%
        dplyr::filter(emf_type == 'model', emf_data_type == "external_data") %>%
        dplyr::pull(id),
      creaf_data = resources_table %>%
        dplyr::filter(emf_type == 'data', emf_data_type == "creaf_data") %>%
        dplyr::pull(id),
      external_data = resources_table %>%
        dplyr::filter(emf_type == 'data', emf_data_type == "external_data") %>%
        dplyr::pull(id)
    )

    res <- list(
      workflows = render_rmd(
        resources_by_type[['workflows']],
        type = "workflow",
        .con = .con,
        .force = .force,
        .web_path = .web_path
      ),
      tech_docs = render_rmd(
        resources_by_type[['tech_docs']],
        type = "tech_doc",
        .con = .con,
        .force = .force,
        .web_path = .web_path
      ),
      softworks = render_rmd(
        resources_by_type[['softworks']],
        type = "softwork",
        .con = .con,
        .force = .force,
        .web_path = .web_path
      ),
      creaf_models = render_metadata(
        resources_by_type[['creaf_models']],
        type = "creaf_model",
        .con = .con,
        .force = .force,
        .web_path = .web_path
      ),
      external_models = render_metadata(
        resources_by_type[['external_models']],
        type = "external_model",
        .con = .con,
        .force = .force,
        .web_path = .web_path
      ),
      creaf_data = render_metadata(
        resources_by_type[['creaf_data']],
        type = "creaf_data",
        .con = .con,
        .force = .force,
        .web_path = .web_path
      ),
      external_data = render_metadata(
        resources_by_type[['external_data']],
        type = "external_data",
        .con = .con,
        .force = .force,
        .web_path = .web_path
      )
    )

    return(res)
  }

render_rmd <- function(resource, type, .con, .force, .web_path) {

  # argument checks
  if (length(resource) < 1) {
    usethis::ui_info("No {type} to render, skipping...")
    return(invisible(FALSE))
  }

  # vectorize function if resource is longer than 1, in a recursive way
  if (length(resource) > 1) {
    safe_fun <- purrr::possibly(render_rmd, otherwise = FALSE, quiet = FALSE)
    res <-
      purrr::map(
        resource,
        .f = safe_fun,
        type = type, .con = .con, .force = .force, .web_path = .web_path
      )

    return(res)
  }

  # pretty print
  usethis::ui_line()
  usethis::ui_info("Starting render of {resource} ({type})")
  usethis::ui_line("-----")

  # get input name, dest and others, from type
  input_file <- dplyr::case_when(
    type %in% c('workflow', 'tech_doc') ~ glue::glue("{resource}.Rmd"),
    type %in% c('softwork') ~ glue::glue("README.Rmd"),
    TRUE ~ glue::glue("README.Rmd")
  )

  plural_type <- switch(
    type,
    'workflow' = 'workflows',
    'tech_doc' = 'tech_docs',
    'softwork' = 'softworks'
  )

  dest <- fs::path(
    .web_path, "content",
    # in the web is called software :(
    dplyr::if_else(plural_type == 'softworks', 'software', plural_type),
    resource
  )

  # get metadata, if there is no metadata, stop there
  filter_expr <- rlang::parse_expr(glue::glue("{type} == '{resource}'"))
  resource_metadata <- use_public_table(plural_type, filter_expr, .con = .con)

  if (nrow(resource_metadata) < 1) {
    usethis::ui_stop(
      "{resource} not found in public {plural_type} table. Stopping creation of {resource} page"
    )
  }

  # repo info
  url_source <- resource_metadata %>%
    dplyr::pull(url_source)

  repo <- resource
  org <- "emf-creaf"

  if (!is.na(url_source)) {
    repo_info <- url_source %>%
      stringr::str_split(pattern = '/', simplify = TRUE) %>%
      magrittr::extract(4:5) %>%
      magrittr::set_names(c('org', 'repo'))
    repo <- repo_info[["repo"]]
    org <- repo_info[["org"]]
  }

  # commit check
  if (check_last_commit_for(repo = repo, org = org, .con = .con)) {
    .force <- TRUE
  }

  # destination checks:
  # if dest folder does not exists, we must create it
  # if index.md file does not exists in the folder, we need to force the render
  if (!fs::dir_exists(dest)) {
    usethis::ui_info("Creating {resource} folder at {dest}")
    fs::dir_create(dest)
  }

  if (!fs::file_exists(fs::path(dest, 'index.md'))) {
    usethis::ui_info("{usethis::ui_code('index.md')} doesn't exist, forcing render.")
    .force <- TRUE
  }

  # create a backup
  dest_backup <- create_folder_backup(dest)

  # render the fragment, only if force is TRUE
  if (!.force) {
    return(invisible(FALSE))
  }

  # now we can render
  # clone and go to folder
  cloned_dir <- clone_from_github(repo, org)

  # if file does not exists, it doesn't matter if should be updated or not
  if (!fs::file_exists(input_file)) {
    usethis::ui_stop("{input_file} file not found in {resource} repository")
  }

  # render the Rmd
  rmarkdown::render(
    input = input_file,
    output_format = rmarkdown::md_document(variant = 'markdown'),
    output_file = glue::glue("{resource}.md"),
    quiet = TRUE
  )

  # copy intermediate images
  usethis::ui_info("Copying the intermediate images needed:")
  intermediate_images <- copy_images('.', dest, plural_type) %>%
    purrr::walk(usethis::ui_todo)


  # And now return the html file.
  # We return the html as a readLines object
  rd_fragment <- readLines(
    glue::glue("{resource}.md"),
    warn = FALSE, encoding = "UTF-8"
  ) %>%
    # images substitution
    rd_postprocessing(intermediate_images)

  # create the yaml frontmatter from the metadata
  yaml_frontmatter <- frontmatter_generator(resource_metadata, plural_type)

  # join frontmatter and fragment and write the file
  # (overwritting existing file). Code for writing taken from usethis::write_over and
  # xfun::write_utf8, because usethis::write_over (which would be ideal), requires
  # mandatory user input to overwrite, with no way to avoid it.
  writeLines(
    enc2utf8(c(yaml_frontmatter, rd_fragment)),
    fs::path(dest, 'index.md'),
    useBytes = TRUE
  )

  usethis::ui_done(
    "{usethis::ui_code('index.md')} written succesfully at {usethis::ui_path(dest)}"
  )
  # update last commit
  return(invisible(update_resource_last_commit_db(
    repo = repo, org = org, .con = .con
  )))

}

render_metadata <- function(resources, type, .con, .force, .web_path) {

  # argument checks
  if (length(resources) < 1) {
    usethis::ui_info("No {type} to render, skipping...")
    return(invisible(FALSE))
  }

  # get dest and others, from type
  plural_type <- switch(
    type,
    'creaf_model' = 'models',
    'external_model' = 'external_models',
    'creaf_data' = 'data',
    'external_data' = 'external_data'
  )

  repo <- switch(
    type,
    'creaf_model' = 'emf_creaf_models',
    'external_model' = 'emf_external_models',
    'creaf_data' = 'emf_creaf_data',
    'external_data' = 'emf_external_data'
  )
  org <- "emf-creaf"

  external <- type %in% c('external_model', 'external_data')

  category <- dplyr::if_else(
    type %in% c('creaf_model', 'external_model'),
    'models',
    'data'
  )

  resource_type <- dplyr::if_else(
    type %in% c('creaf_model', 'external_model'),
    'model',
    'data'
  )

  metadata_table <- switch(
    type,
    'creaf_model' = public_models(emf_data_type == 'creaf_data', .con = .con),
    'external_model' = public_models(emf_data_type == 'external_data', .con = .con),
    'creaf_data' = public_data(emf_data_type == 'creaf_data', .con = .con),
    'external_data' = public_data(emf_data_type == 'external_data', .con = .con)
  )

  # commit check
  if (check_last_commit_for(
    repo = repo, repo_db = resources[1], org = org, .con = .con
  )) {
    .force <- TRUE
  }

  # render the fragment, only if force is TRUE
  if (!.force) {
    return(invisible(FALSE))
  }

  res <- metadata_table[[1]] %>%
    magrittr::set_names(., .) %>%
    purrr::map(
      .f = function(resource) {

        # pretty print
        usethis::ui_line()
        usethis::ui_info("Starting render of {resource} ({type})")
        usethis::ui_line("-----")

        dest <- fs::path(.web_path, "content", plural_type, resource)
        # if dest folder does not exists, we must create it
        if (!fs::dir_exists(dest)) {
          usethis::ui_info("Creating {resource} folder at {dest}")
          fs::dir_create(dest)
        }

        filter_expr <- rlang::parse_expr(glue::glue("{resource_type} == '{resource}'"))
        resource_metadata <- metadata_table %>%
          dplyr::filter(!!filter_expr)

        yaml_frontmatter <- frontmatter_generator(
          resource_metadata, category, .external = external
        )
        md_content <- md_content_generator(
          resource_metadata, dest, category, .external = external
        )

        writeLines(
          enc2utf8(c(yaml_frontmatter, md_content)),
          fs::path(dest, 'index.md'),
          useBytes = TRUE
        )

        usethis::ui_done(
          "{usethis::ui_code('index.md')} written succesfully at {usethis::ui_path(dest)}"
        )
        # update last commit
        return(invisible(update_resource_last_commit_db(
          repo = repo, repo_db = resource, org = org, .con = .con
        )))

      }
    )

  return(res)

}

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


check_last_commit_for <- function(
    repo,
    org = "emf-creaf",
    repo_db = NULL,
    .con, .branch = 'main'
) {

  # for multi resources repositories (data, models), the repo name in the database
  # is not the same.
  if (is.null(repo_db)) {
    repo_db <- repo
  }

  # get the repo last commit (via github API), get the db recorded last commit,
  # compare them and return TRUE if they are different, and FALSE if they are
  # identical

  last_commit_repo <- get_resource_last_commit_from_repo(
    repo = repo, org = org, .branch = .branch
  )

  last_commit_db <-
    get_resource_last_commit_from_db(repo_db, .con)

  # if hashes are identical, return FALSE
  if (identical(last_commit_repo, last_commit_db)) {
    usethis::ui_info('{repo} last commit is up-to-date with database')
    return(invisible(FALSE))
  }

  # if they are not identical, return TRUE
  usethis::ui_info('{repo} last commit is ahead of database')
  return(invisible(TRUE))
}

update_web_repo <- function(
    repo = "emf_web", org = "emf-creaf",
    commit_message = glue::glue("{Sys.Date()} automatic update"),
    .con = NULL, .force = FALSE
) {
  # pretty print
  usethis::ui_line()
  usethis::ui_info("Updating EMF web repository ({org}/{repo})")
  usethis::ui_line("-----")

  # Clone the web repo
  repo_web_dir <- clone_from_github(repo = repo, org = org)

  # Render all pages
  rendered_pages <- render_resource_pages(
    .con = .con, .force = .force, .web_path = repo_web_dir
  )

  # Commit changes, only if there is changes to commit
  if (!nrow(gert::git_status()) > 0) {
    usethis::ui_done("No changes made in the web repository, exiting...")
    return(invisible(FALSE))
  }

  # Commit changes
  added <- gert::git_add('.')
  commited <- gert::git_commit(commit_message)
  # Push changes
  pushed <- gert::git_push(
    remote = 'origin',
    ssh_key = Sys.getenv('GITHUB_PAT')
  )


  usethis::ui_done("Web repository updated (commit: {commited}).")
  return(invisible(TRUE))

}
