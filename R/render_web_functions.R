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
#' @noRd
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

#' Render rmd resources
#'
#' Rmd resources render function
#'
#' Rmd resources are those that need of rendering an specified Rmd file,
#' i.e. workflows, tech_docs and softworks (as the README.Rmd is rendered).
#' This functions recurse itself if resource vector is longer than 1
#'
#' @param resource character vector with the resource ids
#' @param type character vector of length 1 with the resource type to render:
#'   "workflow", "tech_doc" or "softwork"
#' @param .con pool::pool object with the metadata database connection info
#' @param .force logical indicating if the render shoud be forced even if
#'   everything is up-to-date
#' @param .web_path path to the web folder
#'
#' @return A list, with resources as names and logical values indicating
#'   if that resource was rendered (TRUE) or not (FALSE)
#'
#' @noRd
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
      resource %>%
      magrittr::set_names(., .) %>%
      purrr::map(
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

#' Render metadata resources
#'
#' metadata resources render function
#'
#' metadata resources are those that render occur based on metadata and not
#' in any Rmd file, i.e. models and data.
#' This function iterates among all resources present in the type (external
#' models, external data, creaf data or creaf models).
#' As all resources are in the same repository, is an all or nothing situation,
#' or all resources are rendered or none are, as they all share the same last
#' commit.
#'
#' @param resources character vector with the resource ids
#' @param type character vector of length 1 with the resource type to render:
#'   "creaf_data", "creaf_models", "external_data" or "external_models"
#' @param .con pool::pool object with the metadata database connection info
#' @param .force logical indicating if the render shoud be forced even if
#'   everything is up-to-date
#' @param .web_path path to the web folder
#'
#' @return A list, with resources as names and logical values indicating
#'   if that resource was rendered (TRUE) or not (FALSE)
#'
#' @noRd
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

#' Check last commit
#'
#' Check last commit of a repo against the metadata database
#'
#' In the case of models and data, as the type shares the same repository
#' (i.e. all external models are in the same repository), repo name does not
#' match the resource name in the database. In those cases we need to supply
#' repo_db with the id of the resouce in the db.
#'
#' @param repo character with repo name
#' @param org character with organization/user name
#' @param repo_db character with the repo name in the database. If NULL it
#'   defaults to repo value
#' @param .con pool::pool object with the metadata database connection info
#' @param .branch character with branch name (default to main)
#'
#' @return Invisible FALSE if repo is up-to-date, TRUE if repo is ahead database
#'
#' @noRd
check_last_commit_for <- function(
    repo,
    repo_db = NULL,
    org = "emf-creaf",
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

#' Check git status, commit and push to the remote
#'
#' Check git status, commit and push to the remote
#'
#' This function check the active working directory git status, commit any
#' changes and push to the remote repository
#'
#' @param commit_message Character with the commit message
#' @param github_pat Character wiht the remote token (GitHub)
#'
#' @return Invisible FALSE if no changes are found, TRUE if changes are found,
#'   commited and pushed
#'
#' @noRd
commit_push_web_repo <- function(commit_message, github_pat) {

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
    ssh_key = github_pat
  )


  usethis::ui_done("Web repository updated (commit: {commited}).")
  return(invisible(TRUE))
}

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
    .con = NULL, .force = FALSE
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
  pushed <- commit_push_web_repo(commit_message, github_pat)

  if (!pushed) {
    usethis::ui_done(
      "No changes detected, web repository ({org}/{repo}) up-to-date"
    )
    # if no force, exit gracefully
    if (!.force) {
      return(invisible(FALSE))
    }
  }

  # pretty print
  usethis::ui_line()
  usethis::ui_info("Copy to production server")
  usethis::ui_line("-----")

  # if remote, connect to the server by ssh and execute the command in the
  # server. If not remote, execute the function here
  prod_folder <- Sys.getenv("PROD_FOLDER")
  prod_output <- NA

  if (remote) {
    # connect to the server
    prod_session <- ssh::ssh_connect(host = Sys.getenv("PROD_HOST"))
    withr::defer(ssh::ssh_disconnect(prod_session))

    # execute command
    prod_output <- ssh::ssh_exec_internal(
      prod_session,
      glue::glue("R -e 'EMFtoolbox::copy_emf_web(dest = \"{prod_folder}\")'")
    )

  } else {
    prod_output <- copy_emf_web(dest = prod_folder)
  }

  usethis::ui_done("Web in production updated!!!")
  return(invisible(TRUE))
}

#' Print a summary of rendered pages
#'
#' Print a summary of rendered pages, using (\code{\link[usethis]{ui_todo}})
#'
#' This function takes the list returned by \code{\link{render_resource_pages}}
#' and use it to print the list of pages rendered and not rendered
#'
#' @param rendered_pages A list, as returned by \code{\link{render_resource_pages}}
#'
#' @return Invisible TRUE after printing the summary
#'
#' @noRd
rendered_summary <- function(rendered_pages) {
  usethis::ui_line()
  usethis::ui_info("The following pages were rendered:")
  purrr::flatten(rendered_pages) %>%
    purrr::keep(isTRUE) %>%
    names() %>%
    purrr::walk(usethis::ui_todo)

  usethis::ui_line()
  usethis::ui_info("The following pages were skipped:")
  purrr::flatten(rendered_pages) %>%
    purrr::keep(isFALSE) %>%
    names() %>%
    purrr::walk(usethis::ui_todo)

  return(invisible(TRUE))
}

#' Delete page from web content
#'
#' Delete page from web content
#'
#' Give a resource id and a resource type, the corresponding content
#' folder in the Hugo web folder is removed
#'
#' @param resource_id character with the ID of the resource
#' @param resource_type character with the resource type
#' @param .web_path path to the Hugo web folder
#'
#' @return Invisible TRUE
#'
#' @noRd
delete_page <- function(
    resource_id,
    resource_type = c(
      'workflows', 'tech_docs', 'models', 'data', 'softworks',
      'external_models', 'external_data'
    ),
    .web_path = Sys.getenv('WEB_PATH')
) {
  page_path <- fs::path(.web_path, 'content', resource_type, resource_id)
  fs::dir_delete(page_path)
  return(invisible(TRUE))
}

#' Front matter generator for resource pages
#'
#' Front matter generator for resource pages
#'
#' @param resource_metadata tibble with the resource metadata (one row)
#' @param category character with the category in the Hugo web (plural)
#' @param .external Not used
#'
#' @return Vector with the yaml header lines
#'
#' @noRd
frontmatter_generator <- function(resource_metadata, category, .external = FALSE) {

  # create the yaml frontmatter from the metadata
  yaml_frontmatter <-
    list(
      title = resource_metadata$title,
      authors = pq__text_to_vector_parser(resource_metadata$author),
      categories = category,
      tags = pq__text_to_vector_parser(resource_metadata$tag),
      draft = resource_metadata$emf_draft,
      featured = FALSE,
      date = dplyr::if_else(
        is_na_or_null(resource_metadata$date),
        as.character(Sys.Date()),
        as.character(resource_metadata$date)
      ),
      lastmod = dplyr::if_else(
        is_na_or_null(resource_metadata$date_lastmod),
        as.character(Sys.Date()),
        as.character(resource_metadata$date_lastmod)
      ),
      summary = resource_metadata$description,
      model_repository = resource_metadata$model_repository,
      data_repository = resource_metadata$data_repository,
      links = list(
        url_doi = resource_metadata$url_doi,
        url_pdf = resource_metadata$url_pdf,
        url_source = resource_metadata$url_source,
        url_docs = resource_metadata$url_docs
      )
    ) %>%
    purrr::map(nas_to_empty_strings) %>%
    ymlthis::as_yml() %>%
    capture_yml()

  if (isTRUE(.external)) {

  }

  return(yaml_frontmatter)

}

#' md file content generator from metadata
#'
#' md file content generator from metadata
#'
#' This function generates content from resource metadata (i.e. for models or
#' data).
#'
#' @param resource_metadata tibble with the resource metadata (one row)
#' @param dest path to Hugo web, for resource image copy (basically featured.png)
#' @param category character with the category in the Hugo web (plural)
#' @param .external Not used
#'
#' @return Vector with the content lines
#'
#' @noRd
md_content_generator <- function(resource_metadata, dest, category, .external = FALSE) {

  # create the content from the metadata
  md_content <- c(
    "",
    "## Description",
    "",
    resource_metadata$description,
    ""
  )

  usethis::ui_info("Copying the intermediate images needed:")
  intermediate_images <- copy_images(folder = emf_temp_folder(), dest, category) %>%
    purrr::walk(usethis::ui_todo)

  return(md_content)

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
#' @noRd
pq__text_to_vector_parser <- function(pq__text) {
  stringr::str_remove_all(pq__text, '[{}\"]') %>%
    stringr::str_split(',') %>%
    purrr::flatten_chr()
}

#' Get resource last commit in the database
#'
#' Get resource last commit in the database
#'
#' Connects to the database and retrieve the last commit for the desired
#' resource
#'
#' @param resource_id character with the resource id
#' @param .con pool::pool object with the metadata database connection info
#'
#' @return Hash for the last commit stored in the database for \code{resource_id}
#'
#' @noRd
get_resource_last_commit_from_db <- function(resource_id, .con) {
  dplyr::tbl(.con, 'resources_last_commit') %>%
    dplyr::filter(id == resource_id) %>%
    dplyr::collect() %>%
    dplyr::pull(last_commit_hash)
}

#' Get resource last commit in the remote repository
#'
#' Get resource last commit in the remote repository
#'
#' Connects to GitHub API and retrieve the last commit for the desired
#' resource
#'
#' @param repo character with repo name
#' @param org character with organization/user name
#' @param .branch character with branch name (default to main)
#'
#' @return Hash for the last commit in the \code{resource_id} remote repository
#'
#' @noRd
get_resource_last_commit_from_repo <- function(repo, org, .branch) {
  httr::GET(
    url = glue::glue_safe(
      "https://api.github.com/repos/{org}/{repo}/commits/{.branch}"
    ),
    config = httr::authenticate(
      user = gh::gh_whoami()$login, password = Sys.getenv("GITHUB_PAT")
    )
  ) %>%
    httr::content() %>%
    magrittr::extract2('sha')
}

#' Update resource last commit in the database
#'
#' Update resource last commit in the database
#'
#' This function checks if the resource repository is ahead of the database, and
#' if it is, updates the db with the new commit hash
#'
#' @param repo character with repo name
#' @param repo_db character with the repo name in the database. If NULL it
#'   defaults to repo value
#' @param org character with organization/user name
#' @param .con pool::pool object with the metadata database connection info
#' @param .branch character with branch name (default to main)
#'
#' @return Invisible TRUE if update occurs, FALSE otherwise
#'
#' @noRd
update_resource_last_commit_db <- function(repo, repo_db = repo, org, .con, .branch = 'main') {

  if (!check_last_commit_for(
    repo = repo, repo_db = repo_db, org = org,
    .con = .con, .branch = .branch
  )) {
    return(invisible(FALSE))
  }

  last_commit_repo <- get_resource_last_commit_from_repo(
    repo = repo, org = org, .branch = .branch
  )

  usethis::ui_info('{repo_db} last commit is ahead of database, updating...')
  update_resource_last_commit_queries <- list(
    remove = glue::glue_sql(
      "DELETE FROM resources_last_commit WHERE id = {repo_db};",
      .con = .con
    ),
    insert = glue::glue_sql(
      "INSERT INTO resources_last_commit (id, last_commit_hash) VALUES ({repo_db}, {last_commit_repo});",
      .con = .con
    )
  )
  purrr::walk(update_resource_last_commit_queries, ~ DBI::dbExecute(.con, .x))
  usethis::ui_done("{repo_db} last commit succesfully updated")
  return(invisible(TRUE))
}

#' Capture yaml lines to write
#'
#' Capture yaml lines to write
#'
#' @param yml yml object (\code{\link[ymlthis]{as_yml}})
#'
#' @return captured output (lines) without color attributes of the provided ylm
capture_yml <- function(yml) {
  withr::local_envvar(NO_COLOR = TRUE)
  utils::capture.output(print(yml))
}

#' Copy image artifacts
#'
#' Copy image artifacts
#'
#' Copy any generated image artifact (from the render step) to the corresponding
#' content folder.
#'
#' @param folder Rendering path, default to working folder
#' @param dest Destination path
#' @param category character with the Hugo category (plural)
#' @param formats image artifact formats to watch and copy
#'
#' @return FALSE if no images are copied. A vector of image file names copied
#'   otherwise
#'
#' @noRd
copy_images <- function(folder = '.', dest, category, formats = c('png', 'jpg', 'svg')) {
  # list images in folders (by formats) and copy them to dest

  # list of images
  images_list <- fs::dir_ls(
    path = folder,
    recurse = TRUE,
    type = "file",
    regexp = glue::glue("[.]{glue::glue_collapse(formats, '|')}$")
  )

  if (!any(stringr::str_detect(images_list, '^featured.png$'))) {
    images_list <- c(
      images_list,
      system.file('default_featured_images', category, 'featured.png', package = 'EMFtoolbox')
    )
  }

  if (length(images_list) < 1) {
    usethis::ui_done("No intermediate images needed")
    return(invisible(FALSE))
  }

  fs::file_copy(images_list, dest, overwrite = TRUE)
  return(invisible(images_list))
}

#' Convert NA to empty string
#'
#' Convert NA to empty string
#'
#' If \code{x} is a list, the function recurses itself by all elements of x.
#' If \code{x} is a character vector, the function recurses itself by all
#'   elements using \code{\link[purrr]{map_chr}}.
#'
#' @param x object to check
#'
#' @return If \code{x} is NA the "", \code{x} otherwise. For lists and vectors,
#'   \code{x} with all NA elements transformed to ""
#'
#' @noRd
nas_to_empty_strings <- function(x) {
  if (length(x) > 1) {
    if (is.list(x)) {
      return(purrr::map(x, nas_to_empty_strings))
    } else {
      return(purrr::map_chr(x, nas_to_empty_strings))
    }

  }
  if (is_na_or_null(x)) {
    x <- ''
  }

  return(x)
}

#' Is NA or NULL
#'
#' Is NA or NULL
#'
#' This function checks if an object is NA or NULL
#'
#' @param x object to test
#'
#' @return TRUE if \code{x} is NA or NULL, FALSE otherwise
#'
#' @noRd
is_na_or_null <- function(x) {
  is.null(x) || is.na(x)
}

