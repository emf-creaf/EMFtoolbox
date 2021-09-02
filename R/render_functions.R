#' Render resource Rmd as html fragment
#'
#' Generate an html fragment with the contents of the rendered Rmd file.
#'
#' This function assumes that the Rmd file is called the same as the resource id
#'
#' @param resource_id Character with the resource ID
#' @param .envir envir for \code{withr::defer}, default to \code{parent.frame()}
#' @param .render_quiet Must \code{rmarkdown::render} be quiet?
#'
#' @return html object
#'
#' @examples
#' render_html_fragment('test_dummy_workflow')
#'
#' @export
render_html_fragment <- function(resource_id, .envir = parent.frame(), .render_quiet = TRUE) {

  temp_proj <- emf_temp_folder()
  withr::defer(fs::dir_delete(temp_proj), envir = .envir)

  # store the current project
  old_project <- usethis::proj_get()

  # get the dir
  dir <- fs::path(temp_proj, resource_id)
  fs::dir_create(dir)
  # create the dir, go to the folder and do whatever it needs, but always back again to the original one when
  # finish (defer)
  setwd(dir)
  withr::defer(setwd(old_project), envir = .envir)
  # switch to new project
  usethis::proj_set(dir, force = TRUE)
  withr::defer(usethis::proj_set(old_project, force = TRUE), envir = .envir)

  # create the repo based on resource_id
  usethis::create_from_github(
    repo_spec = glue::glue("emf-creaf/{resource_id}"),
    destdir = temp_proj,
    fork = FALSE,
    rstudio = FALSE,
    open = FALSE
  )

  # render the Rmd
  rmarkdown::render(
    input = glue::glue("{resource_id}.Rmd"),
    output_format = "html_fragment",
    output_file = glue::glue("{resource_id}_fragment.html"),
    quiet = .render_quiet
  )

  # And now return the html file.
  # We return the html as a readLines object
  html_fragment <- readLines(glue::glue("{resource_id}_fragment.html"), warn = FALSE, encoding = "UTF-8")
  # html_fragment <- htmltools::includeHTML(glue::glue("{resource_id}_fragment.html"))
  return(html_fragment)
}

#' Create a workflow page in the web project
#'
#' This function creates a web page for the workflow resource based on the id
#'
#' The path to the web can be stored in a environment variable and this function will take it automatically.
#'
#' @param resource_id ID of the workflow resource
#' @param fragment HTML object with the rendered html fragment of the workflow, as obtained by
#'   \code{\link{render_html_fragment}}
#' @param dest Path to the resource page tree
#'
#' @return Invisible TRUE
#' @examples
#' create_workflow_page('dummy_workflow')
#'
#' @export
create_workflow_page <- function(
  resource_id,
  fragment = render_html_fragment(resource_id),
  dest = fs::path(Sys.getenv('WEB_PATH'), 'content', 'workflows', resource_id),
  .con = NULL
) {

  # connect to database if needed
  if (is.null(.con)) {
    .con <- metadata_db_con()
    # close the connection when the function exits
    withr::defer(pool::poolClose(.con))
  }

  # get resource metadata
  resource_metadata <- public_workflows(workflow == resource_id)

  # if the tibble returned has no rows, then resource does not exist and must not be created
  if (nrow(resource_metadata) < 1) {
    usethis::ui_stop(
      "{resource_id} not found in public workflows table. Stopping creation of {resource_id} page"
    )
  }

  # create the yaml frontmatter from the metadata
  yaml_frontmatter <- ymlthis::as_yml(list(
    title = resource_metadata$title,
    authors = pq__text_to_vector_parser(resource_metadata$author),
    categories = 'workflows',
    tags = pq__text_to_vector_parser(resource_metadata$tag),
    draft = resource_metadata$emf_draft,
    featured = FALSE,
    date = resource_metadata$date,
    lastmod = resource_metadata$date_lastmod,
    summary = resource_metadata$description
  )) %>%
    capture_yml()

  # join frontmatter and fragment and write the file
  if (!fs::dir_exists(dest)) {
    usethis::ui_info("Creating workflow folder at {dest}")
    fs::dir_create(dest)
  }

  # write file (overwritting existing file). Code for writing taken from usethis::write_over and
  # xfun::write_utf8, because usethis::write_over (which would be ideal), requires mandatory user input to
  # overwrite, with no way to avoid it.
  written <- write_lines_utf8(lines = c(yaml_frontmatter, fragment), path = fs::path(dest, 'index.md'))

  if (written) {
    usethis::ui_done("{usethis::ui_code('index.md')} written succesfully at {usethis::ui_path(dest)}")
  } else {
    usethis::ui_info("{usethis::ui_path(dest)} already up-to-date, not overwritting.")
    return(invisible(FALSE))
  }

  return(invisible(TRUE))
}

delete_page <- function(
  resource_id,
  resource_type = c('workflows', 'tech_docs', 'models', 'data', 'softworks'),
  .web_path = Sys.getenv('WEB_PATH')
) {
  page_path <- fs::path(.web_path, 'content', resource_type, resource_id)
  fs::dir_delete(page_path)
}

# Capture yaml lines to write
capture_yml <- function(yml) {
  withr::local_envvar(NO_COLOR = TRUE)
  utils::capture.output(print(yml))
}

#' Update pages for workflows
#'
#' Update pages for workflows
#'
#' This function is a vectorised and safe (purrr::possibly) version of \code{\link{create_workflow_page}}, and
#' hence, it depends on a connection to the database (created from environment variables).
#'
#' @param resources Character vector with the resource IDs to update. Default to NULL, which retrieves all
#' public workflows and update them
#'
#' @param ... Arguments for \code{\link{create_workflow_page}}, except for resource_id.
#'
#' @return a named list with the resources and if they were updated or not (logical)
#'
#' @examples
#' update_workflow_pages(c('test_dummy_workflow', 'non_existent_workflow'))
#'
#' @export
update_workflow_pages <- function(resources = NULL, ...) {

  # retrieve the public workflows if resources list is null
  if (is.null(resources)) {
    resources <- public_workflows()[['workflow']]
  }

  # name the resources
  if (is.null(names(resources))) {
    names(resources) <- resources
  }
  # create a safe version of create_workflow_page to check the errors if any
  create_workflow_page_safe <- purrr::possibly(create_workflow_page, otherwise = FALSE, quiet = FALSE)
  created_pages <- purrr::map(resources, create_workflow_page_safe, ...)
  failed_pages <- created_pages[created_pages == FALSE]
  if (length(failed_pages) > 0) {
    usethis::ui_oops(c(
      crayon::bold("Oops! The following workflow pages weren't updated:"),
      "{usethis::ui_value(names(failed_pages))}"
    ))
  }
  ok_pages <- created_pages[created_pages == TRUE]
  if (length(ok_pages) > 0) {
    usethis::ui_done(c(
      crayon::bold("Succesfully updated the following workflow pages:"),
      "{usethis::ui_value(names(ok_pages))}"
    ))
  }
  # return all pages and their state (updated or not)
  return(created_pages)
}
