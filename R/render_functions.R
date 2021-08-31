#' Render workflow as html fragment
#'
#' Generate an html fragment with the contents of the rendered workflow
#'
#' @param resource_id Character with the resource ID
#'
#' @return html object
#'
#' @examples
#' render_workflow_fragment('dummy_workflow')
#'
#' @export
render_workflow_fragment <- function(resource_id, .envir = parent.frame()) {

  # create local tempdir, delete afterwards. The problem here is that if we use this inside a workflow
  # that already had created a tempdir, calling tempdir() here will return the same session tempfolder,
  # and deleting it afterwards (when the parent frame ends). THis will make
  temp_proj <- emf_temp_folder()
  withr::defer(fs::dir_delete(temp_proj), envir = .envir)

  # create the repo based on resource_id
  usethis::create_from_github(
    repo_spec = glue::glue("emf-creaf/{resource_id}"),
    destdir = temp_proj,
    fork = FALSE,
    rstudio = FALSE,
    open = FALSE
  )

  # get the dir
  dir <- fs::path(temp_proj, resource_id)

  # store the current project
  old_project <- usethis::proj_get()

  # go to the folder and do whatever it needs, but always back again to the original one when finish (defer)
  setwd(dir)
  withr::defer(setwd(old_project), envir = .envir)

  # switch to new project
  usethis::proj_set(dir)
  withr::defer(usethis::proj_set(old_project, force = TRUE), envir = .envir)

  # render the Rmd
  rmarkdown::render(
    input = glue::glue("{resource_id}.Rmd"),
    output_format = "html_fragment",
    output_file = glue::glue("{resource_id}_fragment.html")
  )

  # And now return the html file.
  # We return the html as a readLines object
  html_fragment <- htmltools::includeHTML(glue::glue("{resource_id}_fragment.html"))
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
#'   \code{\link{render_workflow_fragment}}
#' @param dest Path to the resource page tree
#'
#' @return Invisible TRUE
#' @examples
#' create_workflow_page('dummy_workflow')
#'
#' @export
create_workflow_page <- function(
  resource_id,
  fragment = render_workflow_fragment(resource_id),
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
  # resource_metadata <- use_public_table('workflow', workflow == resource_id)
  resource_metadata <- dplyr::tbl(.con, 'public_workflows') %>%
    dplyr::filter(workflow == resource_id) %>%
    dplyr::collect()

  # if the tibble returned has no rows, then resource does not exist and must not be created
  if (nrow(resource_metadata) < 1) {
    usethis::ui_stop(
      "{resource_id} not found in public workflows table. Stopping creation of {resource_id} page"
    )
  }

  # create the yaml frontmatter from the metadata
  yaml_frontmatter <- ymlthis::as_yml(list(
    title = resource_metadata$workflow,
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
    usethis::ui_info(c("Creating workflow folder at ", dest))
    fs::dir_create(dest)
  }

  # we need to set up interactive to FALSE, in this caller environment (this function), to avoid api asking
  # if the file must be overwritten.
  rlang::local_interactive(FALSE)
  usethis::write_over(
    fs::path(dest, 'index.md'),
    lines = c(yaml_frontmatter, fragment),
    quiet = TRUE
  )

  return(invisible(TRUE))

}

# Capture yaml lines to write
capture_yml <- function(yml) {
  withr::local_envvar(NO_COLOR = TRUE)
  utils::capture.output(print(yml))
}
