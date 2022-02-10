#' Render resource Rmd as html fragment
#'
#' Generate an html fragment with the contents of the rendered Rmd file.
#'
#' This function assumes that the Rmd file is called the same as the resource id
#'
#' @param resource_id Character with the resource ID
#' @param dest Destination of any intermediate file generated (images)
#' @param .render_quiet Must \code{rmarkdown::render} be quiet?
#' @param .force Should the fragment be rendered even if there is no new commit in resource repository?
#' @param .con Connection to pass, if provided
#'
#' @return html object
#'
#' @examples
#' render_rd_fragment('test_dummy_workflow')
#'
#' @export
render_rd_fragment <- function(
  resource_id, dest, category, .render_quiet = TRUE, .force = FALSE, .con = NULL, .input = NULL, ...
) {

  if (is.null(.input)) {
    .input <- glue::glue("{resource_id}.Rmd")
  }

  # clone the repository in a temporal folder that will be cleaned afterwards
  should_be_updated <- create_from_emf_github(resource_id, .con = .con, ...)

  # if file does not exists, it doesn't matter if should be updated or not
  if (!fs::file_exists(.input)) {
    usethis::ui_oops("Oops! Something went wrong with {resource_id}")
    usethis::ui_stop("{.input} file not found in {resource_id} repository")
  }

  # Check if the page should be updated and there is no force in play
  if (isFALSE(should_be_updated) & isFALSE(.force)) {
    return(invisible(FALSE))
  }

  # render the Rmd
  rmarkdown::render(
    input = .input,
    output_format = rmarkdown::md_document(variant = 'markdown'),
    output_file = glue::glue("{resource_id}.md"),
    quiet = .render_quiet
  )

  # copy intermediate images
  usethis::ui_info("Copying the intermediate images needed:")
  intermediate_images <- copy_images('.', dest, category) %>%
    purrr::walk(usethis::ui_todo)


  # And now return the html file.
  # We return the html as a readLines object
  rd_fragment <- readLines(
    glue::glue("{resource_id}.md"),
    warn = FALSE, encoding = "UTF-8"
  ) %>%
    # images substitution
    rd_postprocessing(intermediate_images)

  return(rd_fragment)
}

create_metadata_page <- function(emf_type, resource_id, dest, .con, .render_quiet, .force, ...) {

  # connect to database if needed
  if (is.null(.con)) {
    .con <- metadata_db_con()
    # close the connection when the function exits
    withr::defer(pool::poolClose(.con))
  }

  # emf_type related
  category <- switch(
    emf_type,
    'workflow' = "workflows",
    'tech_doc' = "tech_docs",
    'model' = "models",
    'data' = "data",
    'softwork' = "softworks"
  )

  filter_expr <- rlang::parse_expr(glue::glue("{emf_type} == '{resource_id}'"))

  # first things first, check if the provided resource is a public workflow
  # get resource metadata
  resource_metadata <- use_public_table(category, filter_expr, .con = .con)

  # if the tibble returned has no rows, then resource does not exist and must not be created
  if (nrow(resource_metadata) < 1) {
    usethis::ui_oops('Oops!')
    usethis::ui_stop(
      "{resource_id} not found in public {category} table. Stopping creation of {resource_id} page"
    )
  }

  # clone the repository in a temporal folder that will be cleaned afterwards
  # Here we check if is external and hijack the resource_id
  should_be_updated <- FALSE
  yaml_frontmatter <- ''
  md_content <- ''

  if (is_external(resource_metadata)) {
    ## external model/data
    dest <- fs::path(Sys.getenv('WEB_PATH'), 'content', glue::glue('external_{category}'), resource_id)
    # Check if dest exists, if not, not matter the commit, we need to create the page
    if (!fs::dir_exists(dest)) {
      usethis::ui_info("Creating {emf_type} folder at {dest}")
      fs::dir_create(fs::path(dest))
    }
    external_resource_id <- glue::glue("emf_external_{category}")
    should_be_updated <- create_from_emf_github(
      resource_id, .con = .con, .external = TRUE,
      .external_id = external_resource_id, ...
    )
    yaml_frontmatter <- frontmatter_generator(resource_metadata, category, .external = TRUE)
    md_content <- md_content_generator(resource_metadata, dest, category, .external = TRUE)
  } else {
    # Check if dest exists, if not, not matter the commit, we need to create the page
    if (!fs::dir_exists(dest)) {
      usethis::ui_info("Creating {emf_type} folder at {dest}")
      fs::dir_create(fs::path(dest))
    }
    ## no external
    should_be_updated <- create_from_emf_github(resource_id, .con = .con, ...)
    yaml_frontmatter <- frontmatter_generator(resource_metadata, category, .external = FALSE)
    md_content <- md_content_generator(resource_metadata, dest, category, .external = FALSE)
  }

  # Check if index.md exists, if not, not matter the commit, we need to create the page
  if (!fs::file_exists(fs::path(dest, 'index.md'))) {
    should_be_updated <- TRUE
  }

  # now in a folder call as the resource, it must be the static files we need to move to the web folder
  if (!should_be_updated & !.force) {
    usethis::ui_info("{usethis::ui_path(dest)} already up-to-date, not overwritting.")
    return(invisible(FALSE))
  }

  # write file (overwritting existing file). Code for writing taken from usethis::write_over and
  # xfun::write_utf8, because usethis::write_over (which would be ideal), requires mandatory user input to
  # overwrite, with no way to avoid it.
  written <- write_lines_utf8(lines = c(yaml_frontmatter, md_content), path = fs::path(dest, 'index.md'))

  if (!written) {
    usethis::ui_info("{usethis::ui_path('index.md')} content didn't change, ommiting writting step")
    return(invisible(TRUE))
  }

  usethis::ui_done("{usethis::ui_code('index.md')} written succesfully at {usethis::ui_path(dest)}")
  return(invisible(TRUE))

}

create_rmd_page <- function(emf_type, resource_id, dest, .con, .render_quiet, .force, .input, ...) {

  # connect to database if needed
  if (is.null(.con)) {
    .con <- metadata_db_con()
    # close the connection when the function exits
    withr::defer(pool::poolClose(.con))
  }

  # emf_type related
  category <- switch(
    emf_type,
    'workflow' = "workflows",
    'tech_doc' = "tech_docs",
    'model' = "models",
    'data' = "data",
    'softwork' = "softworks"
  )

  filter_expr <- rlang::parse_expr(glue::glue("{emf_type} == '{resource_id}'"))

  # first things first, check if the provided resource is a public workflow
  # get resource metadata
  resource_metadata <- use_public_table(category, filter_expr, .con = .con)

  # if the tibble returned has no rows, then resource does not exist and must not be created
  if (nrow(resource_metadata) < 1) {
    usethis::ui_oops('Oops!')
    usethis::ui_stop(
      "{resource_id} not found in public {category} table. Stopping creation of {resource_id} page"
    )
  }

  # create the dest if it not exist
  if (!fs::dir_exists(dest)) {
    usethis::ui_info("Creating {emf_type} folder at {dest}")
    fs::dir_create(dest)
  }

  # Check if dest exists, if not, not matter the commit, we need to create the page
  if (!fs::file_exists(fs::path(dest, 'index.md'))) {
    .force <- TRUE
  }

  fragment <- render_rd_fragment(resource_id, dest, category, .force = .force, .con = .con, .input = .input, ...)

  # Check if the workflow page must be updated by the last commit.
  # If there is a new commit, we start the process but check later if the fragment is different or not.
  if (isFALSE(fragment)) {
    usethis::ui_info("{usethis::ui_path(dest)} already up-to-date, not overwritting.")
    return(invisible(FALSE))
  }

  # create the yaml frontmatter from the metadata
  yaml_frontmatter <- frontmatter_generator(resource_metadata, category)

  # join frontmatter and fragment and write the file
  # (overwritting existing file). Code for writing taken from usethis::write_over and
  # xfun::write_utf8, because usethis::write_over (which would be ideal), requires mandatory user input to
  # overwrite, with no way to avoid it.
  written <- write_lines_utf8(lines = c(yaml_frontmatter, fragment), path = fs::path(dest, 'index.md'))

  if (!written) {
    usethis::ui_info("{usethis::ui_path('index.md')} content didn't change, ommiting writting step")
    return(invisible(TRUE))
  }

  usethis::ui_done("{usethis::ui_code('index.md')} written succesfully at {usethis::ui_path(dest)}")
  return(invisible(TRUE))
}

#' Create a workflow page in the web project
#'
#' This function creates a web page for the workflow resource based on the id
#'
#' The path to the web can be stored in a environment variable and this function will take it automatically.
#'
#' @param resource_id ID of the workflow resource
#' @param dest Path to the resource page tree
#' @param .con Connection to the database
#' @param .render_quiet Logical, must the render function be quiet?
#'
#' @return Invisible TRUE
#' @examples
#' create_workflow_page('test_dummy_workflow')
#'
#' @export
create_workflow_page <- function(
  resource_id,
  dest = fs::path(Sys.getenv('WEB_PATH'), 'content', 'workflows', resource_id),
  .con = NULL, .render_quiet = TRUE, .force = FALSE, ...
) {
  create_rmd_page('workflow', resource_id, dest, .con, .render_quiet, .force, .input = NULL, ...)
}

#' Render pkgdown for softworks
#'
#' Generate the static folder to add to the web project
#'
#' This function assumes that the softwork repository name is the same as the resource_id
#'
#' @param resource_id Character with the resource ID
#' @param .render_quiet Must \code{pkgdown::build_site} be quiet?
#'
#' @return invisible TRUE
#'
#' @examples
#' create_softwork_page('meteospain')
#'
#' @export
create_softwork_page <- function(
  resource_id,
  dest = fs::path(Sys.getenv('WEB_PATH'), 'content', 'software', resource_id),
  .con = NULL, .render_quiet = TRUE, .force = FALSE, .input = 'README.Rmd', ...
) {
  create_rmd_page('softwork', resource_id, dest, .con, .render_quiet, .force, .input, ...)
}

#' Create a technical document page in the web project
#'
#' This function creates a web page for the tech_doc resource based on the id
#'
#' Tech docs can be Rmd docs or bookdown pages.
#'
#' @inheritParams create_workflow_page
#'
#' @return invisible TRUE
#'
#' @examples
#' create_tech_doc_page('test_dummy_tech_doc')
#'
#' @export
create_tech_doc_page <- function(
  resource_id,
  dest = fs::path(Sys.getenv('WEB_PATH'), 'content', 'tech_docs', resource_id),
  .con = NULL, .render_quiet = TRUE, .force = FALSE, ...
) {
  create_rmd_page('tech_doc', resource_id, dest, .con, .render_quiet, .force, .input = NULL, ...)
}

#' Create a tmodel page in the web project
#'
#' This function creates a web page for the model resource based on the id
#'
#' Root web path can be stored in a env var and used here
#'
#' @param resource_id ID of the workflow resource
#' @param dest Path to the resource page tree
#' @param .con Connection to the database
#' @param .render_quiet Logical, must the render function be quiet?
#' @param .force Logical, must the render be done even if not needed?
#'
#' @return invisible TRUE
#'
#' @examples
#' create_model_page('test_dummy_model')
#'
#' @export
create_model_page <- function(
  resource_id,
  dest = fs::path(Sys.getenv('WEB_PATH'), 'content', 'models', resource_id),
  .con = NULL, .render_quiet = TRUE, .force = FALSE, ...
) {
  create_metadata_page('model', resource_id, dest, .con, .render_quiet, .force, ...)
}

#' Create a data page in the web project
#'
#' This function creates a web page for the data resource based on the id
#'
#' Root web path can be stored in a env var and used here
#'
#' @param resource_id ID of the workflow resource
#' @param dest Path to the resource page tree
#' @param .con Connection to the database
#' @param .render_quiet Logical, must the render function be quiet?
#' @param .force Logical, must the render be done even if not needed?
#'
#' @return invisible TRUE
#'
#' @examples
#' create_data_page('test_dummy_data')
#'
#' @export
create_data_page <- function(
  resource_id,
  dest = fs::path(Sys.getenv('WEB_PATH'), 'content', 'data', resource_id),
  .con = NULL, .render_quiet = TRUE, .force = FALSE, ...
) {
  create_metadata_page('data', resource_id, dest, .con, .render_quiet, .force, ...)
}

#' Update pages for resources
#'
#' Update pages for resources
#'
#' This function takes a resource type and a resource names list and create the pages if needed.
#' If no resources list is supplied, all resources of the selected type are updated.
#'
#' @param emf_type Character with the type of resources to update
#' @param resources Character vector with the resource IDs to update. Default to NULL, which retrieves all
#' public workflows and update them
#' @param ... Arguments for \code{\link{create_*_page}}, except for resource_id.
#'
#' @return a named list with the resources and if they were updated or not (logical)
#'
#' @examples
#' update_resource_pages_by_type('workflow', c('test_dummy_workflow', 'non_existent_workflow'))
#'
#' @export
update_resource_pages_by_type <- function(
  emf_type = c('workflow', 'tech_doc', 'model', 'data', 'softwork'),
  resources = NULL,
  ...
) {

  emf_type <- match.arg(emf_type)

  # if resources is NULL, it means all of the type provided
  if (is.null(resources)) {
    table_fun <- switch(
      emf_type,
      'workflow' = public_workflows,
      'tech_doc' = public_tech_docs,
      'model' = public_models,
      'data' = public_data,
      'softwork' = public_softworks
    )
    dot_args <- rlang::dots_list(...)
    resources <- table_fun(.con = dot_args$.con)[[emf_type]]
  }

  if (length(resources) < 1) {
    usethis::ui_oops(crayon::bold("Seems that no {emf_type} resources have been found"))
    return(invisible(FALSE))
  }

  # info
  usethis::ui_info("Updating the following {emf_type} pages: {usethis::ui_value(resources)}")

  # name the resources
  if (is.null(names(resources))) {
    names(resources) <- resources
  }
  # create the safe version of the corresponding create_*_page function
  create_fun <- rlang::eval_tidy(rlang::sym(glue::glue("create_{emf_type}_page")))
  safe_fun <- purrr::possibly(create_fun, otherwise = FALSE, quiet = FALSE)
  # create the pages
  created_pages <- purrr::map(resources, safe_fun, ...)

  failed_pages <- created_pages[created_pages == FALSE]
  if (length(failed_pages) > 0) {
    usethis::ui_oops(c(
      crayon::bold("Oops! The following {emf_type} pages weren't updated:"),
      "{usethis::ui_value(names(failed_pages))}"
    ))
  }
  ok_pages <- created_pages[created_pages == TRUE]
  if (length(ok_pages) > 0) {
    usethis::ui_done(c(
      crayon::bold("Succesfully updated the following {emf_type} pages:"),
      "{usethis::ui_value(names(ok_pages))}"
    ))
  }
  # return all pages and their state (updated or not)
  return(created_pages)
}

#' Update all resource pages
#'
#' Update all resources pages if needed
#'
#' This function update all resources pages that need it, or all if \code{.force = TRUE})
#'
#' @param ... arguments to pass to the create_*_page functions.
#'
#' @return a list of named lists with the resources and if they were updated or not (logical)
#'
#' @examples
#' update_all_resource_pages()
#'
#' @export
update_all_resource_pages <- function(...) {
  c('workflow', 'tech_doc', 'model', 'data', 'softwork') %>%
    magrittr::set_names(., .) %>%
    purrr::map(update_resource_pages_by_type, ...)
}

delete_page <- function(
  resource_id,
  resource_type = c('workflows', 'tech_docs', 'models', 'data', 'softworks'),
  .web_path = Sys.getenv('WEB_PATH')
) {
  page_path <- fs::path(.web_path, 'content', resource_type, resource_id)
  fs::dir_delete(page_path)
}

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

md_content_generator <- function(resource_metadata, dest, category, .external = FALSE) {

  # create the content from the metadata
  md_content <- c(
    "",
    "## Description",
    "",
    resource_metadata$description,
    ""
  )

  if (!isTRUE(.external)) {
    # copy intermediate images
    usethis::ui_info("Copying the intermediate images needed:")
    intermediate_images <- copy_images(folder = emf_temp_folder(), dest, category) %>%
      purrr::walk(usethis::ui_todo)
  } else {
    # copy intermediate images
    usethis::ui_info("Copying the intermediate images needed:")
    intermediate_images <- copy_images(folder = '.', dest, category) %>%
      purrr::walk(usethis::ui_todo)

  }

  return(md_content)

}
