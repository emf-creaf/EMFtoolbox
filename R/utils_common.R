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
#' @export
pq__text_to_vector_parser <- function(pq__text) {
  stringr::str_remove_all(pq__text, '[{}\"]') %>%
    stringr::str_split(',') %>%
    purrr::flatten_chr()
}

# write file (overwritting existing file). Code for writing taken from usethis::write_over and
# xfun::write_utf8, because usethis::write_over (which would be ideal), requires mandatory user input to
# overwrite, with no way to avoid it.
write_lines_utf8 <- function(lines, path) {
  if (equal_lines_utf8(lines, path)) {
    return(invisible(FALSE))
  }
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  return(invisible(TRUE))
}

equal_lines_utf8 <- function(lines, path) {
  if (!fs::file_exists(path)) {
    return(FALSE)
  }
  identical(readLines(path, warn = FALSE, encoding = 'UTF8'), lines)
}

#' Clone and set project from github
#'
#' Temporal cloned repository to work with the resource (render...)
#'
#' This function creates a temporal folder, clone the desired repository and set the active project on it
#'
#' @param resource_id Resource ID
#' @param .envir envir for \code{withr::defer}, default to \code{parent.frame()}
#' @param .con connection to the db
#' @param .external is the resource external
#' @param .external_id if .external is TRUE, the id of the external data/models repository

create_from_emf_github <- function(
  resource_id, .envir = parent.frame(), .con = NULL, .external = FALSE, .external_id
) {
  temp_proj <- emf_temp_folder()
  withr::defer(fs::dir_delete(temp_proj), envir = .envir)

  # store the current project
  old_project <- usethis::proj_get()

  repository <- resource_id
  if (isTRUE(.external)) {
    repository <- .external_id
  }

  # get the dir
  dir <- fs::path(temp_proj, repository)
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
    repo_spec = glue::glue("emf-creaf/{repository}"),
    destdir = temp_proj,
    fork = FALSE,
    rstudio = FALSE,
    open = FALSE
  )

  # last step, check the database for last commit hash, and if is equal return invisible FALSE,
  # but if not, update the db with the last commit hash
  # connect to database if needed
  if (is.null(.con)) {
    usethis::ui_info("Connection to the database not provided. Attempting to connect using environment variables.")
    .con <- metadata_db_con()
    # close the connection when the function exits
    withr::defer(pool::poolClose(.con))
  }

  return(update_resource_last_commit_db(resource_id, .con))

}

get_resource_last_commit_from_db <- function(resource_id, .con) {
  dplyr::tbl(.con, 'resources_last_commit') %>%
    dplyr::filter(id == resource_id) %>%
    dplyr::collect() %>%
    dplyr::pull(last_commit_hash)
}

update_resource_last_commit_db <- function(resource_id, .con) {
  repo_last_commit <- gert::git_commit_info()$id
  db_last_commit <- get_resource_last_commit_from_db(resource_id, .con)
  if (identical(repo_last_commit, db_last_commit)) {
    usethis::ui_info('{resource_id} last commit up-to-date with database')
    return(invisible(FALSE))
  }

  usethis::ui_info('{resource_id} last commit is ahead of database, updating...')
  update_resource_last_commit_queries <- list(
    remove = glue::glue_sql(
      "DELETE FROM resources_last_commit WHERE id = {resource_id};",
      .con = .con
    ),
    insert = glue::glue_sql(
      "INSERT INTO resources_last_commit (id, last_commit_hash) VALUES ({resource_id}, {repo_last_commit});",
      .con = .con
    )
  )
  purrr::walk(update_resource_last_commit_queries, ~ DBI::dbExecute(.con, .x))
  usethis::ui_done("{resource_id} last commit succesfully updated")
  return(invisible(TRUE))
}

# transform the excel file for external models
external_models_transform <- function(external_models_file = 'ProcessBasedModelsDatabase.xlsx') {

  original_table <- readxl::read_xlsx(
    path = external_models_file,
    sheet = 1, skip = 1, .name_repair = 'universal'
  )

  original_table %>%
    # remove those without DOI or URL, as then we have nothing to offer
    dplyr::filter(
      !(is.na(URL)) | !(is.na(DOI)), External.catalog.entry == 'Y'
    ) %>%
    # create all necessary variables/metadata
    dplyr::mutate(
      # id & title
      id = Model.name.acronym,
      description = Short.description,
      title = dplyr::if_else(
        !is.na(Full.name), glue::glue("{Full.name} ({Model.name.acronym})"), Model.name.acronym
      ),
      # the necessary emf metadata
      emf_type = 'model',
      emf_public = TRUE,
      emf_automatized = TRUE,
      emf_reproducible = FALSE,
      emf_draft = FALSE,
      emf_data_type = 'external_data',
      # get the url and create the metadata var.
      # for that, we choose between url and doi, with preference for the URL
      model_repository = dplyr::if_else(!is.na(URL), URL, DOI),
      # tags, built from model type, level and code language
      tags = purrr::pmap(
        list(Model.type, Level, Code.language.platform),
        .f = function(x,y,z) {return(c(x,y,z))}
      ),
      nodes = list(""),
      authors = list(""),
      requirements = list(""),
      links = purrr::pmap(
        list(DOI, URL),
        .f = function(x,y) {return(list(url_doi = x, url_source = y))}
      )
    ) %>%
    dplyr::select(
      id, description, title, emf_type, emf_public, emf_automatized,
      emf_reproducible, emf_draft, emf_data_type, model_repository, tags,
      nodes, authors, requirements, links
    )
}

is_external <- function(resource_metadata) {
  !is.null(resource_metadata$emf_data_type) &&
    !is.na(resource_metadata$emf_data_type) &&
    resource_metadata$emf_data_type == "external_data"
}

# Capture yaml lines to write
capture_yml <- function(yml) {
  withr::local_envvar(NO_COLOR = TRUE)
  utils::capture.output(print(yml))
}

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

rd_postprocessing <- function(rd_fragment, intermediate_images) {

  # image postprocessing:
  # converting all images calls "![]()" to {{< image >}}
  purrr::map(
    intermediate_images,
    ~ which(stringr::str_detect(rd_fragment, .x))
  ) %>%
    purrr::iwalk(
      function(index, image_path) {
        image_shorthand <- glue::glue(
          '{{{{< figure src="{stringr::str_split(image_path, "/", simplify = TRUE) %>% dplyr::last()}" class="single-image" >}}}}'
        )

        rd_fragment[index] <<- image_shorthand
      }
    )

  rd_fragment

}

nas_to_empty_strings <- function(x) {
  if (length(x) == 1 && is.na(x)){
    x <- ''
  }
  return(x)
}
