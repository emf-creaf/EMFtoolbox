local_temp_proj <- function(dir = emf_temp_folder(), env = parent.frame()) {

  # store the current project
  old_project <- usethis::proj_get()

  # create new folder and project, remove it when finished
  usethis::create_project(dir, open = FALSE)
  withr::defer(fs::dir_delete(dir), envir = env)

  # go to the folder and do whatever it needs, but always back again to the original one when finish (defer)
  setwd(dir)
  withr::defer(setwd(old_project), envir = env)

  # switch to new project
  usethis::proj_set(dir)
  withr::defer(usethis::proj_set(old_project, force = TRUE), envir = env)

  return(dir)
}

local_resource_proj <- function(dir = emf_temp_folder(), env = parent.frame(), .resource_generator) {

  # store the current project
  old_project <- usethis::proj_get()

  # create new folder and project, remove it when finished
  usethis::create_project(dir, open = FALSE)
  withr::defer(fs::dir_delete(dir), envir = env)

  # go to the folder and do whatever it needs, but always back again to the original one when finish (defer)
  setwd(dir)
  withr::defer(setwd(old_project), envir = env)

  # switch to new project
  usethis::proj_set(dir)
  withr::defer(usethis::proj_set(old_project, force = TRUE), envir = env)

  # Do whatever we need in the new project

  # store the resource function and create the
  rlang::eval_tidy(.resource_generator)

  return(dir)
}

resource_project_generator <- function(resource = c('workflow', 'tech_doc', 'data', 'model', 'softwork')) {
  resource <- match.arg(resource)
  resource_fun <- glue::glue("use_{resource}_yml")
  resources_available <- c('dummy_data', 'dummy_tech_doc', 'dummy_softwork', 'dummy_model', 'dummy_workflow')
  new_md_field_1 <- glue::glue("dummy_{resource}_field_1")
  new_md_field_2 <- glue::glue("dummy_{resource}_field_2")
  # build the resource call with the selected resource
  resource_call <- rlang::call2(
    .ns = 'EMFtoolbox',
    .fn = resource_fun,
    id = glue::glue("dummy_{resource}"),
    emf_draft = FALSE,
    nodes = resources_available[-which(stringr::str_detect(resources_available, resource))],
    date = as.character(Sys.Date()),
    date_lastmod = as.character(Sys.Date()),
    authors = list('vgranda', 'mr_dummy', 'emf', 'rmolowni', 'mcaceres'),
    authors_aff = list("CREAF", "Dummy Uni", "CREAF", "CREAF", "CREAF"),
    resource_link = glue::glue("{resource}s/dummy_{resource}"),
    thematic = 'dummy',
    tags = c('dummy', 'tururu', 'larara'),
    requirements = '',
    links = list(
      url_doi = 'example.com',
      url_pdf = 'example.pdf',
      url_source = 'example.com',
      url_docs = 'example.com'
    ),
    !! new_md_field_1 := 'dummy',
    !! new_md_field_2 := 'dummydummy',
    .write = TRUE
  )
  return(resource_call)
}

remove_dummy_columns <- function(con, resource) {
  remove_dummy_cols_query <- glue::glue_sql(
    "ALTER TABLE resources {columns*};",
    columns = glue::glue_sql(
      "DROP COLUMN {`resource_dummy_cols`}",
      resource_dummy_cols = c(
        glue::glue('dummy_{resource}_field_1'), glue::glue('dummy_{resource}_field_2')
      ),
      .con = con
    ),
    .con = con
  )
  DBI::dbExecute(con, remove_dummy_cols_query)
}
