# use_metadata_yml

withr::local_envvar(list(
  EMF_DATABASE = "emf_metadata_dummy",
  WEB_PATH = "/home/malditobarbudo/data/CREAF/projects/emf/emf_web/emf_hugo_theme_site_example"
))

test_that("use_metadata_yml works as expected", {
  # class
  expect_s3_class(suppressMessages(use_metadata_yml()), 'yml')
  # update and creation
  core_yml <- suppressMessages(use_metadata_yml())
  fbb_yml <- suppressMessages(
    use_metadata_yml(emf_type = 'workflow', nodes = c('foo', 'bar', 'baz'), links = list(url_doi = 'example.com'))
  )
  expect_identical(core_yml$emf_type, '')
  expect_identical(fbb_yml$emf_type, 'workflow')
  expect_identical(fbb_yml$nodes, c('foo', 'bar', 'baz'))
  expect_identical(fbb_yml$links$url_doi, 'example.com')
  expect_identical(fbb_yml$date, as.character(Sys.Date()))
  expect_identical(fbb_yml$date_lastmod, as.character(Sys.Date()))

  fbb_yml <- suppressMessages(
    use_metadata_yml(emf_type = 'data', nodes = c('foo', 'bar', 'baz'), links = list(url_doi = 'example.com'))
  )
  expect_identical(core_yml$emf_type, '')
  expect_identical(fbb_yml$emf_type, 'data')
  expect_identical(fbb_yml$nodes, c('foo', 'bar', 'baz'))
  expect_identical(fbb_yml$links$url_doi, 'example.com')
  expect_identical(fbb_yml$date, as.character(Sys.Date()))
  expect_identical(fbb_yml$date_lastmod, as.character(Sys.Date()))

})

test_that("files are created correctly", {
  local_proj <- local_temp_proj()
  suppressMessages(use_metadata_yml(.write = TRUE))
  # 'metadata.yml' is created
  expect_true(fs::file_exists(fs::path(local_proj, 'metadata.yml')))
  # 'metadata.yml' has correct fields
  metadata_path <- fs::path(local_proj, 'metadata.yml')
  expect_s3_class((metadata_yml <- read_metadata_file(metadata_path)), 'yml')
  expect_identical(metadata_yml$id, '')
  expect_identical(metadata_yml$emf_type, '')
  expect_identical(metadata_yml$links$url_doi, '')
  expect_true(metadata_yml$emf_draft)
})

# connecting to the database (and close it later)
emf_database <- metadata_db_con()

# deferring the db cleaning
# When we finish the unit test, we remove the dummy resource from the db
withr::defer(
  purrr::walk(
    c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork'),
    delete_resource_from_db, con = emf_database
  )
)
# And also, we need to remove the dummy fields from the resources table
withr::defer(
  purrr::walk(
    c('workflow', 'tech_doc', 'model', 'data', 'softwork'),
    ~ remove_dummy_columns(emf_database, .x)
  )
)

test_that("metadata collection helpers work properly", {

  # mocking a workflow project
  workflow_proj <- local_resource_proj(.resource_generator = resource_project_generator('workflow'))

  # defers for when finishing
  # When we finish the unit test, we remove the dummy resource from the db
  withr::defer(delete_resource_from_db(metadata_collected$id, emf_database))
  # And also, we need to remove the dummy fields from the resources table
  withr::defer(remove_dummy_columns(emf_database, 'workflow'))

  # 'metadata.yml' is created
  expect_true(fs::file_exists(fs::path(workflow_proj, 'metadata.yml')))
  # 'metadata.yml' has correct fields
  expect_s3_class((metadata_yml <- read_metadata_file()), 'yml')
  expect_identical(metadata_yml$id, 'dummy_workflow')
  expect_identical(metadata_yml$emf_type, 'workflow')
  expect_false(metadata_yml$emf_draft)
  expect_identical(metadata_yml$authors, c('vgranda', 'mr_dummy', 'emf', 'rmolowni', 'mcaceres'))
  expect_identical(metadata_yml$resource_link, 'workflows/dummy_workflow')
  expect_identical(metadata_yml$links$url_doi, 'example.com')
  expect_identical(metadata_yml$thematic, 'dummy')
  expect_identical(metadata_yml$dummy_workflow_field_1, 'dummy')
  expect_identical(metadata_yml$dummy_workflow_field_2, 'dummydummy')
  # read_metadata_file throws error when incorrect (duplicated fields)
  yml_contents <- readLines(fs::path(workflow_proj, 'metadata.yml'))
  yml_end_index <- length(yml_contents)
  writeLines(
    c(yml_contents[1:7], yml_contents[6:yml_end_index]),
    fs::path(workflow_proj, 'bad_metadata.yml')
  )
  expect_error(read_metadata_file(fs::path(workflow_proj, 'bad_metadata.yml')), "Duplicate map key")

  ## collect dry
  expect_identical((metadata_collected <- collect_metadata(emf_database, .dry = TRUE)), metadata_yml)

  ## collect and update db
  # update tables list
  expect_type((update_tables_list <- prepare_update_metadata_tables(metadata_collected)), 'list')
  expect_named(
    update_tables_list, c(
      'resources_update_table', 'resource_tags_update_table', 'nodes_update_table',
      'resource_authors_update_table', 'requirements_update_table', 'links_update_table'
    )
  )
  purrr::map(.x = update_tables_list, .f = expect_s3_class, class = 'tbl_df')
  purrr::map(.x = update_tables_list, .f = ~ expect_true(nrow(.x) > 0))

  # comparing tables
  expect_type(
    (update_info <- compare_metadata_tables(update_tables_list, emf_database, metadata_collected$id)),
    'list'
  )
  expect_named(
    update_info,
    c('valid_update_list', 'resources_columns_to_add', 'resources_columns_to_add_type')
  )
  expect_true(all(update_info$valid_update_list))
  purrr::map(.x = update_info[2:3], .f = expect_length, n = 2)
  expect_identical(update_info$resources_columns_to_add_type, rep('TEXT', 2))

  # update metadata queries execution
  expect_true(update_metadata_queries(update_tables_list, update_info, emf_database, metadata_collected))
  # is redundant to check this because is done inside the function, but I'll do it anyway with the resources
  # table:
  resources_db_updated <- dplyr::tbl(emf_database, 'resources') %>%
    dplyr::filter(id == 'dummy_workflow') %>%
    dplyr::collect()
  expect_identical(metadata_collected$id, resources_db_updated$id)
  expect_identical(metadata_collected$emf_type, resources_db_updated$emf_type)
  expect_identical(metadata_collected$emf_draft, resources_db_updated$emf_draft)
  expect_identical(metadata_collected$resource_link, resources_db_updated$resource_link)
  expect_identical(metadata_collected$thematic, resources_db_updated$thematic)
  expect_identical(metadata_collected$dummy_workflow_field_1, resources_db_updated$dummy_workflow_field_1)
  expect_identical(metadata_collected$dummy_workflow_field_2, resources_db_updated$dummy_workflow_field_2)
  resource_authors_db_updated <- dplyr::tbl(emf_database, 'resource_authors') %>%
    dplyr::filter(id == 'dummy_workflow') %>%
    dplyr::select(-resource_authors_pk) %>%
    dplyr::collect()
  expect_setequal(metadata_collected$authors, resource_authors_db_updated$author_id)
  resource_tags_db_updated <- dplyr::tbl(emf_database, 'resource_tags') %>%
    dplyr::filter(id == 'dummy_workflow') %>%
    dplyr::select(-resource_tags_pk) %>%
    dplyr::collect()
  expect_setequal(metadata_collected$tags, resource_tags_db_updated$tag_id)

  # if we try again, compare metadata tables should return invisible FALSE
  expect_false(collect_metadata(emf_database, .dry = FALSE))

  # if we delete the resource, it must reflect on the database
  expect_true(delete_resource_from_db(metadata_collected$id, emf_database))
  # but if we try again the resource is already deleted
  expect_false(suppressWarnings(delete_resource_from_db(metadata_collected$id, emf_database)))
  # and finally, after removing it, all the process should work from the wrapper function
  expect_true(collect_metadata(emf_database, .dry = FALSE))


  ## TODO try to test more compare_metadata_tables?? it would be with:
  # bad_resources_updated_table <- update_tables_list$resources_updated_table
  # bad_resources_updated_table$emf_draft <- TRUE
  # and then check if the only TRUE is resources and not the others
})

test_that("collect from workflow works", {
  # mocking different resources and collect metadata
  workflow_proj <- local_resource_proj(.resource_generator = resource_project_generator('workflow'))
  expect_true(collect_metadata(emf_database, .dry = FALSE))
})

test_that("collect from tech_doc works", {
  # mocking different resources and collect metadata
  tech_doc_proj <- local_resource_proj(.resource_generator = resource_project_generator('tech_doc'))
  expect_true(collect_metadata(emf_database, .dry = FALSE))
})

test_that("collect from model works", {
  # mocking different resources and collect metadata
  model_proj <- local_resource_proj(.resource_generator = resource_project_generator('model'))
  expect_true(collect_metadata(emf_database, .dry = FALSE))
})

test_that("collect from data works", {
  # mocking different resources and collect metadata
  data_proj <- local_resource_proj(.resource_generator = resource_project_generator('data'))
  expect_true(collect_metadata(emf_database, .dry = FALSE))
})

test_that("collect from softwork works", {
  # mocking different resources and collect metadata
  softwork_proj <- local_resource_proj(.resource_generator = resource_project_generator('softwork'))
  expect_true(collect_metadata(emf_database, .dry = FALSE))
})

test_that("updated database is correct", {

  expect_s3_class(
    (resources_db <- dplyr::tbl(emf_database, 'resources') %>%
       dplyr::filter(
         id %in% c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
       ) %>%
       dplyr::collect()),
    'tbl_df'
  )
  expect_equal(nrow(resources_db), 5)
  expect_setequal(
    resources_db$id,
    c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
  )
  expect_true(!all(resources_db$emf_draft))
  expect_setequal(resources_db$emf_type, c('workflow', 'tech_doc', 'model', 'data', 'softwork'))
  expect_setequal(resources_db$emf_reproducible, c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_true(all(
    c(
      'dummy_workflow_field_1', 'dummy_workflow_field_2',
      'dummy_tech_doc_field_1', 'dummy_tech_doc_field_2',
      'dummy_model_field_1', 'dummy_model_field_2',
      'dummy_data_field_1', 'dummy_data_field_2',
      'dummy_softwork_field_1', 'dummy_softwork_field_2'
    ) %in% names(resources_db)
  ))

  expect_s3_class(
    (tags_db <- dplyr::tbl(emf_database, 'resource_tags') %>%
       dplyr::filter(
         id %in% c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
       ) %>%
       dplyr::collect()),
    'tbl_df'
  )
  expect_equal(nrow(tags_db), 15) # 5 resources x 3 tags each
  expect_setequal(
    unique(tags_db$id),
    c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
  )
  expect_true(all(tags_db$tag_id %in% c('dummy', 'tururu', 'larara')))

  expect_s3_class(
    (nodes_db <- dplyr::tbl(emf_database, 'nodes') %>%
       dplyr::filter(
         id %in% c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
       ) %>%
       dplyr::collect()),
    'tbl_df'
  )
  expect_equal(nrow(nodes_db), 20) # 5 resources x 4 nodes each
  expect_setequal(
    unique(nodes_db$id),
    c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
  )
  expect_true(all(stringr::str_detect(
    nodes_db$node, 'dummy_workflow|dummy_tech_doc|dummy_model|dummy_data|dummy_softwork'
  )))

  expect_s3_class(
    (authors_db <- dplyr::tbl(emf_database, 'resource_authors') %>%
       dplyr::filter(
         id %in% c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
       ) %>%
       dplyr::collect()),
    'tbl_df'
  )
  expect_equal(nrow(authors_db), 25) # 5 resources x 5 authors each
  expect_setequal(
    unique(authors_db$id),
    c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
  )
  expect_setequal(
    unique(authors_db$author_id),
    c('vgranda', 'mr_dummy', 'emf', 'rmolowni', 'mcaceres')
  )

  expect_s3_class(
    (requirements_db <- dplyr::tbl(emf_database, 'requirements') %>%
       dplyr::filter(
         id %in% c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
       ) %>%
       dplyr::collect()),
    'tbl_df'
  )
  expect_equal(nrow(requirements_db), 5) # 5 resources x 1 requirements each
  expect_setequal(
    unique(requirements_db$id),
    c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
  )
  expect_true(all(requirements_db$requirement == ''))
})

test_that("use_public_table works as expected", {
  expect_s3_class((emf_public_workflows <- use_public_table('workflows', .con = emf_database)), 'tbl')
  expect_true(all(
    c("workflow", "author", "tag", "emf_draft", "date", "date_lastmod", "description") %in%
      names(emf_public_workflows)
  ))
  expect_s3_class((emf_public_models <- use_public_table('models', .con = emf_database)), 'tbl')
  expect_true(all(
    c("model", "author", "tag", "emf_draft", "date", "date_lastmod", "description", "model_repository") %in%
      names(emf_public_models)
  ))
  expect_s3_class((emf_public_softworks <- use_public_table('softworks', .con = emf_database)), 'tbl')
  expect_true(all(
    c("softwork", "author", "tag", "emf_draft", "date", "date_lastmod", "description") %in%
      names(emf_public_softworks)
  ))
  expect_s3_class((all_public_resources <- use_public_table('all', .con = emf_database)), 'tbl')
  expect_true(all(
    c("id", "emf_type", "emf_draft", "date", "date_lastmod",
      "description", "title", "data_repository", "model_repository") %in%
      names(all_public_resources)
  ))
  # resource specific methods works
  expect_setequal(emf_public_workflows$workflow, public_workflows(.con = emf_database)$workflow)
  expect_setequal(emf_public_workflows$title, public_workflows(.con = emf_database)$title)
  # all resources retrieved are public:
  expect_true(all(emf_public_softworks$emf_public))
  expect_true(all(emf_public_workflows$emf_public))
  expect_true(all(emf_public_models$emf_public))
  expect_true(all(all_public_resources$emf_public))
})

test_that("collecting external models works", {

  temp_proj <- emf_temp_folder()
  withr::defer(fs::dir_delete(temp_proj))

  # store the current project
  old_project <- usethis::proj_get()

  repository <- "emf_external_models"

  # get the dir
  dir <- fs::path(temp_proj, repository)
  fs::dir_create(dir)
  # create the dir, go to the folder and do whatever it needs, but always back again to the original one when
  # finish (defer)
  setwd(dir)
  withr::defer(setwd(old_project))
  # switch to new project
  usethis::proj_set(dir, force = TRUE)
  withr::defer(usethis::proj_set(old_project, force = TRUE))

  # create the repo based on resource_id
  usethis::create_from_github(
    repo_spec = glue::glue("emf-creaf/{repository}"),
    destdir = temp_proj,
    fork = FALSE,
    rstudio = FALSE,
    open = FALSE
  )

  expected_names <- c(
    "id", "title", "emf_type", "emf_public", "emf_automatized",
    "emf_reproducible", "emf_draft", "emf_data_type", "model_repository", "tags",
    "nodes", "authors", "requirements", "links", "description"
  )

  expect_s3_class((external_models_table <- external_models_transform()), 'tbl_df')
  expect_named(external_models_table, expected_names, ignore.order = TRUE)
  expect_s3_class(collect_metadata(.dry = TRUE, yml_file = external_models_table[1,]), 'yml')

})

test_that("collecting external data works", {

  temp_proj <- emf_temp_folder()
  withr::defer(fs::dir_delete(temp_proj))

  # store the current project
  old_project <- usethis::proj_get()

  repository <- "emf_external_data"

  # get the dir
  dir <- fs::path(temp_proj, repository)
  fs::dir_create(dir)
  # create the dir, go to the folder and do whatever it needs, but always back again to the original one when
  # finish (defer)
  setwd(dir)
  withr::defer(setwd(old_project))
  # switch to new project
  usethis::proj_set(dir, force = TRUE)
  withr::defer(usethis::proj_set(old_project, force = TRUE))

  # create the repo based on resource_id
  usethis::create_from_github(
    repo_spec = glue::glue("emf-creaf/{repository}"),
    destdir = temp_proj,
    fork = FALSE,
    rstudio = FALSE,
    open = FALSE
  )

  expected_names <- c(
    "id", "title", "emf_type", "emf_public", "emf_automatized",
    "emf_reproducible", "emf_draft", "emf_data_type", "data_repository", "tags",
    "nodes", "authors", "requirements", "links", "description"
  )

  expect_s3_class((external_data_table <- external_data_transform()), 'tbl_df')
  expect_named(external_data_table, expected_names, ignore.order = TRUE)
  expect_s3_class(collect_metadata(.dry = TRUE, yml_file = external_data_table[1,]), 'yml')

})
