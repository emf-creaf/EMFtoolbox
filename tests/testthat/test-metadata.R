# use_metadata_yml

test_that("use_metadata_yml works as expected", {
  # class
  expect_s3_class(suppressMessages(use_metadata_yml()), 'yml')
  # update and creation
  core_yml <- suppressMessages(use_metadata_yml())
  fbb_yml <- suppressMessages(
    use_metadata_yml(emf_type = 'workflow', edges = c('foo', 'bar', 'baz'), author_aff = 'tururu')
  )
  expect_identical(core_yml$emf_type, '')
  expect_identical(fbb_yml$emf_type, 'workflow')
  expect_identical(fbb_yml$edges, c('foo', 'bar', 'baz'))
  expect_identical(fbb_yml$author_aff, 'tururu')

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
  expect_true(metadata_yml$emf_draft)
})

test_that("metadata collection works properly on workflows", {

  # mocking a workflow project
  workflow_proj <- local_resource_proj(.resource_generator = resource_project_generator('workflow'))

  # connecting to the database
  emf_database <- pool::dbPool(
    drv = RPostgres::Postgres(),
    host = "65.21.120.62",
    user = Sys.getenv('emf_database_user'),
    password = Sys.getenv('emf_database_pass'),
    dbname = 'emf_metadata_dummy'
  )
  withr::defer(pool::poolClose(emf_database))

  # 'metadata.yml' is created
  expect_true(fs::file_exists(fs::path(workflow_proj, 'metadata.yml')))
  # 'metadata.yml' has correct fields
  expect_s3_class((metadata_yml <- read_metadata_file()), 'yml')
  expect_identical(metadata_yml$id, 'dummy_workflow')
  expect_identical(metadata_yml$emf_type, 'workflow')
  expect_false(metadata_yml$emf_draft)
  expect_identical(metadata_yml$authors, 'vgranda')
  expect_identical(metadata_yml$authors_aff, 'CREAF')
  expect_identical(metadata_yml$resource_link, 'workflows/dummy_workflow')
  expect_identical(metadata_yml$thematic, 'dummy')
  expect_identical(metadata_yml$dummy_workflow_field_1, 'dummy')
  expect_identical(metadata_yml$dummy_workflow_field_2, 'dummydummy')

  ## collect dry
  expect_identical((metadata_collected <- collect_metadata(emf_database, .dry = TRUE)), metadata_yml)

  ## collect and update db
  # update tables list
  expect_type((update_tables_list <- prepare_update_metadata_tables(metadata_collected)), 'list')
  expect_named(
    update_tables_list, c(
      'resources_update_table', 'tags_update_table', 'edges_update_table',
      'authors_update_table', 'requirements_update_table'
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
  # is redundant to check this because is done inside the function, but I'll doit anyway with the resources
  # table.
  resources_db_updated <- dplyr::tbl(emf_database, 'resources')
  expect_identical(update_tables_list$resources_updated_table, resources_db_updated)


  ## TODO delete from where dummy_workflow para dejar la bd como estaba
  ## TODO try to test more compare_metadata_tables?? it would be with:
  # bad_resources_updated_table <- update_tables_list$resources_updated_table
  # bad_resources_updated_table$emf_draft <- TRUE
  # and then check if the only TRUE is resources and not the others



})
