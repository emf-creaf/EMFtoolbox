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

# connecting to the database (and close it later)
emf_database <- metadata_db_con()
withr::defer(pool::poolClose(emf_database))

# deferring the db cleaning
# When we finish the unit test, we remove the dummy resource from the db
withr::defer(
  purrr::walk(
    c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork'),
    ~ delete_resource(emf_database, .x)
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
  withr::defer(delete_resource(emf_database, metadata_collected$id))
  # And also, we need to remove the dummy fields from the resources table
  withr::defer(remove_dummy_columns(emf_database, 'workflow'))

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

  # if we try again, compare metadata tables should return invisible FALSE
  expect_false(collect_metadata(emf_database, .dry = FALSE))

  # if we delete the resource, it must reflect on the database
  expect_true(delete_resource(emf_database, metadata_collected$id))
  # but if we try again the resource is already deleted
  expect_false(delete_resource(emf_database, metadata_collected$id))
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
  expect_identical(
    resources_db$id,
    c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
  )
  expect_true(!all(resources_db$emf_draft))
  expect_identical(resources_db$emf_type, c('workflow', 'tech_doc', 'model', 'data', 'softwork'))
  expect_identical(resources_db$emf_reproducible, c(TRUE, TRUE, FALSE, FALSE, FALSE))
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
    (tags_db <- dplyr::tbl(emf_database, 'tags') %>%
       dplyr::filter(
         id %in% c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
       ) %>%
       dplyr::collect()),
    'tbl_df'
  )
  expect_equal(nrow(tags_db), 15) # 5 resources x 3 tags each
  expect_identical(
    unique(tags_db$id),
    c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
  )
  expect_true(all(tags_db$tag %in% c('dummy', 'tururu', 'larara')))

  expect_s3_class(
    (edges_db <- dplyr::tbl(emf_database, 'edges') %>%
       dplyr::filter(
         id %in% c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
       ) %>%
       dplyr::collect()),
    'tbl_df'
  )
  expect_equal(nrow(edges_db), 20) # 5 resources x 4 edges each
  expect_identical(
    unique(edges_db$id),
    c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
  )
  expect_true(all(stringr::str_detect(
    edges_db$edge, 'dummy_workflow|dummy_tech_doc|dummy_model|dummy_data|dummy_softwork'
  )))

  expect_s3_class(
    (authors_db <- dplyr::tbl(emf_database, 'authors') %>%
       dplyr::filter(
         id %in% c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
       ) %>%
       dplyr::collect()),
    'tbl_df'
  )
  expect_equal(nrow(authors_db), 5) # 5 resources x 1 authors each
  expect_identical(
    unique(authors_db$id),
    c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
  )
  expect_true(all(authors_db$author == 'vgranda'))
  expect_true(all(authors_db$author_aff == 'CREAF'))

  expect_s3_class(
    (requirements_db <- dplyr::tbl(emf_database, 'requirements') %>%
       dplyr::filter(
         id %in% c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
       ) %>%
       dplyr::collect()),
    'tbl_df'
  )
  expect_equal(nrow(requirements_db), 5) # 5 resources x 1 requirements each
  expect_identical(
    unique(requirements_db$id),
    c('dummy_workflow', 'dummy_tech_doc', 'dummy_model', 'dummy_data', 'dummy_softwork')
  )
  expect_true(all(requirements_db$requirement == ''))
})