# renders tests

test_that('render_workflow_fragment works as expected', {
  expect_s3_class(render_workflow_fragment('test_dummy_workflow'), 'html')
  expect_error(render_workflow_fragment('test_dummy_workflow_not_existent'), "Message: Not Found")
})

# connecting to the database (and close it later)
emf_database <- metadata_db_con()
withr::defer(pool::poolClose(emf_database))

test_that("create_workflow_page works as expected", {
  # paths
  local_web <- local_temp_proj()
  workflow_path <- fs::path(local_web, 'content', 'workflows', 'test_dummy_workflow')
  md_path <- fs::path(workflow_path, 'index.md')

  expect_true(
    create_workflow_page('test_dummy_workflow', dest = workflow_path, .con = emf_database)
  )
  expect_true(fs::file_exists(md_path))
  expect_true(any(
    (file_lines <- readLines(md_path, encoding = "UTF-8", warn = FALSE)) == 'title: test_dummy_workflow'
  ))
  # expect_identical(
  #   (file_lines <- readLines(md_path, encoding = "UTF-8", warn = FALSE))[2],
  #   'title: test_dummy_workflow'
  # )
  expect_true(any(
    file_lines == "<p>Tururu</p>"
  ))
  # expect_identical(file_lines[33], "<p>Tururu</p>")
  expect_error(
    create_workflow_page('test_dummy_workflow_not_existent', dest = workflow_path, .con = emf_database),
    "not found in public workflows table"
  )
})

test_that("update_workflow_tables works as expected", {
  # paths
  local_web <- local_temp_proj()
  workflow_path <- fs::path(local_web, 'content', 'workflows', 'test_dummy_workflow')
  md_path <- fs::path(workflow_path, 'index.md')

  expect_type(
    (updated_pages <- update_workflow_pages(
      c('test_dummy_workflow', 'non_existent_workflow'), dest = workflow_path, .con = emf_database
    )),
    'list'
  )
  expect_false(all(purrr::flatten_lgl(updated_pages)))
  expect_true(any(purrr::flatten_lgl(updated_pages)))
})
