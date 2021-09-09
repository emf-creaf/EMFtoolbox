# renders tests

# connecting to the database (and close it later)
emf_database <- metadata_db_con()
withr::defer(pool::poolClose(emf_database))

test_that('render_html_fragment works as expected', {
  expect_type(render_html_fragment('test_dummy_workflow', .force = TRUE, .con = emf_database), 'character')
  expect_error(
    render_html_fragment('test_dummy_workflow_not_existent', .con = emf_database),
    "Message: Not Found"
  )
  expect_error(render_html_fragment('test_dummy_softwork', .con = emf_database), 'No test_dummy_softwork.Rmd')
})

test_that("create_workflow_page works as expected", {
  # paths
  local_web <- local_temp_proj()
  md_path <- fs::path(local_web, 'content', 'workflows', 'test_dummy_workflow', 'index.md')
  # set local envvars and remove them later
  withr::local_envvar(list(WEB_PATH = local_web))

  expect_true(
    create_workflow_page('test_dummy_workflow', .con = emf_database)
  )
  expect_true(fs::file_exists(md_path))
  expect_true(any(
    (file_lines <- readLines(md_path, encoding = "UTF-8", warn = FALSE)) ==
      'title: Dummy workflow for testing'
  ))
  expect_true(any(
    file_lines == "<p>Tururu’s</p>"
  ))
  expect_error(
    create_workflow_page('test_dummy_softwork', .con = emf_database),
    "not found in public workflows table"
  )
  expect_false(
    create_workflow_page('test_dummy_workflow', .con = emf_database)
  )
})

test_that("create_softwork_page works as expected", {
  # paths
  local_web <- local_temp_proj()
  pkgdown_path <- fs::path(local_web, 'static', 'softworks', 'test_dummy_softwork')
  # set local envvars and remove them later
  withr::local_envvar(list(WEB_PATH = local_web))

  expect_true(create_softwork_page('test_dummy_softwork', .con = emf_database))
  expect_true(fs::dir_exists(pkgdown_path))
  expect_true(fs::file_exists(fs::path(pkgdown_path, 'index.html')))
  expect_error(
    create_softwork_page('test_dummy_workflow', .con = emf_database),
    "not found in public softworks table"
  )
  expect_false(create_softwork_page('test_dummy_softwork', .con = emf_database))
})

test_that("update_resource_pages_by_type works as expected", {
  # paths
  local_web <- local_temp_proj()
  # set local envvars and remove them later
  withr::local_envvar(list(WEB_PATH = local_web))

  expect_type(
    (updated_workflows <- update_resource_pages_by_type(
      'workflow',
      c('test_dummy_workflow', 'non_existent_workflow'),
      .con = emf_database, .force = TRUE
    )),
    'list'
  )
  expect_false(updated_workflows$non_existent_workflow)
  expect_true(updated_workflows$test_dummy_workflow)

  expect_type(
    (updated_softworks <- update_resource_pages_by_type(
      'softwork',
      c('test_dummy_softwork', 'non_existent_softwork'),
      .con = emf_database, .force = TRUE
    )),
    'list'
  )
  expect_false(updated_softworks$non_existent_softwork)
  expect_true(updated_softworks$test_dummy_softwork)
})

test_that("update_all_resource_pages works as expected", {
  # paths
  local_web <- local_temp_proj()
  # set local envvars and remove them later
  withr::local_envvar(list(WEB_PATH = local_web))

  expect_type(
    (updated_pages <- update_all_resource_pages(.con = emf_database)),
    'list'
  )

  expect_named(updated_pages, c('workflow', 'tech_doc', 'model', 'data', 'softwork'))
  expect_true(all(purrr::flatten_lgl(purrr::flatten(updated_pages))))
})
