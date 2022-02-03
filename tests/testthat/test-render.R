# renders tests

# connecting to the database (and close it later)
emf_database <- metadata_db_con()
withr::defer(pool::poolClose(emf_database))

test_that('render_rd_fragment works as expected', {
  tmp_dest <- tempdir()

  expect_type(render_rd_fragment('test_dummy_workflow', tmp_dest, "workflows", .force = TRUE, .con = emf_database), 'character')
  expect_error(
    render_rd_fragment('test_dummy_workflow_not_existent', tmp_dest, "workflows", .con = emf_database),
    "Message: Not Found"
  )
  expect_error(render_rd_fragment('test_dummy_softwork', tmp_dest, "softworks", .con = emf_database, .input = 'not_existent_file.Rmd'), 'Rmd file not found')
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
  frontmatter <- rmarkdown::yaml_front_matter(md_path)
  expect_identical(frontmatter$title, 'Dummy workflow for testing')
  expect_type(frontmatter$links, 'list')
  expect_named(frontmatter$links, c('url_doi', 'url_pdf', 'url_source', 'url_docs'))
  expect_true(any(
    (file_lines <- readLines(md_path, encoding = "UTF-8", warn = FALSE)) ==
      "Tururu's"
  ))

  expect_error(
    create_workflow_page('test_dummy_softwork', .con = emf_database),
    "not found in public workflows table"
  )
  expect_false(
    create_workflow_page('test_dummy_workflow', .con = emf_database)
  )
  expect_message(
    (forced_update <- create_workflow_page('test_dummy_workflow', .con = emf_database, .force = TRUE)),
    "ommiting writting step"
  )
  expect_true(forced_update)
})

test_that("create_softwork_page works as expected", {
  # paths
  local_web <- local_temp_proj()
  pkgdown_path <- fs::path(local_web, 'content', 'software', 'test_dummy_softwork', 'index.md')
  # set local envvars and remove them later
  withr::local_envvar(list(WEB_PATH = local_web))

  expect_true(create_softwork_page('test_dummy_softwork', .con = emf_database, .input = 'README.Rmd'))
  expect_true(fs::file_exists(pkgdown_path))
  expect_error(
    create_softwork_page('test_dummy_workflow', .con = emf_database),
    "not found in public softworks table"
  )
  expect_false(create_softwork_page('test_dummy_softwork', .con = emf_database))
  expect_message(
    (forced_update <- create_softwork_page('test_dummy_softwork', .con = emf_database, .force = TRUE)),
    "ommiting writting step"
  )
  expect_true(forced_update)
  frontmatter <- rmarkdown::yaml_front_matter(pkgdown_path)
  expect_identical(frontmatter$title, 'test_dummy_softwork')
  expect_type(frontmatter$links, 'list')
  expect_named(frontmatter$links, c('url_doi', 'url_pdf', 'url_source', 'url_docs'))
})

test_that("create_tech_doc_page works as expected", {
  # paths
  local_web <- local_temp_proj()
  md_path <- fs::path(local_web, 'content', 'tech_docs', 'test_dummy_tech_doc', 'index.md')
  # set local envvars and remove them later
  withr::local_envvar(list(WEB_PATH = local_web))

  expect_true(
    create_tech_doc_page('test_dummy_tech_doc', .con = emf_database)
  )
  expect_true(fs::file_exists(md_path))
  frontmatter <- rmarkdown::yaml_front_matter(md_path)
  expect_identical(frontmatter$title, 'Test dummy tech doc')
  expect_type(frontmatter$links, 'list')
  expect_named(frontmatter$links, c('url_doi', 'url_pdf', 'url_source', 'url_docs'))
  expect_true(any(
    (file_lines <- readLines(md_path, encoding = "UTF-8", warn = FALSE)) ==
      "Tururu's"
  ))
  expect_error(
    create_tech_doc_page('test_dummy_softwork', .con = emf_database),
    "not found in public tech_docs table"
  )
  expect_false(
    create_tech_doc_page('test_dummy_tech_doc', .con = emf_database)
  )
  expect_message(
    (forced_update <- create_tech_doc_page('test_dummy_tech_doc', .con = emf_database, .force = TRUE)),
    "ommiting writting step"
  )
  expect_true(forced_update)
})

test_that("create_model_page works as expected", {
  # paths
  local_web <- local_temp_proj()
  md_path <- fs::path(local_web, 'content', 'models', 'test_dummy_model', 'index.md')
  # set local envvars and remove them later
  withr::local_envvar(list(WEB_PATH = local_web))

  expect_true(
    create_model_page('test_dummy_model', .con = emf_database)
  )
  expect_true(fs::file_exists(md_path))
  expect_true(any(
    (file_lines <- readLines(md_path, encoding = "UTF-8", warn = FALSE)) ==
      'title: Test dummy model'
  ))
  expect_true(any(stringr::str_detect(file_lines, "## Description")))
  expect_error(
    create_model_page('test_dummy_softwork', .con = emf_database),
    "not found in public models table"
  )
  expect_false(
    create_model_page('test_dummy_model', .con = emf_database)
  )
  expect_message(
    (forced_update <- create_model_page('test_dummy_model', .con = emf_database, .force = TRUE)),
    "ommiting writting step"
  )
  expect_true(forced_update)
  frontmatter <- rmarkdown::yaml_front_matter(md_path)
  expect_identical(frontmatter$title, 'Test dummy model')
  expect_type(frontmatter$links, 'list')
  expect_named(frontmatter$links, c('url_doi', 'url_pdf', 'url_source', 'url_docs'))

})

test_that("create_data_page works as expected", {
  # paths
  local_web <- local_temp_proj()
  md_path <- fs::path(local_web, 'content', 'data', 'test_dummy_data', 'index.md')
  # set local envvars and remove them later
  withr::local_envvar(list(WEB_PATH = local_web))

  expect_true(
    create_data_page('test_dummy_data', .con = emf_database)
  )
  expect_true(fs::file_exists(md_path))
  expect_true(any(
    (file_lines <- readLines(md_path, encoding = "UTF-8", warn = FALSE)) ==
      'title: Test dummy data'
  ))
  expect_true(any(stringr::str_detect(file_lines, "## Description")))
  expect_error(
    create_data_page('test_dummy_softwork', .con = emf_database),
    "not found in public data table"
  )
  expect_false(
    create_data_page('test_dummy_data', .con = emf_database)
  )
  expect_message(
    (forced_update <- create_data_page('test_dummy_data', .con = emf_database, .force = TRUE)),
    "ommiting writting step"
  )
  expect_true(forced_update)
  frontmatter <- rmarkdown::yaml_front_matter(md_path)
  expect_identical(frontmatter$title, 'Test dummy data')
  expect_type(frontmatter$links, 'list')
  expect_named(frontmatter$links, c('url_doi', 'url_pdf', 'url_source', 'url_docs'))
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

  expect_type(
    (updated_tech_docs <- update_resource_pages_by_type(
      'tech_doc',
      c('test_dummy_tech_doc', 'non_existent_tech_doc'),
      .con = emf_database, .force = TRUE
    )),
    'list'
  )
  expect_false(updated_tech_docs$non_existent_tech_doc)
  expect_true(updated_tech_docs$test_dummy_tech_doc)
})

test_that("update_all_resource_pages works as expected", {
  skip("too big, only from time to time")
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
