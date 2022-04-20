# renders tests

temp_web <- emf_temp_folder()

withr::local_envvar(list(
  EMF_DATABASE = "emf_metadata_dummy",
  WEB_PATH = temp_web,
  PROD_FOLDER = "~/temporal_garbage"
))

# connecting to the database (and close it later)
emf_database <- metadata_db_con()


# render utils work as intended -------------------------------------------

test_that("resource last commit utils work as intended", {

  # expect_true(TRUE)

  # check for last commits
  expect_length(
    get_resource_last_commit_from_db('non_existent_resource', .con = emf_database),
    0
  )
  expect_length(
    get_resource_last_commit_from_db('test_dummy_workflow', .con = emf_database),
    1
  )

  expect_true(is.null(get_resource_last_commit_from_repo(
      'non_existent_resource', 'emf-creaf', .branch = 'main'
  )))
  expect_false(is.null(get_resource_last_commit_from_repo(
    'test_dummy_workflow', 'emf-creaf', .branch = 'main'
  )))

  expect_false(
    check_last_commit_for("test_dummy_workflow", .con = emf_database)
  )
  expect_true(
    check_last_commit_for("non_existent_resource", .con = emf_database)
  )

  expect_false(update_resource_last_commit_db(
      'non_existent_resource', org = 'emf-creaf', .con = emf_database
  ))
  expect_false(update_resource_last_commit_db(
    'test_dummy_workflow', org = 'emf-creaf', .con = emf_database
  ))

  # now check pushing commits and check again update_resource_last_commit_db
  clone_from_github('test_dummy_workflow', 'emf-creaf')

  expect_false(
    commit_push_web_repo(
      glue::glue("{Sys.time()} test commit"), Sys.getenv("GITHUB_PAT")
    )
  )

  if (fs::file_exists("commit_test.txt")) {
    fs::file_delete("commit_test.txt")
  }

  writeLines(glue::glue("{Sys.time()} test commit"), "commit_test.txt")

  expect_true(
    commit_push_web_repo(
      glue::glue("{Sys.time()} test commit"), Sys.getenv("GITHUB_PAT")
    )
  )
  expect_true(
    check_last_commit_for("test_dummy_workflow", .con = emf_database)
  )
  expect_true(update_resource_last_commit_db(
    'test_dummy_workflow', org = 'emf-creaf', .con = emf_database
  ))
  expect_false(
    commit_push_web_repo(
      glue::glue("{Sys.time()} test commit"), Sys.getenv("GITHUB_PAT")
    )
  )

  # now test dry mode
  if (fs::file_exists("commit_test.txt")) {
    fs::file_delete("commit_test.txt")
  }

  writeLines(glue::glue("{Sys.time()} test commit"), "commit_test.txt")

  expect_true(
    commit_push_web_repo(
      glue::glue("{Sys.time()} test commit"), Sys.getenv("GITHUB_PAT"),
      .dry_push = TRUE
    )
  )

})

test_that("delete_page helper works", {
  fs::dir_create(
    fs::path(Sys.getenv("WEB_PATH"), 'content', 'workflows', 'tururu')
  )
  expect_true(fs::dir_exists(
    fs::path(Sys.getenv("WEB_PATH"), 'content', 'workflows', 'tururu')
  ))

  expect_true(delete_page('tururu', 'workflows'))
  expect_false(fs::dir_exists(
    fs::path(Sys.getenv("WEB_PATH"), 'content', 'workflows', 'tururu')
  ))
})

test_that("frontmatter and md content helpers work as intended", {
  resource_metadata <- public_workflows(workflow == "test_dummy_workflow")

  expect_true(
    length((frontmatter <- frontmatter_generator(resource_metadata, 'workflows'))) > 0
  )
  expect_true(
    any(stringr::str_detect(frontmatter, "title: Dummy workflow for testing"))
  )
  expect_true(
    any(stringr::str_detect(frontmatter, "categories: workflows"))
  )

  expect_length(
    (md_content <- md_content_generator(resource_metadata)), 5
  )
  expect_true(
    any(stringr::str_detect(
      md_content, "A dummy workflow for testing automatization processes"
    ))
  )
})

test_that("render metadata for models and data works as intended", {

  expect_false(
    render_metadata(
      character(), 'creaf_model',
      .con = emf_database, .web_path = Sys.getenv("WEB_PATH"), .force = FALSE
    )
  )

  expect_false(
    render_metadata(
      'test_dummy_model', 'creaf_model',
      .con = emf_database, .web_path = Sys.getenv("WEB_PATH"), .force = FALSE
    )
  )

  expect_message(
    (rendered <- render_metadata(
      'test_dummy_model', 'creaf_model',
      .con = emf_database, .web_path = Sys.getenv("WEB_PATH"), .force = TRUE
    )),
    "written succesfully"
  )
  # is false because the last commit in the db is not ahead
  expect_false(rendered$test_dummy_model)

  expect_true(
    fs::file_exists(
      fs::path(Sys.getenv("WEB_PATH"), 'content', 'models', 'test_dummy_model', 'index.md')
    )
  )

})

test_that("render_rmd for other resources (workflows, tech_docs, softworks) works as intended", {

  expect_false(
    render_rmd(
      character(), 'workflow',
      .con = emf_database, .web_path = Sys.getenv("WEB_PATH"), .force = FALSE
    )
  )

  expect_false(
    render_rmd(
      'test_dummy_workflow', 'workflow',
      .con = emf_database, .web_path = Sys.getenv("WEB_PATH"), .force = FALSE
    )
  )

  expect_message(
    (rendered <- render_rmd(
      'test_dummy_workflow', 'workflow',
      .con = emf_database, .web_path = Sys.getenv("WEB_PATH"), .force = TRUE
    )),
    "written succesfully"
  )

  # is false because the last commit in the db is not ahead
  expect_false(rendered)

  expect_true(
    fs::file_exists(
      fs::path(
        Sys.getenv("WEB_PATH"), 'content', 'workflows',
        'test_dummy_workflow', 'index.md'
      )
    )
  )

  expect_error(
    render_rmd(
      "non_existent_workflow", 'workflow',
      .con = emf_database, .web_path = Sys.getenv("WEB_PATH"), .force = FALSE
    ),
    "Stopping creation"
  )
})

test_that("wrapper render_resource_pages works as intended", {

  expect_type(
    (rendered <- render_resource_pages(.con = emf_database)),
    'list'
  )
  expect_named(
    rendered,
    c("workflows", "tech_docs", "softworks", "creaf_models", "external_models", "creaf_data", "external_data")
  )
  expect_false(purrr::flatten(rendered) %>% purrr::flatten_lgl() %>% any())

  expect_message(
    (rendered <- render_resource_pages(.con = emf_database, .force = TRUE)),
    "written succesfully"
  )
  expect_type(rendered, 'list')
  expect_named(
    rendered,
    c("workflows", "tech_docs", "softworks", "creaf_models", "external_models", "creaf_data", "external_data")
  )
  expect_false(purrr::flatten(rendered) %>% purrr::flatten_lgl() %>% any())


})

# main function to update web ---------------------------------------------

# TODO, make a test for the main function. probably we need a test_dummy_web repo
test_that("update_emf_web works as intended", {

  # remotely
  expect_message(
    (res <- update_emf_web(
      commit_message = glue::glue("EMFtoolbox testing ({Sys.time()})"),
      prod_folder = "~/temporal_garbage", prod_host = Sys.getenv("PROD_HOST"),
      prod_pass = Sys.getenv("PROD_PASS"),
      .con = emf_database, .force = FALSE, .dry_push = TRUE
    )),
    "Connecting to remote"
  )
  expect_false(res)

  expect_message(
    (res <- update_emf_web(
      commit_message = glue::glue("EMFtoolbox testing ({Sys.time()})"),
      prod_folder = "~/temporal_garbage", prod_host = Sys.getenv("PROD_HOST"),
      prod_pass = Sys.getenv("PROD_PASS"),
      .con = emf_database, .force = TRUE, .dry_push = TRUE
    )),
    "Connecting to remote"
  )
  expect_true(res)

  # locally
  expect_message(
    (res <- update_emf_web(
      prod_folder = "~/temporal_garbage",
      .con = emf_database, .force = FALSE, .dry_push = TRUE
    )),
    "Copying locally to"
  )
  expect_false(res)

  expect_message(
    (res <- update_emf_web(
      prod_folder = "~/temporal_garbage",
      .con = emf_database, .force = TRUE, .dry_push = TRUE
    )),
    "Copying locally to"
  )
  expect_true(res)


})


# test_that('render_rd_fragment works as expected', {
#   tmp_dest <- tempdir()
#
#   expect_type(render_rd_fragment('test_dummy_workflow', tmp_dest, "workflows", .force = TRUE, .con = emf_database), 'character')
#   expect_error(
#     render_rd_fragment('test_dummy_workflow_not_existent', tmp_dest, "workflows", .con = emf_database),
#     "Message: Not Found"
#   )
#   expect_error(render_rd_fragment('test_dummy_softwork', tmp_dest, "softworks", .con = emf_database, .input = 'not_existent_file.Rmd'), 'Rmd file not found')
# })
#
# test_that("create_workflow_page works as expected", {
#   # paths
#   local_web <- local_temp_proj()
#   md_path <- fs::path(local_web, 'content', 'workflows', 'test_dummy_workflow', 'index.md')
#   featured_path <- fs::path(local_web, 'content', 'workflows', 'test_dummy_workflow', 'featured.png')
#   # set local envvars and remove them later
#   withr::local_envvar(list(WEB_PATH = local_web))
#
#   expect_true(
#     create_workflow_page('test_dummy_workflow', .con = emf_database)
#   )
#   expect_true(fs::file_exists(md_path))
#   expect_true(fs::file_exists(featured_path))
#   frontmatter <- rmarkdown::yaml_front_matter(md_path)
#   expect_identical(frontmatter$title, 'Dummy workflow for testing')
#   expect_type(frontmatter$links, 'list')
#   expect_named(frontmatter$links, c('url_doi', 'url_pdf', 'url_source', 'url_docs'))
#   expect_true(any(
#     (file_lines <- readLines(md_path, encoding = "UTF-8", warn = FALSE)) ==
#       "Tururu's"
#   ))
#
#   expect_error(
#     create_workflow_page('test_dummy_softwork', .con = emf_database),
#     "not found in public workflows table"
#   )
#   expect_false(
#     create_workflow_page('test_dummy_workflow', .con = emf_database)
#   )
#   expect_message(
#     (forced_update <- create_workflow_page('test_dummy_workflow', .con = emf_database, .force = TRUE)),
#     "ommiting writting step"
#   )
#   expect_true(forced_update)
# })
#
# test_that("create_softwork_page works as expected", {
#   # paths
#   local_web <- local_temp_proj()
#   pkgdown_path <- fs::path(local_web, 'content', 'software', 'test_dummy_softwork', 'index.md')
#   # set local envvars and remove them later
#   withr::local_envvar(list(WEB_PATH = local_web))
#
#   expect_true(create_softwork_page('test_dummy_softwork', .con = emf_database, .input = 'README.Rmd'))
#   expect_true(fs::file_exists(pkgdown_path))
#   expect_error(
#     create_softwork_page('test_dummy_workflow', .con = emf_database),
#     "not found in public softworks table"
#   )
#   expect_false(create_softwork_page('test_dummy_softwork', .con = emf_database))
#   expect_message(
#     (forced_update <- create_softwork_page('test_dummy_softwork', .con = emf_database, .force = TRUE)),
#     "ommiting writting step"
#   )
#   expect_true(forced_update)
#   frontmatter <- rmarkdown::yaml_front_matter(pkgdown_path)
#   expect_identical(frontmatter$title, 'test_dummy_softwork')
#   expect_type(frontmatter$links, 'list')
#   expect_type(frontmatter$tags, 'character')
#   expect_named(frontmatter$links, c('url_doi', 'url_pdf', 'url_source', 'url_docs'))
#   expect_identical(frontmatter$links$url_doi, "")
#   expect_true(frontmatter$date != '')
#   expect_true(frontmatter$lastmod != '')
# })
#
# test_that("create_tech_doc_page works as expected", {
#   # paths
#   local_web <- local_temp_proj()
#   md_path <- fs::path(local_web, 'content', 'tech_docs', 'test_dummy_tech_doc', 'index.md')
#   # set local envvars and remove them later
#   withr::local_envvar(list(WEB_PATH = local_web))
#
#   expect_true(
#     create_tech_doc_page('test_dummy_tech_doc', .con = emf_database)
#   )
#   expect_true(fs::file_exists(md_path))
#   frontmatter <- rmarkdown::yaml_front_matter(md_path)
#   expect_identical(frontmatter$title, 'Test dummy tech doc')
#   expect_type(frontmatter$links, 'list')
#   expect_named(frontmatter$links, c('url_doi', 'url_pdf', 'url_source', 'url_docs'))
#   expect_identical(frontmatter$links$url_doi, "")
#   expect_true(frontmatter$date != '')
#   expect_true(frontmatter$lastmod != '')
#   expect_true(any(
#     (file_lines <- readLines(md_path, encoding = "UTF-8", warn = FALSE)) ==
#       "Tururu's"
#   ))
#   expect_error(
#     create_tech_doc_page('test_dummy_softwork', .con = emf_database),
#     "not found in public tech_docs table"
#   )
#   expect_false(
#     create_tech_doc_page('test_dummy_tech_doc', .con = emf_database)
#   )
#   expect_message(
#     (forced_update <- create_tech_doc_page('test_dummy_tech_doc', .con = emf_database, .force = TRUE)),
#     "ommiting writting step"
#   )
#   expect_true(forced_update)
# })
#
# test_that("create_model_page works as expected", {
#   # paths
#   local_web <- local_temp_proj()
#   md_path <- fs::path(local_web, 'content', 'models', 'test_dummy_model', 'index.md')
#   featured_path <- fs::path(local_web, 'content', 'models', 'test_dummy_model', 'featured.png')
#   # set local envvars and remove them later
#   withr::local_envvar(list(WEB_PATH = local_web))
#
#   expect_true(
#     create_model_page('test_dummy_model', .con = emf_database)
#   )
#   expect_true(fs::file_exists(md_path))
#   expect_true(fs::file_exists(featured_path))
#   expect_true(any(
#     (file_lines <- readLines(md_path, encoding = "UTF-8", warn = FALSE)) ==
#       'title: Test dummy model'
#   ))
#   expect_true(any(stringr::str_detect(file_lines, "## Description")))
#   expect_error(
#     create_model_page('test_dummy_softwork', .con = emf_database),
#     "not found in public models table"
#   )
#   expect_false(
#     create_model_page('test_dummy_model', .con = emf_database)
#   )
#   expect_message(
#     (forced_update <- create_model_page('test_dummy_model', .con = emf_database, .force = TRUE)),
#     "ommiting writting step"
#   )
#   expect_true(forced_update)
#   frontmatter <- rmarkdown::yaml_front_matter(md_path)
#   expect_identical(frontmatter$title, 'Test dummy model')
#   expect_type(frontmatter$links, 'list')
#   expect_named(frontmatter$links, c('url_doi', 'url_pdf', 'url_source', 'url_docs'))
#   expect_identical(frontmatter$links$url_doi, "")
#   expect_true(frontmatter$date != '')
#   expect_true(frontmatter$lastmod != '')
#
# })
#
# test_that("create_data_page works as expected", {
#   # paths
#   local_web <- local_temp_proj()
#   md_path <- fs::path(local_web, 'content', 'data', 'sapfluxnet', 'index.md')
#   # set local envvars and remove them later
#   withr::local_envvar(list(WEB_PATH = local_web))
#
#   expect_true(
#     create_data_page('sapfluxnet', .con = emf_database)
#   )
#   expect_true(fs::file_exists(md_path))
#   expect_true(any(
#     (file_lines <- readLines(md_path, encoding = "UTF-8", warn = FALSE)) ==
#       'title: SAPFLUXNET'
#   ))
#   expect_true(any(stringr::str_detect(file_lines, "## Description")))
#   expect_error(
#     create_data_page('test_dummy_softwork', .con = emf_database),
#     "not found in public data table"
#   )
#   expect_false(
#     create_data_page('sapfluxnet', .con = emf_database)
#   )
#   expect_message(
#     (forced_update <- create_data_page('sapfluxnet', .con = emf_database, .force = TRUE)),
#     "ommiting writting step"
#   )
#   expect_true(forced_update)
#   frontmatter <- rmarkdown::yaml_front_matter(md_path)
#   expect_identical(frontmatter$title, 'SAPFLUXNET')
#   expect_type(frontmatter$links, 'list')
#   expect_named(frontmatter$links, c('url_doi', 'url_pdf', 'url_source', 'url_docs'))
#   expect_identical(frontmatter$links$url_docs, "http://sapfluxnet.creaf.cat/")
#   expect_true(frontmatter$date != '')
#   expect_true(frontmatter$lastmod != '')
# })
#
# test_that("update_resource_pages_by_type works as expected", {
#   # paths
#   local_web <- local_temp_proj()
#   # set local envvars and remove them later
#   withr::local_envvar(list(WEB_PATH = local_web))
#
#   expect_type(
#     (updated_workflows <- update_resource_pages_by_type(
#       'workflow',
#       c('test_dummy_workflow', 'non_existent_workflow'),
#       .con = emf_database, .force = TRUE
#     )),
#     'list'
#   )
#   expect_false(updated_workflows$non_existent_workflow)
#   expect_true(updated_workflows$test_dummy_workflow)
#
#   expect_type(
#     (updated_softworks <- update_resource_pages_by_type(
#       'softwork',
#       c('test_dummy_softwork', 'non_existent_softwork'),
#       .con = emf_database, .force = TRUE
#     )),
#     'list'
#   )
#   expect_false(updated_softworks$non_existent_softwork)
#   expect_true(updated_softworks$test_dummy_softwork)
#
#   expect_type(
#     (updated_tech_docs <- update_resource_pages_by_type(
#       'tech_doc',
#       c('test_dummy_tech_doc', 'non_existent_tech_doc'),
#       .con = emf_database, .force = TRUE
#     )),
#     'list'
#   )
#   expect_false(updated_tech_docs$non_existent_tech_doc)
#   expect_true(updated_tech_docs$test_dummy_tech_doc)
# })
#
# test_that("update_all_resource_pages works as expected", {
#   skip("too big, only from time to time")
#   # paths
#   local_web <- local_temp_proj()
#   # set local envvars and remove them later
#   withr::local_envvar(list(WEB_PATH = local_web))
#
#   expect_type(
#     (updated_pages <- update_all_resource_pages(.con = emf_database)),
#     'list'
#   )
#
#   expect_named(updated_pages, c('workflow', 'tech_doc', 'model', 'data', 'softwork'))
#   expect_true(all(purrr::flatten_lgl(purrr::flatten(updated_pages))))
# })
