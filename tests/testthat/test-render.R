# renders tests

# render utils work as intended -------------------------------------------

test_that("resource last commit utils work as intended", {

  temp_web <- emf_temp_folder()

  withr::local_envvar(list(
    EMF_DATABASE = "emf_metadata_dummy",
    WEB_PATH = temp_web,
    PROD_FOLDER = "~/temporal_garbage"
  ))

  # connecting to the database (and close it later)
  emf_database <- metadata_db_con()

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
    commit_push_repo(
      glue::glue("{Sys.time()} test commit"), Sys.getenv("GITHUB_PAT")
    )
  )

  if (fs::file_exists("commit_test.txt")) {
    fs::file_delete("commit_test.txt")
  }

  writeLines(glue::glue("{Sys.time()} test commit"), "commit_test.txt")

  expect_true(
    commit_push_repo(
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
    commit_push_repo(
      glue::glue("{Sys.time()} test commit"), Sys.getenv("GITHUB_PAT")
    )
  )

  # now test dry mode
  if (fs::file_exists("commit_test.txt")) {
    fs::file_delete("commit_test.txt")
  }

  writeLines(glue::glue("{Sys.time()} test commit"), "commit_test.txt")

  expect_true(
    commit_push_repo(
      glue::glue("{Sys.time()} test commit"), Sys.getenv("GITHUB_PAT"),
      .dry_push = TRUE
    )
  )

})

test_that("delete_page helper works", {

  temp_web <- emf_temp_folder()

  withr::local_envvar(list(
    EMF_DATABASE = "emf_metadata_dummy",
    WEB_PATH = temp_web,
    PROD_FOLDER = "~/temporal_garbage"
  ))

  # connecting to the database (and close it later)
  emf_database <- metadata_db_con()

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
  temp_web <- emf_temp_folder()

  withr::local_envvar(list(
    EMF_DATABASE = "emf_metadata_dummy",
    WEB_PATH = temp_web,
    PROD_FOLDER = "~/temporal_garbage"
  ))

  # connecting to the database (and close it later)
  emf_database <- metadata_db_con()

  resource_metadata <- public_workflows(
    workflow == "test_dummy_workflow", .con = emf_database
  )

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

  temp_web <- emf_temp_folder()

  withr::local_envvar(list(
    EMF_DATABASE = "emf_metadata_dummy",
    WEB_PATH = temp_web,
    PROD_FOLDER = "~/temporal_garbage"
  ))

  # connecting to the database (and close it later)
  emf_database <- metadata_db_con()

  expect_false(
    render_metadata(
      character(), 'creaf_model',
      .con = emf_database, .web_path = Sys.getenv("WEB_PATH"), .force = FALSE
    )
  )

  # as web path is an empty temp folder, we expect the render to occur as
  # there is no index.md
  expect_message(
    render_metadata(
      'test_dummy_model', 'creaf_model',
      .con = emf_database, .web_path = Sys.getenv("WEB_PATH"), .force = FALSE
    ),
    "written succesfully"
  )

  # now there is index.md, so the render should stop before doing anything
  expect_message(
    render_metadata(
      'test_dummy_model', 'creaf_model',
      .con = emf_database, .web_path = Sys.getenv("WEB_PATH"), .force = FALSE
    ),
    "Nothing to render, finishing..."
  )

  # if we force we get again to write
  expect_message(
    (rendered <- render_metadata(
      'test_dummy_model', 'creaf_model',
      .con = emf_database, .web_path = Sys.getenv("WEB_PATH"), .force = TRUE
    )),
    "written succesfully"
  )
  # is false because the last commit in the db is not ahead, but the writing
  # was forced
  expect_false(rendered$test_dummy_model)

  expect_true(
    fs::file_exists(
      fs::path(Sys.getenv("WEB_PATH"), 'content', 'models', 'test_dummy_model', 'index.md')
    )
  )

})

test_that("render_rmd for other resources (workflows, tech_docs, softworks) works as intended", {

  temp_web <- emf_temp_folder()

  withr::local_envvar(list(
    EMF_DATABASE = "emf_metadata_dummy",
    WEB_PATH = temp_web,
    PROD_FOLDER = "~/temporal_garbage"
  ))

  # connecting to the database (and close it later)
  emf_database <- metadata_db_con()

  expect_false(
    render_rmd(
      character(), 'workflow',
      .con = emf_database, .web_path = Sys.getenv("WEB_PATH"), .force = FALSE
    )
  )

  expect_message(
    render_rmd(
      'test_dummy_workflow', 'workflow',
      .con = emf_database, .web_path = Sys.getenv("WEB_PATH"), .force = FALSE
    ),
    "written succesfully"
  )

  expect_message(
    render_rmd(
      'test_dummy_workflow', 'workflow',
      .con = emf_database, .web_path = Sys.getenv("WEB_PATH"), .force = FALSE
    ),
    "Nothing to render, finishing..."
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

  temp_web <- emf_temp_folder()

  withr::local_envvar(list(
    EMF_DATABASE = "emf_metadata_dummy",
    WEB_PATH = temp_web,
    PROD_FOLDER = "~/temporal_garbage"
  ))

  # connecting to the database (and close it later)
  emf_database <- metadata_db_con()

  expect_type(
    (rendered <- render_resource_pages(.con = emf_database)),
    'list'
  )
  expect_named(
    rendered,
    c("workflows", "tech_docs", "softworks", "creaf_models", "external_models", "creaf_data", "external_data")
  )
  expect_false(purrr::flatten(rendered) |> purrr::flatten_lgl() |> any())

  expect_message(
    (rendered <- render_resource_pages(.con = emf_database, .force = TRUE)),
    "written succesfully"
  )
  expect_type(rendered, 'list')
  expect_named(
    rendered,
    c("workflows", "tech_docs", "softworks", "creaf_models", "external_models", "creaf_data", "external_data")
  )

  expect_false(purrr::flatten(rendered) |> purrr::flatten_lgl() |> any())
  # expect_identical(rendered, c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE))
  # expect_true(purrr::flatten(rendered) |> purrr::flatten_lgl() |> all())


})
