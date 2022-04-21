# main function to update web ---------------------------------------------

test_that("update_emf_web works as intended", {

  skip("too big, only from time to time")

  # env var setup
  withr::local_envvar(list(
    PROD_FOLDER = "~/temporal_garbage"
  ))

  # connecting to the database (and close it later)
  emf_database <- metadata_db_con()

  # remotely
  expect_message(
    (res <- update_emf_web(
      .con = emf_database, .force = FALSE, .dry_push = TRUE
    )),
    "EMF web up-to-date"
  )
  expect_false(res)

  expect_message(
    (res <- update_emf_web(
      .con = emf_database, .force = TRUE, .dry_push = TRUE
    )),
    "Connecting to remote"
  )
  expect_true(res)

  # locally
  expect_message(
    (res <- update_emf_web(
      remote = FALSE,
      .con = emf_database, .force = FALSE, .dry_push = TRUE
    )),
    "EMF web up-to-date"
  )
  expect_false(res)

  expect_message(
    (res <- update_emf_web(
      remote = FALSE,
      .con = emf_database, .force = TRUE, .dry_push = TRUE
    )),
    "Copying locally to"
  )
  expect_true(res)


})

# ga updating -------------------------------------------------------------
test_that("update_metadata_collection_ga works as intended", {

  # env var setup
  withr::local_envvar(list(
    EMF_DATABASE = "emf_metadata_dummy"
  ))

  expect_type(
    (updated <- update_metadata_collection_ga(.dry_push = TRUE)),
    'list'
  )

  expect_true(updated$test_dummy_workflow)
  expect_true(updated$test_dummy_softwork)
  expect_true(updated$test_dummy_tech_doc)
  expect_false(updated$emf_creaf_models)
  expect_false(updated$emf_creaf_data)
  expect_false(updated$emf_external_models)
  expect_false(updated$emf_external_data)

})
