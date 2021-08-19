# use_emf_ga

test_that("use_emf_ga works as intended", {
  # prepare a local project
  template <- "collect_metadata.yml"
  workflow_proj <- local_resource_proj(.resource_generator = resource_project_generator('workflow'))

  ## expectations
  # template is copied to the correct folder
  expect_true(use_emf_ga(template))
  expect_identical(
    base::readLines(
      system.file(fs::path("ga_templates", template), package = 'EMFtoolbox'),
      encoding = "UTF-8", warn = FALSE
    ),
    base::readLines(
      fs::path(".github/workflows", template),
      encoding = "UTF-8", warn = FALSE
    )
  )
  # no problems in overwrite
  expect_true(use_emf_ga(template))
})