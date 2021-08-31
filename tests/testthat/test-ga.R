# use_emf_ga

test_that("use_emf_ga works as intended", {
  # prepare a local project
  template <- "collect_metadata.yml"
  workflow_proj <- local_resource_proj(.resource_generator = resource_project_generator('workflow'))

  ## expectations
  # template is copied to the correct folder
  expect_true(use_emf_ga(template))
  expect_identical(
    readLines(
      system.file(fs::path("ga_templates", template), package = 'EMFtoolbox'),
      encoding = "UTF-8", warn = FALSE
    ),
    readLines(
      fs::path(".github/workflows", template),
      encoding = "UTF-8", warn = FALSE
    )
  )
  # no problems in overwrite
  expect_true(use_emf_ga(template))
  # error when no template is found
  expect_error(use_emf_ga("this_is_not_a_valid_template_name.yml"), "Template not found")
})
