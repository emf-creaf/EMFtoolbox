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
