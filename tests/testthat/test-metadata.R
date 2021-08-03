# use_metadata_yml

test_that("use_metadata_yml works as expected", {
  expect_s3_class(suppressMessages(use_metadata_yml()), 'yml')
})
