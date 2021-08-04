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

test_that("files are created correctly", {
  local_proj <- local_temp_proj()
  suppressMessages(use_metadata_yml(.write = TRUE))
  # 'metadata.yml' is created
  expect_true(fs::file_exists(fs::path(local_proj, 'metadata.yml')))
  # 'metadata.yml' has correct fields
  metadata_path <- fs::path(local_proj, 'metadata.yml')
  expect_s3_class(ymlthis::read_toml(metadata_path), 'yml')
  expect_identical(ymlthis::read_toml(metadata_path)$ID, '')
  expect_identical(ymlthis::read_toml(metadata_path)$emf_type, '')
  expect_identical(ymlthis::read_toml(metadata_path)$emf_draft, TRUE)
})
