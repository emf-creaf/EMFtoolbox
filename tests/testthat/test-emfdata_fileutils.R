test_that("read_emfdata works as intended", {
  # data for tests
  test_data_path <- "vgranda/"
  test_file <- "public_test.csv"

  # assertions
  expect_error(read_emfdata(25, test_file), "Check arguments")
  expect_error(read_emfdata(test_data_path, 25), "Check arguments")
  expect_error(
    read_emfdata(test_data_path, test_file, ftps_address_port = ""), "Check arguments"
  )
  expect_error(
    read_emfdata(test_data_path, test_file, userpwd = ""), "Check arguments"
  )
  expect_error(
    read_emfdata(test_data_path, test_file, download_tmp = ""), "Check arguments"
  )
  expect_error(
    read_emfdata(test_data_path, test_file, .fun = ""), "Check arguments"
  )

  ### csv working
  # csv working with defaults
  expect_s3_class(csv_default <- read_emfdata(test_data_path, test_file), "tbl")
  # csv working with download
  expect_s3_class(csv_down <- read_emfdata(test_data_path, test_file, download_tmp = TRUE), "tbl")
  # csv working with fun
  expect_s3_class(csv_fun <- read_emfdata(test_data_path, test_file, .fun = readr::read_csv), "tbl")
  # csv working with fun and download
  expect_s3_class(
    csv_fun_down <- read_emfdata(test_data_path, test_file, download_tmp = TRUE, .fun = readr::read_csv),
    "tbl"
  )
  # all should be identical
  expect_identical(csv_default, csv_down)
  expect_identical(csv_default, csv_fun)
  expect_identical(csv_default, csv_fun_down)

  ### RData working
  test_data_path <- "vgranda/test/"
  test_file <- "meteoland_topo_example.RData"
  # RData working with defaults
  expect_type(RData_default <- read_emfdata(test_data_path, test_file), "character")
  expect_s3_class(eval(rlang::sym(RData_default)), "sf")
  # RData working with download
  expect_type(RData_down <- read_emfdata(test_data_path, test_file, download_tmp = TRUE), "character")
  expect_s3_class(eval(rlang::sym(RData_down)), "sf")
  # RData working with fun
  expect_type(RData_fun <- read_emfdata(test_data_path, test_file, .fun = load), "character")
  expect_s3_class(eval(rlang::sym(RData_fun)), "sf")
  # RData working with fun and download
  expect_type(
    RData_fun_down <- read_emfdata(test_data_path, test_file, download_tmp = TRUE, .fun = load),
    "character"
  )
  expect_s3_class(eval(rlang::sym(RData_fun_down)), "sf")
  # all should be identical
  expect_identical(RData_default, RData_down)
  expect_identical(RData_default, RData_fun)
  expect_identical(RData_default, RData_fun_down)

  ### rds working
  test_data_path <- "vgranda/test/"
  test_file <- "meteoland_topo_example.rds"
  # rds working with defaults
  expect_s3_class(rds_default <- read_emfdata(test_data_path, test_file), "sf")
  # rds working with download
  expect_s3_class(rds_down <- read_emfdata(test_data_path, test_file, download_tmp = TRUE), "sf")
  # rds working with fun (not working because rds needs two functions)
  # expect_s3_class(rds_fun <- read_emfdata(test_data_path, test_file, .fun = readr::read_csv), "sf")
  # all three should be identical
  expect_identical(rds_default, rds_down)
  # expect_identical(rds_default, rds_fun)

  ### other formats working
  test_data_path <- "vgranda/test/"
  test_file <- "meteoland_topo_example.gpkg"
  # gpkg with defaults, expect error
  expect_error(
    gpkg_default <- read_emfdata(test_data_path, test_file),
    "The file doesn't seem to exist"
  )
  # gpkg working with download
  expect_s3_class(gpkg_down <- read_emfdata(test_data_path, test_file, download_tmp = TRUE), "sf")
  # gpkg with fun, no download, expet error
  expect_error(
    gpkg_fun <- read_emfdata(test_data_path, test_file, .fun = sf::read_sf),
    "The file doesn't seem to exist"
  )
  # gpkg working with download and fun
  expect_s3_class(
    gpkg_fun_down <-
      read_emfdata(test_data_path, test_file, download_tmp = TRUE, .fun = sf::read_sf),
    "sf"
  )
  # all should be identical
  expect_identical(gpkg_down, gpkg_fun_down)
})

test_that("contents_emfdata works as intended", {
  test_data_path <- "vgranda/"
  test_data_path_wrong <- "non/existent/path/"

  expect_type(test_res <- contents_emfdata(test_data_path), "character")
  expect_true(length(test_res) > 0)
  expect_error(contents_emfdata(test_data_path_wrong), "Server denied")
})
