#' Access data from EMF data server
#'
#' Read files in the EMF data server
#'
#' \code{read_emfdata} can automatically detect the type of file and apply the default function
#' for that file. Using curl connections or downloading the file is indicated by the
#' \code{download_tmp} argument, by default FALSE. Extra arguments for reading function can be
#' supplied.
#'
#' @section Default functions:
#'   If no \code{.fun} is supplied, \code{read_emfdata} will try to match a default function with
#'   the file extension. If no match is found, the connection (or the file if \code{download_tmp}
#'   is TRUE) are returned for the user to deal with.
#'
#' @section RData files:
#'   RData files are loaded with \code{\link[base]{load}}. This means that objects contained in the
#'   RData file will be loaded by default into the environment \code{read_emfdata} is called. The
#'   names of the objects loaded are returned in a character vector.
#'
#' @param data_path base path in EMF data server
#' @param file file name (with extension) to read
#' @param ftps_addres_port Address and port (in the form "protocol://address:port/"). Defaults to
#'   \code{Sys.getenv("emfdata_ftps_address_port")}.
#' @param userpwd User and password (in the form "user:pasword"). Defaults to
#'   \code{Sys.getenv("emfdata_userpwd")}.
#' @param download_tmp Logical indicating if the file should be downloaded in a temporal file
#'   before applying \code{.fun} instead of being treated as a curl connection by \code{.fun}.
#'   Defaults to \code{FALSE}.
#' @param noread Logical indicating if the file should be downloaded only and no attempt to read it
#'   should be done. Defaults to \code{FALSE}. Only used if \code{download_tmp} is TRUE.
#' @param .fun Function to read the connection (or file if \code{download_tmp}) is TRUE. If no
#'   function is supplied, \code{read_emfdata} will try default ones based on the file extension.
#'   For a comprehensible list of default functions see Details.
#' @param ... Extra arguments for \code{.fun}
#' @param .env Environment to load the data in, if loading RData files
#'
#' @export
read_emfdata <- function(
    data_path, file,
    ftps_address_port = Sys.getenv("emfdata_ftps_address_port"),
    userpwd = Sys.getenv("emfdata_userpwd"),
    download_tmp = FALSE, noread = FALSE,
    .fun = NULL, ...,
    .env = parent.frame()
) {

  ## Assertions
  assertthat::assert_that(
    is.character(data_path), is.character(file), ftps_address_port != "", userpwd != "",
    is.logical(download_tmp), is.logical(noread), if (!is.null(.fun)) is.function(.fun) else TRUE,
    msg = "Check arguments, something went wrong"
  )

  ## Download?
  # handle
  emf_handle <- curl::new_handle(use_ssl = TRUE, userpwd = userpwd)
  # con is a downloaded file if download TRUE or a connection if download FALSE
  if (isTRUE(download_tmp)) {
    # tmp file
    tmp_file <- fs::path(emf_temp_folder(), file)
    # tmp download
    con <- curl::curl_download(
      paste0(ftps_address_port, data_path, file),
      tmp_file,
      handle = emf_handle
    )
    # if no read is indicated, return the path to the file and exit gracefully
    if (isTRUE(noread)) {
      return(con)
    }
  } else {
    # tmp connection
    con <- curl::curl(
      url = paste0(ftps_address_port, data_path, file),
      handle = emf_handle
    )
    # always close connection, even if function errored
    withr::defer(try(close(con), silent = TRUE))
  }

  # different things for different files, but if fun is supplied then use it directly
  if (!is.null(.fun)) {
    res <- .fun(con, ...)
    return(res)
  }
  # if not fun is provided then, defaults
  # extract extension
  file_extension <- stringr::str_extract(file, "\\.[0-9a-zA-Z]+$")
  # apply function corresponding to known extensions, if not recognized, return con (the connection
  # or the file)
  res <- switch(
    file_extension,
    ".rda" = load(con, envir = .env),
    ".RData" = load(con, envir = .env),
    ".Rdata" = load(con, envir = .env),
    ".rdata" = load(con, envir = .env),
    ".rds" = if (inherits(con, "connection")) readRDS(gzcon(con)) else readRDS(con),
    ".RDS" = if (inherits(con, "connection")) readRDS(gzcon(con)) else readRDS(con),
    ".Rds" = if (inherits(con, "connection")) readRDS(gzcon(con)) else readRDS(con),
    ".csv" = readr::read_csv(con, ...),
    ".txt" = readr::read_delim(con, ...),
    ".gpkg" = sf::read_sf(con, ...),
    con
  )

  return(res)
}

#' @describeIn read_emfdata List available contents for the provided data path
#' @export
contents_emfdata <- function(
  data_path,
  ftps_address_port = Sys.getenv("emfdata_ftps_address_port"),
  userpwd = Sys.getenv("emfdata_userpwd")
){

  ## Assertions
  assertthat::assert_that(
    is.character(data_path), ftps_address_port != "", userpwd != "",
    msg = "Check arguments, something went wrong"
  )

  # handle
  emf_handle <- curl::new_handle(use_ssl = TRUE, userpwd = userpwd, dirlistonly = TRUE)
  # connection piped to read lines
  curl::curl(
    url = paste0(ftps_address_port, data_path),
    handle = emf_handle
  ) |>
    readr::read_lines()
}

#' @describeIn read_emfdata Save files to the EMF data server
#'
#' @param x \code{x} can be either an object or a character vector of length one with a path to a
#'   local file. If \code{x} is an object, then it will be saved as an \code{rds} file with
#'   \code{\link[base]{saveRDS}}. If \code{x} is a path, then the file is uploaded as is.
#' @param output_file character indicating the name of the output file in the EMF data server
#'
#' @export
save_emfdata <- function(
  x,
  data_path,
  output_file,
  ftps_address_port = Sys.getenv("emfdata_ftps_address_port"),
  userpwd = Sys.getenv("emfdata_userpwd")
){

  ## Assertions
  assertthat::assert_that(
    is.character(data_path), is.character(output_file), ftps_address_port != "", userpwd != "",
    msg = "Check arguments, something went wrong"
  )

  if (!is.character(x)) {
    # tmp file (and remove it after function exits)
    tmp_file <- fs::path(emf_temp_folder(), output_file)
    withr::defer(try(fs::file_delete(tmp_file), silent = TRUE))
    saveRDS(x, tmp_file)
  } else {
    # if x is character then, the tmp_file path is x
    tmp_file <- x
  }

  # upload data to emf_data
  upload_status <- curl::curl_upload(
    file = tmp_file,
    url = paste0(ftps_address_port, data_path, output_file),
    userpwd = userpwd,
    use_ssl = TRUE,
    verbose = FALSE,
    reuse = FALSE
  )

  return(invisible(TRUE))
}

#
#
# #' @rdname emfdata_file_utilities
# readRDS_emfdata <- function(emfdata_base, file) {
#   emf_handle <- curl::new_handle(
#     use_ssl = TRUE,
#     userpwd = Sys.getenv("emfdata_userpwd")
#   )
#   con <- curl::curl(
#     paste0("ftp://data-emf.creaf.cat:22111/", emfdata_base, file),
#     handle = emf_handle
#   )
#   obj <- readRDS(gzcon(con))
#   close(con)
#   return(obj)
# }
#
# #' @rdname emfdata_file_utilities
# curl_emfdata<-function(emfdata_base, file) {
#   # create the connection
#   con <- curl::curl(
#     paste0("ftp://data-emf.creaf.cat:22111/", emfdata_base, file),
#     handle = curl::new_handle(
#       use_ssl = TRUE,
#       userpwd =  Sys.getenv("emfdata_userpwd")
#     )
#   )
#   return(con)
# }
#
# #' @param obj object to be written as RDS file
# #' @rdname emfdata_file_utilities
# saveRDS_emfdata<-function(obj, emfdata_base, file) {
#   # save data in temporary file
#   tmp <- tempfile()
#   saveRDS(obj, tmp)
#   # upload data to emf_data
#   upload_status <- curl::curl_upload(
#     file = tmp,
#     url = paste0("ftp://data-emf.creaf.cat:22111/", emfdata_base, file),
#     userpwd = Sys.getenv("emfdata_userpwd"),
#     use_ssl = TRUE,
#     verbose = FALSE,
#     reuse = FALSE)
#   # remove temporary file
#   file.remove(tmp)
# }
#
# #' @param dir directory of EMF_DATA to be listed
# #' @rdname emfdata_file_utilities
# list_emfdata<-function(emfdata_base, dir=""){
#   con <- curl::curl(
#     paste0("ftp://data-emf.creaf.cat:22111/",emfdata_base,dir),
#     handle = curl::new_handle(
#       use_ssl = TRUE,
#       userpwd = Sys.getenv("emfdata_userpwd"),
#       dirlistonly = TRUE
#     )
#   )
#   l <- readr::read_lines(con)
#   return(l)
# }
#
#
# #' @param local_file Path to local file to be read or written
# #' @rdname emfdata_file_utilities
# upload_emfdata<-function(local_file, emfdata_base, file) {
#   upload_status <- curl::curl_upload(
#     local_file,
#     paste0("ftp://data-emf.creaf.cat:22111/", emfdata_base, file),
#     userpwd = Sys.getenv("emfdata_userpwd"),
#     use_ssl = TRUE,
#     verbose = FALSE,
#     reuse = FALSE
#   )
#   return(upload_status)
# }
#
# #' File utilities for EMF_DATA
# #'
# #' @param emfdata_base base path in EMF_DATA
# #' @param file target file in EMF_DATA
# #'
# #' @export
# #'
# #' @name emfdata_file_utilities
# download_emfdata<-function(emfdata_base, file) {
#   handle = curl::new_handle(
#     use_ssl = TRUE,
#     userpwd = Sys.getenv("emfdata_userpwd")
#   )
#   local_file <- tempfile()
#   curl::curl_download(paste0("ftp://data-emf.creaf.cat:22111/", emfdata_base, file),
#                       local_file,
#                       handle = handle)
#   return(local_file)
# }
