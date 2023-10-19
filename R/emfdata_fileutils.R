#' @rdname emfdata_file_utilities
readRDS_emfdata<-function(emfdata_base, file) {
  emf_handle <- curl::new_handle(
    use_ssl = TRUE,
    userpwd = Sys.getenv("emfdata_userpwd")
  )
  con<-curl::curl(paste0("ftp://data-emf.creaf.cat:22111/", emfdata_base, file),
                  handle = emf_handle)
  obj <- readRDS(gzcon(con))
  close(con)
  return(obj)
}

#' @rdname emfdata_file_utilities
curl_emfdata<-function(emfdata_base, file) {
  # create the connection
  con <- curl::curl(
    paste0("ftp://data-emf.creaf.cat:22111/", emfdata_base, file),
    handle = curl::new_handle(
      use_ssl = TRUE,
      userpwd =  Sys.getenv("emfdata_userpwd")
    )
  )
  return(con)
}

#' @param obj object to be written as RDS file
#' @rdname emfdata_file_utilities
saveRDS_emfdata<-function(obj, emfdata_base, file) {
  # save data in temporary file
  tmp <- tempfile()
  saveRDS(obj, tmp)
  # upload data to emf_data
  upload_status <- curl::curl_upload(
    file = tmp,
    url = paste0("ftp://data-emf.creaf.cat:22111/", emfdata_base, file),
    userpwd = Sys.getenv("emfdata_userpwd"),
    use_ssl = TRUE,
    verbose = FALSE,
    reuse = FALSE)
  # remove temporary file
  file.remove(tmp)
}

#' @param dir directory of EMF_DATA to be listed
#' @rdname emfdata_file_utilities
list_emfdata<-function(emfdata_base, dir=""){
  con <- curl::curl(
    paste0("ftp://data-emf.creaf.cat:22111/",emfdata_base,dir),
    handle = curl::new_handle(
      use_ssl = TRUE,
      userpwd = Sys.getenv("emfdata_userpwd"),
      dirlistonly = TRUE
    )
  )
  l <- readr::read_lines(con)
  return(l)
}


#' @param local_file Path to local file to be read or written
#' @rdname emfdata_file_utilities
upload_emfdata<-function(local_file, emfdata_base, file) {
  upload_status <- curl::curl_upload(
    local_file,
    paste0("ftp://data-emf.creaf.cat:22111/", emfdata_base, file),
    userpwd = Sys.getenv("emfdata_userpwd"),
    use_ssl = TRUE,
    verbose = FALSE,
    reuse = FALSE
  )
  return(upload_status)
}

#' File utilities for EMF_DATA
#'
#' @param emfdata_base base path in EMF_DATA
#' @param file target file in EMF_DATA
#'
#' @export
#'
#' @name emfdata_file_utilities
download_emfdata<-function(emfdata_base, file) {
  handle = curl::new_handle(
    use_ssl = TRUE,
    userpwd = Sys.getenv("emfdata_userpwd")
  )
  local_file <- tempfile()
  curl::curl_download(paste0("ftp://data-emf.creaf.cat:22111/", emfdata_base, file),
                      local_file,
                      handle = handle)
  return(local_file)
}
