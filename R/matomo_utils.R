#' Event stats for LFC apps
#'
#' Connect to matomo API and retrieve LFC apps event stats
#'
#' This function assumes some environmental variables are available
#'
#' @param date_start Oldest date to retrieve
#' @param date_end Newest date to retrieve
#'
#' @export
matomo_get_apps_events <- function(date_start = Sys.Date() - 31, date_end = Sys.Date()) {
  # env vars needed
  matomo_url <- Sys.getenv("MATOMO_URL")
  matomo_token <- Sys.getenv("MATOMO_TOKEN")
  matomo_format <- Sys.getenv("MATOMO_FORMAT")

  # site id's are not env vars because they are fixed values that we know
  matomo_sites <- 3:9

  # query
  glue::glue(
    ""
  )

}