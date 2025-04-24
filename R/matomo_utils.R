#' Event stats for LFC apps
#'
#' Connect to matomo API and retrieve LFC apps event stats
#'
#' This function assumes some environmental variables are available
#'
#' @param last number of last records to retrieve
#' @param period period to retrieve (day, week, month...)
#'
#' @export
matomo_get_latest <- function(last = 12, period = "week") {
  # env vars needed
  matomo_url <- Sys.getenv("MATOMO_URL")
  matomo_token <- Sys.getenv("MATOMO_TOKEN")
  matomo_format <- Sys.getenv("MATOMO_FORMAT")

  # site id's are not env vars because they are fixed values that we know
  matomo_sites <- 3:9

  # queries
  matomo_queries <- glue::glue(
    "{matomo_url}?module=API&method=Events.getAction&secondaryDimension=eventName&flat=1&idSite={matomo_sites}&date=last{last}&period={period}&format=json&token_auth={matomo_token}&filter_limit=-1"
  ) |>
    as.character() |>
    purrr::set_names(c(
      "lidar", "nfi", "allometr", "catdrought", "meteoland", "fes", "deboscat"
    ))

  json_responses <- matomo_queries |>
    purrr::map(
      .f = \(query) {jsonlite::read_json(query, simplifyVector = FALSE)}
    )

}