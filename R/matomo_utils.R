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
matomo_lfc_events <- function(last = 31, period = "day") {
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
      "LiDAR App", "NFI app", "AllometrApp", "Catdrought App", "Meteoland App",
      "FES App", "DEBOSCAT App"
    ))

  apps_stats <- matomo_queries |>
    purrr::map(
      .f = \(query) {
        jsonlite::read_json(query, simplifyVector = TRUE) |>
          purrr::discard(.p = purrr::is_empty) |>
          purrr::list_rbind(names_to = period)
      }
    ) |>
    purrr::list_rbind(names_to = "app_name") |>
    dplyr::select(
      app_name, dplyr::any_of(period), nb_visits, nb_events, Events_EventAction, Events_EventName
    ) |>
    dplyr::filter(
      # app_name == "nfi",
      # stringr::str_detect(Events_EventAction, "mod_dataInput-"),
      Events_EventAction != "updates",
      !stringr::str_detect(Events_EventAction, "shinyjs"),
      !stringr::str_detect(Events_EventAction, "_open$")
    ) |>
    dplyr::group_by(app_name, Events_EventAction, Events_EventName) |>
    dplyr::summarise(
      count = sum(as.integer(nb_events)),
      visits = sum(as.integer(nb_visits))
    ) |>
    dplyr::arrange(app_name, Events_EventAction, dplyr::desc(count)) |>
    dplyr::as_tibble()

  return(apps_stats)
}

#' Visit stats for LFC apps
#'
#' Connect to matomo API and retrieve LFC apps visit stats
#'
#' This function assumes some environmental variables are available
#'
#' @param last number of last records to retrieve
#' @param period period to retrieve (day, week, month...)
#'
#' @export
matomo_lfc_visits <- function(last = 31, period = "day") {
  # env vars needed
  matomo_url <- Sys.getenv("MATOMO_URL")
  matomo_token <- Sys.getenv("MATOMO_TOKEN")
  matomo_format <- Sys.getenv("MATOMO_FORMAT")

  # site id's are not env vars because they are fixed values that we know
  matomo_sites <- 3:9

  # queries
  matomo_queries <- glue::glue(
    "{matomo_url}?module=API&method=Live.getLastVisitsDetails&flat=1&idSite={matomo_sites}&date=last{last}&period={period}&format=json&token_auth={matomo_token}&filter_limit=-1"
  ) |>
    as.character() |>
    purrr::set_names(c(
      "LiDAR App", "NFI app", "AllometrApp", "Catdrought App", "Meteoland App",
      "FES App", "DEBOSCAT App"
    ))

  visit_stats <- matomo_queries |>
    purrr::map(
      .f = \(query) {
        jsonlite::read_json(query, simplifyVector = TRUE)
      }
    ) |>
    purrr::discard(.p = purrr::is_empty) |>
    purrr::list_rbind(names_to = "app_name") |>
    dplyr::select(
      app_name, idVisit, country, visitDuration, interactions, actions
    ) |>
    dplyr::filter(interactions > 0) |>
    dplyr::as_tibble()

  return(visit_stats)
}

#' Visit stats for EMF web
#'
#' Connect to matomo API and retrieve EMF web visit stats
#'
#' This function assumes some environmental variables are available
#'
#' @param last number of last records to retrieve
#' @param period period to retrieve (day, week, month...)
#'
#' @export
matomo_emf_visits <- function(last = 31, period = "day") {
  # env vars needed
  matomo_url <- Sys.getenv("MATOMO_URL")
  matomo_token <- Sys.getenv("MATOMO_TOKEN")
  matomo_format <- Sys.getenv("MATOMO_FORMAT")

  # site id's are not env vars because they are fixed values that we know
  matomo_sites <- 1

  # queries
  matomo_query <- glue::glue(
    "{matomo_url}?module=API&method=Live.getLastVisitsDetails&flat=1&idSite={matomo_sites}&period={period}&date=last{last}&format=json&token_auth={matomo_token}&filter_limit=-1"
  )

  visit_stats <- jsonlite::read_json(matomo_query, simplifyVector = TRUE) |>
    # purrr::discard(.p = purrr::is_empty) |>
    # purrr::list_rbind(names_to = "app_name") |>
    dplyr::select(
      idVisit, serverDate, country,
      visitDuration, interactions, actions,
      latitude, longitude
    ) |>
    dplyr::filter(interactions > 0, visitDuration > 3) |>
    dplyr::arrange(serverDate, country, visitDuration) |>
    dplyr::as_tibble()

  return(visit_stats)
}

# lfc_events <- matomo_lfc_events(120)
# lfc_events |>
#   dplyr::filter(Events_EventAction == "mod_dataInput-nfi") |>
#   echarts4r::e_charts(Events_EventName) |>
#   echarts4r::e_pie(count, radius = c("25%", "50%"))
# lfc_events |>
#   dplyr::filter(Events_EventAction == "mod_dataInput-admin_div") |>
#   echarts4r::e_charts(Events_EventName) |>
#   echarts4r::e_pie(count, radius = c("25%", "50%"))
# lfc_events |>
#   dplyr::filter(Events_EventAction == "mod_vizInput-viz_color") |>
#   echarts4r::e_charts(Events_EventName) |>
#   echarts4r::e_pie(count, radius = c("25%", "50%"))
# lfc_events |>
#   dplyr::filter(Events_EventAction == "mod_vizInput-viz_size") |>
#   dplyr::mutate(Events_EventName = tidyr::replace_na(Events_EventName, "no_size")) |>
#   echarts4r::e_charts(Events_EventName) |>
#   echarts4r::e_pie(count, radius = c("25%", "50%"))

# # radar plot of visit duration, number of interactions and visits
# lfc_visits <- matomo_lfc_visits(60)
# lfc_visits |>
#   dplyr::filter(app_name != "Meteoland App") |>
#   dplyr::group_by(app_name) |>
#   dplyr::summarise(
#     duration = mean(visitDuration) / 60,
#     interactions = mean(actions) / 10,
#     visits = dplyr::n() / 10
#   ) |>
#   echarts4r::e_charts(app_name) |>
#   echarts4r::e_radar(
#     duration, name = "Average visit duration (min)", max = 15,
#     radar = list(
#       axisLabel = list(show = TRUE),
#       splitNumber = 3
#     ),
#     symbol = "none", areaStyle = list(opacity = 0.2)
#   ) |>
#   echarts4r::e_radar(
#     interactions, name = "Average number of interactions (x10)",
#     radar = list(
#       axisLabel = list(show = TRUE),
#       splitNumber = 3
#     ),
#     symbol = "none", areaStyle = list(opacity = 0.2)
#   ) |>
#   echarts4r::e_radar(
#     visits, name = "Visits (x10)",
#     radar = list(
#       axisLabel = list(show = TRUE),
#       splitNumber = 3
#     ),
#     symbol = "none", areaStyle = list(opacity = 0.2)
#   ) |>
#   echarts4r::e_tooltip(trigger = "item")

# lfc_visits |>
#   dplyr::filter(app_name != "Meteoland App") |>
#   dplyr::group_by(app_name, country) |>
#   dplyr::count() |>
#   dplyr::group_by(app_name) |>
#   dplyr::group_map(
#     .f = \(app_data, .index) {
#       browser()
#       app_data |>
#         echarts4r::e_chart(country) |>
#         echarts4r::e_pie(n, radius = c("25%", "50%"), name = .index[["app_name"]])
#     }
#   )

# emf_visits <- matomo_emf_visits(120)
# emf_visits |>
#   dplyr::group_by(country) |>
#   dplyr::count() |>
#   dplyr::ungroup() |>
#   echarts4r::e_charts(country) |>
#   echarts4r::e_pie(n, radius = c("25%", "50%"))

# emf_visits |>
#   dplyr::mutate(creaf_lat = 41.50102198873047, creaf_long = 2.1091959965299747) |>
#   echarts4r::e_charts() |>
#   echarts4r::e_geo() |>
#   echarts4r::e_lines(
#     longitude, latitude, creaf_long, creaf_lat, name = "visits",
#     lineStyle = list(normal = list(curveness = 0.3))
#   )

# emf_visits |>
#   dplyr::mutate(creaf_lat = 41.501, creaf_long = 2.109) |>
#   echarts4r::e_charts(longitude) |>
#   echarts4r::e_globe(
#     # environment = echarts4r.assets::ea_asset("galaxy"),
#     environment = "#4E6562",
#     # base_texture = echarts4r.assets::ea_asset("composite 4k"),
#     base_texture = echarts4r.assets::ea_asset("world"),
#     height_texture = echarts4r.assets::ea_asset("elevation 4k"),
#     displacementScale = 0.05
#   ) |>
#   echarts4r::e_lines_3d(
#     longitude, latitude, creaf_long, creaf_lat, country,
#     name = "visits",
#     effect = list(show = TRUE, trailWidth = 6, trailColor = "#E2DF69"),
#     lineStyle = list(color = "#18ACCA", width = 3)
#   ) |>
#   # echarts4r::e_bar_3d(
#   #   latitude, interactions, coord_system = "globe"
#   # ) |>
#   echarts4r::e_legend(FALSE)

# emf_visits |>
#   dplyr::group_by(serverDate) |>
#   dplyr::summarise(
#     visits = dplyr::n(),
#     duration = mean(visitDuration) / 60,
#     actions = mean(actions)
#   ) |>
#   echarts4r::e_charts(serverDate) |>
#   echarts4r::e_river(visits, name = "Visits") |>
#   echarts4r::e_river(duration, name = "Average visit length (min)") |>
#   echarts4r::e_river(actions, name = "Average visit actions") |>
#   echarts4r::e_tooltip(trigger = "axis")
