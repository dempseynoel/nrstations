### Package utility functions

# Import pipe -----------------------------------------------------------------

#' @importFrom magrittr %>%
NULL

# API functions ---------------------------------------------------------------

#' Fetch NRDP authentication token
#'
#' @keywords internal

fetch_authentication <- function(nrdp_user, nrdp_pass) {

  response <- httr::POST(URL_AUTHENTICATE, body = list(
    username = nrdp_user, password = nrdp_pass), encode = "form")
  response_text <- httr::content(response)

  if (response$status_code != 200) stop(error_status(response_text))

  response_text$token
}

#' Fetch NRDP API response
#'
#' @keywords internal

fetch_response <- function(nrdp_user, nrdp_pass) {

  response <- httr::GET(URL_STATIONS_API, config =
    httr::add_headers("X-Auth-Token" =
      fetch_authentication(nrdp_user, nrdp_pass)))
  response_text <- httr::content(response)

  if (response$status_code != 200) stop(error_status(response_text))

  response
}

# Data handling functions -----------------------------------------------------

#' Create basic station data tibble
#'
#' @keywords internal

get_station_basic <- function(stations_list) {

  values <- purrr::map_df(stations_list, function(node) {

      tibble::tibble(
      station_code = handle_null(node$CrsCode[[1]]),
      station_name = handle_null(node$Name[[1]]),
      station_abbr = handle_null(node$SixteenCharacterName[[1]]),
      nlc_code = handle_null(node$AlternativeIdentifiers$NationalLocationCode[[1]]),
      longitude = as.numeric(handle_null(node$Longitude[[1]])),
      latitude = as.numeric(handle_null(node$Latitude[[1]])),
      station_operator = handle_null(node$StationOperator[[1]]))
  })

  dplyr::bind_rows(values)
}

#' Check if list element is NULL
#'
#' @keywords internal

handle_null <- function(station_list_element) {
  ifelse(is.null(station_list_element) == FALSE, station_list_element, NA)
}
