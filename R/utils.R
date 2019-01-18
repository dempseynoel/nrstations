# Package utility functions

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

  if (response$status_code != 201) stop(error_status(response_text))

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

#' Check if list element is NULL
#'
#' @keywords internal

handle_null <- function(station_list_element) {
  ifelse(is.null(station_list_element) == FALSE, station_list_element, NA)
}
