### Package errors

#' Report an error with a http request
#'
#' @param response_text The text of the server response.
#' @keywords internal

error_status <- function(response_text) {
  error <- xml2::xml_children(response_text)
  error <- xml2::xml_text(xml2::xml_child(error[[2]]))
}

#' Report an error when using XML with get_* fuctions
#'
#' @param stations_list The object passed to get_* function
#' @keywords internal

error_get <- function() {
  error <- stringr::str_c("You are tring to use an object with class ",
                          "'xml_document'. Use the object with class 'list' ",
                          "returned by 'fetch_stations_list' instead.")
}
