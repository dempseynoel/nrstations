### Package errors

#' Report an error with a http request
#'
#' @param response The text of the server response.
#' @keywords internal

error_status <- function(response_text) {
  error <- xml2::xml_children(response_text)
  error <- xml2::xml_text(xml2::xml_child(error[[2]]))
}
