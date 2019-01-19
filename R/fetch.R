### Functions for fetching data from the NRDP Stations API

#' Fetch data on all stations and return as an xml_document
#'
#' \code{fetch_stations_xml} fetches data on all stations and returns
#' an R object of class "xml_document" and "xml_node".
#'
#' @param nrdp_user Your National Rail Data Portal email address.
#' @param nrdp_pass Your National Rail Data Portal password.
#' @return An R object of class "xml_document" and "xml_node".
#' @export

fetch_stations_xml <- function(nrdp_user, nrdp_pass) {
  xml2::read_xml(fetch_response(nrdp_user, nrdp_pass), encoding = "ISO-8859-1")
}

#' Fetch data on all stations and return as a list
#'
#' \code{fetch_stations_list} fetches data on all stations and returns an R
#' object of class "list".
#'
#' @param nrdp_user Your National Rail Data Portal email address.
#' @param nrdp_pass Your National Rail Data Portal password.
#' @return An R object of class "list".
#' @export

fetch_stations_list <- function(nrdp_user, nrdp_pass) {
  message("This may take a minute...")
  stations_xml <- xml2::read_xml(fetch_response(nrdp_user, nrdp_pass),
    encoding = "ISO-8859-1")
  xml2::as_list(xml2::xml_children(stations_xml))
}
