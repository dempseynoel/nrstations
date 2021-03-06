% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process.R
\name{get_accessibility_tags}
\alias{get_accessibility_tags}
\title{Get a tibble of accessibility tag data for each station}
\usage{
get_accessibility_tags(stations_list)
}
\arguments{
\item{stations_list}{A list of stations as returned by \code{fetch_stations_list}}
}
\value{
A tibble of data under the accessibility tag for each station.
}
\description{
\code{get_accessibility_tags} takes a list of stations and returns a tibble
of data under the accessibility tag, with one row per station. Longitude
and latitude are numeric, all other variables are a character string.
}
