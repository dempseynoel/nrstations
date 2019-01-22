### Functions for processing data returned from the API as a list

#' Get a tibble of station tag data for each station
#'
#' \code{get_station_tags} takes a list of stations and returns a tibble of
#' data under the station tag, with one row per station. Longitude and
#' latitude are numeric, all other variables are a character string.
#'
#' @param stations_list A list of stations as returned by \code{fetch_stations_list}
#' @return A tibble of data under the station tag for each station.
#' @export

get_station_tags <- function(stations_list) {

  if (class(stations_list)[[1]] == "xml_document") stop(error_get())

  basic <- get_station_basic(stations_list)
  values <- purrr::map_df(stations_list, function(node) {

    tibble::tibble(
      staffing_level = handle_null(node$Staffing$StaffingLevel[[1]]),
      cctv = handle_null(node$Staffing$ClosedCircuitTelevision$Available[[1]]))
  })

  dplyr::bind_rows(values)
  dplyr::bind_cols(basic, values)
}

#' Get a tibble of fare tag data for each station
#'
#' \code{get_fare_tags} takes a list of stations and returns a tibble of
#' data under the fare tag, with one row per station. Longitude and
#' latitude are numeric, all other variables are a character string.
#'
#' @param stations_list A list of stations as returned by \code{fetch_stations_list}
#' @return A tibble of data under the fares tag for each station.
#' @export

get_fare_tags <- function(stations_list) {

  if (class(stations_list)[[1]] == "xml_document") stop(error_get())

  basic <- get_station_basic(stations_list)
  values <- purrr::map_df(stations_list, function(node) {

    tibble::tibble(
      ticket_office = handle_null(node$Fares$TicketOffice$Available[[1]]),
      ticket_collection_office = handle_null(node$Fares$PrepurchaseCollection$TicketOffice[[1]]),
      ticket_collection_machine = handle_null(node$Fares$PrepurchaseCollection$TicketMachine[[1]]),
      ticket_machine_available = handle_null(node$Fares$TicketMachine$Available[[1]]),
      oyster_issue = handle_null(node$Fares$OystercardIssued[[1]]),
      oyster_topup_office = handle_null(node$Fares$OystercardTopup$TicketOffice[[1]]),
      oyster_topup_machine = handle_null(node$Fares$OystercardTopup$TicketMachine[[1]]),
      oyster_use = handle_null(node$Fares$UseOystercard[[1]]),
      smartcard_issue = handle_null(node$Fares$SmartcardIssued[[1]]),
      smartcard_validate = handle_null(node$Fares$SmartcardValidator[[1]]),
      travelcard_zone = handle_null(node$Fares$Travelcard$TravelcardZone[[1]]))
  })

  dplyr::bind_rows(values)
  dplyr::bind_cols(basic, values)
}

#' Get a tibble of facility tag data for each station
#'
#' \code{get_facility_tags} takes a list of stations and returns a tibble of
#' data under the facility tag, with one row per station. Longitude and
#' latitude are numeric, all other variables are a character string.
#'
#' @param stations_list A list of stations as returned by \code{fetch_stations_list}
#' @return A tibble of data under the facilities tag for each station.
#' @export

get_facility_tags <- function(stations_list) {

  if (class(stations_list[[1]]) == "xml_document") stop(error_get())

  basic <- get_station_basic(stations_list)
  values <- purrr::map_df(stations_list, function(node) {

    tibble::tibble(
      luggage_facilities = handle_null(node$PassengerServices$LeftLuggage$Available[[1]]),
      lost_property = handle_null(node$PassengerServices$LostProperty$Available[[1]]),
      first_class_lounge = handle_null(node$StationFacilities$FirstClassLounge$Available[[1]]),
      seated_area = handle_null(node$StationFacilities$SeatedArea$Available[[1]]),
      waiting_area = handle_null(node$StationFacilities$WaitingRoom$Available[[1]]),
      trolleys = handle_null(node$StationFacilities$Trolleys$Available[[1]]),
      station_buffet = handle_null(node$StationFacilities$StationBuffet$Available[[1]]),
      toilets = handle_null(node$StationFacilities$Toilets$Available[[1]]),
      baby_change = handle_null(node$StationFacilities$BabyChange$Available[[1]]),
      showers = handle_null(node$StationFacilities$Showers$Available[[1]]),
      telephones = handle_null(node$StationFacilities$Telephones$Exists[[1]]),
      telephones_usage = handle_null(node$StationFacilities$Telephones$UsageType[[1]]),
      wifi = handle_null(node$StationFacilities$WiFi$Available[[1]]),
      web_kiosk = handle_null(node$StationFacilities$WebKiosk$Available[[1]]),
      post_box = handle_null(node$StationFacilities$PostBox$Available[[1]]),
      tourist_information = handle_null(node$StationFacilities$TouristInformation$Available[[1]]),
      atm_machine = handle_null(node$StationFacilities$AtmMachine$Available[[1]]),
      bureau_dechange = handle_null(node$StationFacilities$BureauDeChange$Available[[1]]),
      shops = handle_null(node$StationFacilities$Shops$Available[[1]]))
  })

  dplyr::bind_rows(values)
  dplyr::bind_cols(basic, values)
}

#' Get a tibble of accessibility tag data for each station
#'
#' \code{get_accessibility_tags} takes a list of stations and returns a tibble
#' of data under the accessibility tag, with one row per station. Longitude
#' and latitude are numeric, all other variables are a character string.
#'
#' @param stations_list A list of stations as returned by \code{fetch_stations_list}
#' @return A tibble of data under the accessibility tag for each station.
#' @export

get_accessibility_tags <- function(stations_list) {

  if (class(stations_list[[1]]) == "xml_document") stop(error_get())

  basic <- get_station_basic(stations_list)
  values <- purrr::map_df(stations_list, function(node) {

    tibble::tibble(
      staff_help = handle_null(node$Accessibility$StaffHelpAvailable$Available[[1]]),
      induction_loop = handle_null(node$Accessibility$InductionLoop[[1]]),
      accessible_ticket_machines = handle_null(node$Accessibility$AccessibleTicketMachines$Available[[1]]),
      accessible_ticket_office = handle_null(node$Accessibility$HeightAdjustedTicketOfficeCounter$Available[[1]]),
      train_ramp_access = handle_null(node$Accessibility$RampForTrainAccess$Available[[1]]),
      national_key_toilets = handle_null(node$Accessibility$NationalKeyToilets$Available[[1]]),
      step_free_access = handle_null(node$Accessibility$StepFreeAccess$Coverage[[1]]),
      ticket_gate = handle_null(node$Accessibility$TicketGate$Available[[1]]),
      mobility_setdown = handle_null(node$Accessibility$ImpairedMobilitySetDown$Available[[1]]),
      wheelchair_available = handle_null(node$Accessibility$WheelchairsAvailable$Available[[1]]))
  })

  dplyr::bind_rows(values)
  dplyr::bind_cols(basic, values)
}

#' Get a tibble of interchange tag data for each station
#'
#' \code{get_interchange_tags} takes a list of stations and returns a tibble of
#' data under the interchange tag, with one row per station. Longitude,
#' latitude cycle_storage_spaces and car_park_spaces are numeric, all
#' other variables are a character string.
#'
#' @param stations_list A list of stations as returned by \code{fetch_stations_list}
#' @return A tibble of data under the interchange tag for each station.
#' @export

get_interchange_tags <- function(stations_list) {

  if (class(stations_list[[1]]) == "xml_document") stop(error_get())

  basic <- get_station_basic(stations_list)
  values <- purrr::map_df(stations_list, function(node) {

    tibble::tibble(
      cycle_storage_spaces = as.numeric(handle_null(node$Interchange$CycleStorage$Spaces[[1]])),
      cycle_storage_sheltered = handle_null(node$Interchange$CycleStorage$Sheltered[[1]]),
      cycle_storage_cctv = handle_null(node$Interchange$CycleStorage$Cctv[[1]]),
      car_park_spaces = as.numeric(handle_null(node$Interchange$CarPark$Spaces[[1]])))
  })

  dplyr::bind_rows(values)
  dplyr::bind_cols(basic, values)
}

#' Get a tibble of all station tag data for each station
#'
#' \code{get_all_station_tags} takes a list of stations and returns a tibble of
#' data under each of the five major tags, with one row per station. Longitude,
#' latitude cycle_storage_spaces and car_park_spaces are numeric, all
#' other variables are a character string.
#'
#' @param stations_list A list of stations as returned by \code{fetch_stations_list}
#' @return A tibble of data for all tags for each station.
#' @export

get_all_station_tags <- function(stations_list) {

  if (class(stations_list[[1]]) == "xml_document") stop(error_get())

  station_tags <- get_station_tags(stations_list)
  fare_tags <- get_fare_tags(stations_list)
  facility_tags <- get_facility_tags(stations_list)
  accessibility_tags <- get_accessibility_tags(stations_list)
  interchange_tags <- get_interchange_tags(stations_list)

  dplyr::bind_cols(station_tags, fare_tags, facility_tags,
    accessibility_tags, interchange_tags) %>%
    dplyr::select(-dplyr::matches("1|2|3|4"))
}
