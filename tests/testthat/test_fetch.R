context("Fetch functions")

# Save personal login credentials as variables named user and pass

# Tests: fetch_stations_xml ---------------------------------------------------

test_that("fetch_stations_xml returns an xml_document", {
  response <- fetch_stations_xml(user, pass)
  expect_that(response, is_a("xml_document"))
})

# Tests: fetch_stations_list --------------------------------------------------

test_that("fetch_stations_list returns a list", {
  response <- fetch_stations_list(user, pass)
  expect_that(response, is_a("list"))
})
