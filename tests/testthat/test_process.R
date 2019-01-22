context("Process functions")

# Save personal login credentials as variables named user and pass

# Mock data ------------------------------------------------------------------
test_data <- list("xml_document", "xml_node")

# Tests: get_* functions ------------------------------------------------------

test_that("get_* returns an error if xml is passed as argument", {
  expect_error(get_stations_tag(test_data))
})
