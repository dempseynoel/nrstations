context("Utility functions")

# Save personal login credentials as variables named user and pass

# Tests: fetch_authentication -------------------------------------------------

test_that("fetch_authentication returns a character string", {
  response <- fetch_authentication(user, pass)
  expect_that(response, is_a("character"))
})

test_that("fetch_authentication contains nrdp username", {
  response <- fetch_authentication(user, pass)
  expect_match(response, user, ignore.cas = TRUE)
})

# Tests: fetch_response -------------------------------------------------------

test_that("fetch_response returns a response from the API", {
  response <- fetch_response(user, pass)
  expect_that(response, is_a("response"))
})

# Tests: handle_null ----------------------------------------------------------

mock_list <- list(node_1 = list(sub_node_1 = "A bit of text",
                                sub_node_2 = "A bit more text",
                                sub_node_3 = "Some more text"),
                  node_2 = list(sub_node_1 = "A bit of text",
                                sub_node_2 = "A bit more text")
)

test_that("handle_null returns node data if node exists", {
  data_1 <- handle_null(mock_list$node_1$sub_node_1[[1]])
  expect_equal(data_1, "A bit of text")
  data_2 <- handle_null(mock_list$node_1$sub_node_2[[1]])
  expect_equal(data_2, "A bit more text")
  data_3 <- handle_null(mock_list$node_2$sub_node_1[[1]])
  expect_equal(data_3, "A bit of text")
})

test_that("handle_null returns NA if node missing from list", {
  data_1 <- handle_null(mock_list$node_2$sub_node_3[[1]])
  expect_equal(data_1, NA)
  data_2 <- handle_null(mock_list$node_3$sub_node_1[[1]])
  expect_equal(data_2, NA)
})
