
# Failing Tests
test_that("gnfinder noncharacter string created error", {
  expect_error(gnfinder(string = 1), class = "usethis_error")
})

test_that("non-utf8 string input stop", {
  expect_error(gnfinder(string = c("fa\u00E7ile", "fa\xE7ile", "fa\xC3\xA7ile")), class = "usethis_error")
})


test_that("check_names FALSE & source_idss provided error", {
  expect_error(gnfinder(string = "hello", check_names = FALSE, source_ids = 179), class = "usethis_error")
})


# Default Tests

test_that("single string default runs and returns data frame", {
  expect_message(one <- gnfinder("Lepidium meyenii is a hot plant. Escherichia coli is not"), "Running search: Checking names against Catalogue of Life")
  expect_is(one, class = "data.frame")
})

test_that("vector default runs and returns data frame", {
  expect_message(two <- gnfinder(c("Lepidium meyenii is a hot plant", "Capsicum annuum is a hot plant for a different reason")), "Running search: Checking names against Catalogue of Life")
  expect_is(two, class = "data.frame")
})

# test data frame input

# No bayes options
test_that("nobayes runs and returns data frame", {
  expect_message(two <- gnfinder(c("Lepidium meyenii is a hot plant", "Capsicum annuum is a hot plant for a different reason"), nobayes = TRUE), "Running search: no bayes, checking names against default Catalogue of Life")
  expect_is(two, class = "data.frame")
})

test_that("nobayes and no check_names runs and returns data frame", {
  expect_message(two <- gnfinder(c("Lepidium meyenii is a hot plant", "Capsicum annuum is a hot plant for a different reason"), nobayes = TRUE, check_names = FALSE), "Running search: no bayes, not checking names")
  expect_is(two, class = "data.frame")
})

test_that("nobayes, check_names and source_idss runs and returns data frame", {
  expect_message(two <- gnfinder(c("Lepidium meyenii is a hot plant", "Capsicum annuum is a hot plant for a different reason"), nobayes = TRUE, source_ids = 11),"Running search: no bayes checking against Catalogue of Life and the source_ids provided")
  expect_is(two, class = "data.frame")
})


# standard options (see defaults above)

test_that("vector default runs and returns data frame", {
  expect_message(two <- gnfinder(c("Lepidium meyenii is a hot plant", "Capsicum annuum is a hot plant for a different reason"), check_names = FALSE), "Running search: not checking names")
  expect_is(two, class = "data.frame")
})

test_that("vector default runs with multiple source_ids and returns data frame", {
  expect_message(two <- gnfinder(c("Lepidium meyenii is a hot plant", "Capsicum annuum is a hot plant for a different reason"), source_ids = c(11,179)), "Running search: Checking against the Catalogue of Life and the source_ids provided")
  expect_is(two, class = "data.frame")
})


test_that("vector default runs with multiple source_ids as character vector returns data frame", {
  expect_message(two <- gnfinder(c("Lepidium meyenii is a hot plant", "Capsicum annuum is a hot plant for a different reason"), source_ids = c("11","179")), "Running search: Checking against the Catalogue of Life and the source_ids provided")
  expect_is(two, class = "data.frame")
})

# data frame option

test_that("check dataframe with multi ids returns dataframe", {
  expect_message(two <- gnfinder(fivetexts$text, fivetexts$id, source_ids = c("11","179")), "Running search: Checking against the Catalogue of Life and the source_ids provided")
  expect_is(two, class = "data.frame")
})


