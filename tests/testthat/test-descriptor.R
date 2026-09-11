fixture <- function() system.file("extdata", "model.json", package = "foceexecutorr")

test_that("a real signed descriptor reads", {
  d <- read_descriptor(fixture())

  expect_s3_class(d, "focex_descriptor")
  expect_equal(d$schema_version, SUPPORTED_SCHEMA_VERSION)
  expect_true(nzchar(d$run_id))
})

test_that("a descriptor from a future schema version is refused, not guessed at", {
  # The failure this actually prevents: sentinel-poppk adds a provenance header
  # (schema 2) and this package silently reads it as if nothing changed.
  tmp <- tempfile(fileext = ".json")
  raw <- jsonlite::fromJSON(fixture(), simplifyVector = FALSE)
  raw$schema_version <- SUPPORTED_SCHEMA_VERSION + 1L
  jsonlite::write_json(raw, tmp, auto_unbox = TRUE)

  expect_error(read_descriptor(tmp), "schema version")
})

test_that("something that is not a descriptor is refused by name", {
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(list(hello = "world"), tmp, auto_unbox = TRUE)

  expect_error(read_descriptor(tmp), "missing required field")
})

test_that("a missing file says so rather than failing inside the parser", {
  expect_error(read_descriptor(file.path(tempdir(), "nope.json")), "no descriptor at")
})

test_that("the chosen model comes from the submitted tuple", {
  d <- read_descriptor(fixture())

  m <- chosen_model(d)

  expect_equal(m$model_id, d$structural_selection$chosen_model_id)
  expect_true(m$compartments >= 1)
})

test_that("a chosen id absent from the tuple is an error naming what was there", {
  d <- read_descriptor(fixture())
  d$structural_selection$chosen_model_id <- "3cmt"

  expect_error(chosen_model(d), "submitted tuple contains")
})
