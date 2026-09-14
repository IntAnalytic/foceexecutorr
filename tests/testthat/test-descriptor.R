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
  # The whole point of the message is to name what *was* submitted, so a
  # regression that drops the id list but keeps the surrounding wording must
  # also fail this test.
  expect_error(chosen_model(d), "1cmt, 2cmt", fixed = TRUE)
})

test_that("a chosen id that appears twice is an error, not a silent first match", {
  d <- read_descriptor(fixture())
  dup <- chosen_model(d)
  d$structural_selection$submitted_models <- list(d$structural_selection$submitted_models[[1]],
                                                    d$structural_selection$submitted_models[[2]],
                                                    dup)
  expect_error(chosen_model(d), "more than once")
})

test_that("chosen_model_id does not partial-match a similarly named field", {
  d <- read_descriptor(fixture())
  d$structural_selection$chosen_model_id <- NULL
  d$structural_selection$chosen_model_id_prev <- "1cmt"

  expect_error(chosen_model(d), "no chosen_model_id")
})

test_that("a list-wrapped chosen_model_id is rejected, not coerced", {
  d <- read_descriptor(fixture())
  d$structural_selection$chosen_model_id <- list("2cmt")

  expect_error(chosen_model(d), "must be a single string")
})

test_that("a submitted model missing model_id fails with a descriptor-level message", {
  d <- read_descriptor(fixture())
  d$structural_selection$submitted_models[[1]]$model_id <- NULL

  expect_error(chosen_model(d), "missing a valid `model_id`", fixed = TRUE)
})

test_that("empty structural_selection fails clearly instead of inside vapply", {
  d <- read_descriptor(fixture())
  d$structural_selection <- list()

  expect_error(chosen_model(d), "no submitted_models")
})

test_that("read_descriptor() attaches the real source path, not a stale one", {
  d <- read_descriptor(fixture())

  expect_equal(d$.source, fixture())
})

test_that("a .source field inside the JSON cannot shadow the real path", {
  tmp <- tempfile(fileext = ".json")
  raw <- jsonlite::fromJSON(fixture(), simplifyVector = FALSE)
  raw$.source <- "/attacker/controlled/path.json"
  jsonlite::write_json(raw, tmp, auto_unbox = TRUE)

  d <- read_descriptor(tmp)

  expect_equal(d$.source, tmp)
})

test_that("a directory is refused, not handed to the JSON parser", {
  expect_error(read_descriptor(tempdir()), "directory")
})

test_that("an empty file is refused, not handed to the JSON parser", {
  tmp <- tempfile(fileext = ".json")
  file.create(tmp)

  expect_error(read_descriptor(tmp), "empty")
})

test_that("a null required field is refused like a missing one", {
  tmp <- tempfile(fileext = ".json")
  raw <- jsonlite::fromJSON(fixture(), simplifyVector = FALSE)
  # `raw["run_id"] <- list(NULL)`, not `raw$run_id <- NULL` -- the latter
  # removes the list element entirely in R, which would only test the
  # already-covered "key absent" case rather than an explicit JSON `null`.
  raw["run_id"] <- list(NULL)
  jsonlite::write_json(raw, tmp, auto_unbox = TRUE, null = "null")

  expect_error(read_descriptor(tmp), "missing required field.*run_id")
})

test_that("a schema_version serialised as a double or a string is still accepted", {
  # jsonlite::write_json() collapses a whole-number double back down to `1`
  # on the wire, which would silently stop this from testing the double case
  # at all -- so the double case is built by editing the JSON text directly,
  # the only way to get a literal `1.0` onto disk.
  as_double <- tempfile(fileext = ".json")
  writeLines(sub('"schema_version": 1,', '"schema_version": 1.0,',
                 readLines(fixture(), warn = FALSE), fixed = TRUE),
             as_double)
  expect_equal(read_descriptor(as_double)$schema_version, 1)

  as_string <- tempfile(fileext = ".json")
  raw <- jsonlite::fromJSON(fixture(), simplifyVector = FALSE)
  raw$schema_version <- "1"
  jsonlite::write_json(raw, as_string, auto_unbox = TRUE)
  expect_equal(read_descriptor(as_string)$schema_version, "1")
})

test_that("a null schema_version is refused as a missing field", {
  tmp <- tempfile(fileext = ".json")
  raw <- jsonlite::fromJSON(fixture(), simplifyVector = FALSE)
  raw["schema_version"] <- list(NULL)
  jsonlite::write_json(raw, tmp, auto_unbox = TRUE, null = "null")

  expect_error(read_descriptor(tmp), "missing required field.*schema_version")
})
