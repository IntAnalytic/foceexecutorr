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

test_that("chosen_model() rejects a non-descriptor with a clear message", {
  # "focex_descriptor" alone doesn't pin this: stopifnot(inherits(...))'s own
  # default message ("inherits(descriptor, \"focex_descriptor\") is not TRUE")
  # contains that substring too, so it would pass just the same if this ever
  # regressed back to stopifnot(). "must be a" is unique to the replacement.
  expect_error(chosen_model(5), "must be a", fixed = TRUE)
  expect_error(chosen_model(list()), "must be a", fixed = TRUE)
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
  # expect_equal(x, 1) alone would also pass if this were parsed as the
  # integer 1L -- all.equal() doesn't distinguish -- so it wouldn't actually
  # pin the double-literal parsing path this test exists to cover.
  double_version <- read_descriptor(as_double)$schema_version
  expect_type(double_version, "double")
  expect_equal(double_version, 1)

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

test_that("a boolean schema_version is refused, not coerced to 1", {
  tmp <- tempfile(fileext = ".json")
  writeLines(sub('"schema_version": 1,', '"schema_version": true,',
                 readLines(fixture(), warn = FALSE), fixed = TRUE),
             tmp)

  expect_error(read_descriptor(tmp), "schema version")
})

test_that("a schema_version string is only accepted if it is digits only", {
  reject_as <- function(literal_json_value) {
    tmp <- tempfile(fileext = ".json")
    writeLines(sub('"schema_version": 1,', paste0('"schema_version": ', literal_json_value, ','),
                   readLines(fixture(), warn = FALSE), fixed = TRUE),
               tmp)
    expect_error(read_descriptor(tmp), "schema version")
  }
  reject_as('"0x1"')
  reject_as('" 1 "')
  reject_as('"1e0"')
})

test_that("a scalar top-level JSON document is refused by name, not a crash", {
  for (literal_json in c('"hello"', "5", "true")) {
    tmp <- tempfile(fileext = ".json")
    writeLines(literal_json, tmp)
    expect_error(read_descriptor(tmp), "missing required field")
  }
})

test_that("a non-object structural_selection fails clearly instead of crashing", {
  d <- read_descriptor(fixture())
  d$structural_selection <- "foo"
  expect_error(chosen_model(d), "structural_selection must be an object")

  d2 <- read_descriptor(fixture())
  d2$structural_selection <- 5
  expect_error(chosen_model(d2), "structural_selection must be an object")
})

test_that("the type-mismatch message picks 'a' or 'an' correctly", {
  d <- read_descriptor(fixture())
  d$structural_selection <- 5L
  expect_error(chosen_model(d), "not an integer", fixed = TRUE)
})

test_that("submitted_models entries that are not objects fail clearly instead of crashing", {
  d <- read_descriptor(fixture())
  d$structural_selection$submitted_models <- list("1cmt", "2cmt")
  expect_error(chosen_model(d), "must be an object")
})

test_that("duplicate .source keys in the JSON are all stripped, not just the first", {
  tmp <- tempfile(fileext = ".json")
  lines <- readLines(fixture(), warn = FALSE)
  lines <- append(lines,
                   c('  ".source": "/attacker/first",',
                     '  ".source": "/attacker/second",'),
                   after = 1)
  writeLines(lines, tmp)

  d <- read_descriptor(tmp)

  expect_equal(d$.source, tmp)
})

test_that("print() shows who signed off when sign_off is actually present", {
  d <- read_descriptor(fixture())

  out <- capture.output(print(d))

  expect_true(any(grepl("signed by: Safi Ahmed", out, fixed = TRUE)))
})

test_that("print() does not partial-match sign_off onto a similarly named field", {
  d <- read_descriptor(fixture())
  d$sign_off <- NULL
  d$sign_off_draft <- list(actor = "DRAFT")

  out <- capture.output(print(d))
  expect_false(any(grepl("signed by", out, fixed = TRUE)))
})

test_that("a bare filename that is itself valid JSON is read as a file, not parsed as text", {
  # jsonlite::fromJSON() checks whether its string argument validates as
  # JSON *before* treating it as a path. A path with a directory component
  # ("/tmp/xyz/2026") never parses as JSON on its own, so it always falls
  # through to being read as a file regardless -- the ambiguity only bites
  # for a bare relative filename, which is exactly what "2026" itself is
  # valid JSON as (the literal number 2026).
  dir <- tempfile()
  dir.create(dir)
  file.copy(fixture(), file.path(dir, "2026"))
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)

  d <- read_descriptor("2026")

  expect_s3_class(d, "focex_descriptor")
  expect_equal(d$run_id, "demo-2cmt-wt-age-signed")
})

test_that("an integer beyond exact double precision is preserved, not rounded", {
  # An R double only represents integers exactly up to 2^53; past that the
  # default JSON parse silently rounds -- corrupting a value in what is
  # meant to be a signed, exact record. 9007199254740993 (2^53 + 1) would
  # come back as 9007199254740992 without bigint_as_char = TRUE.
  tmp <- tempfile(fileext = ".json")
  writeLines(sub('"seed": 7,', '"seed": 9007199254740993,',
                 readLines(fixture(), warn = FALSE), fixed = TRUE),
             tmp)

  d <- read_descriptor(tmp)

  expect_equal(chosen_model(d)$seed, "9007199254740993")
})
