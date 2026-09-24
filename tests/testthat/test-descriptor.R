test_that("a real signed descriptor reads", {
  d <- read_descriptor(fixture())

  expect_s3_class(d, "focex_descriptor")
  # The fixture is schema v1 specifically -- SUPPORTED_SCHEMA_VERSION is the
  # vector of every version this package accepts, not this fixture's own version.
  expect_equal(d$schema_version, 1)
  expect_true(nzchar(d$run_id))
})

test_that("a schema v2 descriptor -- named the way sentinel-poppk actually names one -- reads too", {
  d <- read_descriptor(fixture_v2())

  expect_s3_class(d, "focex_descriptor")
  expect_equal(d$schema_version, 2)
  expect_true(nzchar(d$run_id))
  # The fixture's covariate_search went on to refine structural_selection's
  # bare "2cmt" pick into "cov-step3" (2cmt + covariate effects) -- that, not
  # the structural candidate, is what chosen_model() must resolve to.
  expect_equal(chosen_model(d)$model_id, d$covariate_search$final_model_id)
  expect_false(chosen_model(d)$model_id == d$structural_selection$chosen_model_id)
})

test_that("a descriptor from a future schema version is refused, not guessed at", {
  # The failure this actually prevents: sentinel-poppk moves on to a schema
  # version beyond what this package's SUPPORTED_SCHEMA_VERSION lists, and
  # this package silently reads the descriptor as if nothing changed.
  tmp <- tempfile(fileext = ".json")
  raw <- jsonlite::fromJSON(fixture(), simplifyVector = FALSE)
  raw$schema_version <- max(SUPPORTED_SCHEMA_VERSION) + 1L
  jsonlite::write_json(raw, tmp, auto_unbox = TRUE)

  expect_error(read_descriptor(tmp), "schema version")
  # Pins the message actually naming every supported version, not just one --
  # a regression back to a scalar-shaped message would still contain "schema
  # version" and pass the check above while silently dropping "2" from it.
  expect_error(read_descriptor(tmp), "this package supports 1, 2", fixed = TRUE)
})

test_that("SUPPORTED_SCHEMA_VERSION is the vector of every version this package accepts", {
  # A caller must use `%in%`, not `==`: a scalar constant would let a
  # newer-but-still-supported descriptor fail a naive equality check even
  # though read_descriptor() itself accepts it fine.
  expect_true(is.numeric(SUPPORTED_SCHEMA_VERSION))
  expect_true(all(c(1L, 2L) %in% SUPPORTED_SCHEMA_VERSION))
})

test_that("chosen_model() prefers covariate_search over structural_selection when both are present", {
  # Built from the v1 fixture (whose own covariate_search is null) with a
  # synthetic covariate_search spliced in, so this pins the gate-resolution
  # PRIORITY directly, rather than relying on what the v2 fixture happens to
  # contain.
  d <- read_descriptor(fixture())
  d$covariate_search <- list(
    submitted_ladder = list(list(model_id = "cov-final", compartments = 2,
                                  error_model = "combined", covariates = list("weight:CL"),
                                  estimation_method = "FOCE-I", seed = 1)),
    final_model_id = "cov-final"
  )

  expect_equal(chosen_model(d)$model_id, "cov-final")
})

test_that("chosen_model() returns the structural_selection entry completely unmodified", {
  # Documented (register_backend()) as safe to compare, hash, or forward to
  # an external engine unchanged -- pinned directly, not just inferred from
  # tests that only check individual fields, so a field ever being added
  # back (accidentally or otherwise) would be caught here.
  d <- read_descriptor(fixture())

  expect_identical(chosen_model(d), d$structural_selection$submitted_models[[2]])
})

test_that("chosen_model() returns the covariate_search entry completely unmodified", {
  d <- read_descriptor(fixture_v2())
  ids <- vapply(d$covariate_search$submitted_ladder, function(m) m$model_id, character(1))
  idx <- which(ids == d$covariate_search$final_model_id)

  expect_identical(chosen_model(d), d$covariate_search$submitted_ladder[[idx]])
})

test_that("resolved_gate() answers the same question chosen_model() uses internally", {
  expect_equal(resolved_gate(read_descriptor(fixture())), "structural_selection")
  expect_equal(resolved_gate(read_descriptor(fixture_v2())), "covariate_search")
})

test_that("resolved_gate() accepts an unclassed list, not just a focex_descriptor", {
  # Deliberately permissive: a backend called directly (bypassing execute())
  # may be handed an unclassed descriptor (see register_backend()'s docs),
  # and resolved_gate() is the function they're told to call for this.
  expect_equal(resolved_gate(unclass(read_descriptor(fixture()))), "structural_selection")
  expect_equal(resolved_gate(unclass(read_descriptor(fixture_v2()))), "covariate_search")
  expect_equal(resolved_gate(list()), "structural_selection")
})

test_that("resolved_gate() rejects a non-list with a clear message", {
  expect_error(resolved_gate(5), "must be a", fixed = TRUE)
  expect_error(resolved_gate("not a descriptor"), "must be a", fixed = TRUE)
})

test_that("a covariate_search key removed at the R level falls back to structural_selection", {
  # `d$covariate_search <- NULL` REMOVES the key from the list (R's usual
  # assignment semantics), so this pins "key absent entirely" -- a genuine
  # JSON `null` is covered separately below, since jsonlite does not
  # necessarily represent the two identically once serialised.
  d <- read_descriptor(fixture())
  d$covariate_search <- NULL

  expect_equal(chosen_model(d)$model_id, d$structural_selection$chosen_model_id)
})

test_that("a genuine JSON null covariate_search falls back to structural_selection", {
  # fixture()'s own file has a literal `"covariate_search": null` -- read it
  # with no R-level manipulation at all, the actual shape read_descriptor()
  # hands back for a real, unmodified v1 descriptor.
  d <- read_descriptor(fixture())

  expect_equal(chosen_model(d)$model_id, d$structural_selection$chosen_model_id)
})

test_that("a covariate_search that is genuinely malformed errors loudly, not silently ignored", {
  d <- read_descriptor(fixture())
  d$covariate_search <- "not an object"

  expect_error(chosen_model(d), "covariate_search must be an object")
})

test_that("a covariate_search present but missing final_model_id errors, does not silently fall back", {
  # A covariate_search with a real submitted_ladder but no decision recorded
  # is not "this gate didn't run" (that's NULL, handled above) -- it is an
  # inconsistent signed record: sign_off cannot meaningfully describe a
  # "final model" that covariate_search never named. Falling back to
  # structural_selection here would silently substitute a materially
  # different, less-refined model for the one such a record claims to
  # report.
  d <- read_descriptor(fixture())
  d$covariate_search <- list(
    submitted_ladder = list(list(model_id = "cov-step0", compartments = 2,
                                  error_model = "combined", covariates = list(),
                                  estimation_method = "FOCE-I", seed = 1))
  )

  expect_error(chosen_model(d), "covariate_search has no final_model_id")
})

test_that("print() reports an unresolvable model instead of silently omitting the line", {
  d <- read_descriptor(fixture())
  d$covariate_search <- list(submitted_ladder = list(list(model_id = "x")))

  out <- capture.output(print(d))

  expect_true(any(grepl("model:.*could not be resolved", out)))
})

test_that("a descriptor round-tripped through jsonlite::write_json() still resolves correctly", {
  # The concrete, realistic failure the tests above are only approximating:
  # read a real v1 descriptor, save it back out the ordinary way (no
  # `null = \"null\"`), read it again -- covariate_search survives as `{}`,
  # not `null`, and must still resolve via structural_selection, not error.
  d <- read_descriptor(fixture())
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(unclass(d), tmp, auto_unbox = TRUE, digits = NA)

  resaved_text <- paste(readLines(tmp, warn = FALSE), collapse = "")
  expect_true(grepl('"covariate_search":{}', resaved_text, fixed = TRUE))

  resaved <- read_descriptor(tmp)
  expect_equal(chosen_model(resaved)$model_id, d$structural_selection$chosen_model_id)
  # Not expect_no_error(): that needs testthat >= 3.1.5, newer than this
  # package's own `Suggests: testthat (>= 3.0.0)` floor. An uncaught error
  # inside a test_that() block already fails the test on its own.
  execute(resaved)
  expect_true(any(grepl("model:", capture.output(print(resaved)))))
})

test_that("print() shows the covariate-refined model, not the bare structural one, for a v2 descriptor", {
  d <- read_descriptor(fixture_v2())

  out <- capture.output(print(d))

  expect_true(any(grepl("model:.*cov-step3", out)))
  expect_false(any(grepl("model:.*\\b2cmt\\b", out)))
})

test_that("print() has no dangling ', )' when absorption is absent", {
  # A NULL absorption (v1 has no such field at all) must produce
  # "...combined error)", not "...combined error, )" -- the latter is what
  # an unnormalized empty-list absorption would print as.
  d <- read_descriptor(fixture())

  out <- capture.output(print(d))

  expect_true(any(grepl("combined error)", out, fixed = TRUE)))
  expect_false(any(grepl(", )", out, fixed = TRUE)))
})

test_that("read_descriptor() normalizes jsonlite's {} shape for a round-tripped null, at any depth", {
  # General coverage for normalize_empty_objects(), independent of any one
  # field chosen_model()/the inspect backend happen to read today: a NULL
  # set explicitly (not via `$<-`, which would remove the key) at the
  # top level, and nested two levels deep inside a submitted model, both
  # come back as jsonlite's `{}` shape after an ordinary write_json() round
  # trip and must both normalize back to real NULL, not an empty list.
  d <- read_descriptor(fixture())
  d["parent_run_id"] <- list(NULL)
  d$structural_selection$submitted_models[[1]]["force_outcome"] <- list(NULL)
  resaved <- round_trip(d)

  expect_null(resaved[["parent_run_id"]])
  expect_false(is.list(resaved[["parent_run_id"]]))
  expect_null(resaved$structural_selection$submitted_models[[1]][["force_outcome"]])
  expect_false(is.list(resaved$structural_selection$submitted_models[[1]][["force_outcome"]]))
  # A genuine empty ARRAY must NOT be normalized away -- only jsonlite's
  # empty-NAMED-list shape for a round-tripped null is.
  expect_true(is.list(resaved$data_handling$imputations))
  expect_length(resaved$data_handling$imputations, 0L)
})

test_that("round_trip() preserves the real fixture's own signed estimate", {
  # jsonlite::write_json()'s default `digits` would silently corrupt this --
  # 4.057944 comes back as 4.0579 -- which every other round-trip test in
  # this file relies on round_trip() NOT doing. Pinned directly here, once,
  # rather than trusted implicitly.
  d <- read_descriptor(fixture_v2())
  resaved <- round_trip(d)

  expect_identical(resaved$parameters[[1]]$estimate, d$parameters[[1]]$estimate)
  expect_identical(resaved$parameters[[1]]$estimate, 4.057944)
})

test_that("round_trip() survives a value a subtly-too-low digits setting would not", {
  # 4.057944 above happens to need exactly 6 decimal places, so it would
  # round-trip unchanged even under a regression to digits = 6 (jsonlite's
  # decimal-place mode) -- it can't by itself catch that kind of
  # subtly-too-low setting. This value needs 11 significant digits and is
  # small enough that jsonlite's decimal-place truncation would zero it out
  # entirely at low digits, not just round it.
  #
  # expect_identical(), not expect_equal(): expect_equal()'s default
  # tolerance (~1.5e-8 relative) is loose enough that a regression to
  # `digits = I(8)` -- jsonlite's SIGNIFICANT-FIGURE mode, keeping 8 of this
  # value's 11 significant digits -- would still pass. Confirmed empirically;
  # expect_identical() catches it, expect_equal() does not.
  d <- read_descriptor(fixture_v2())
  d$parameters[[1]]$estimate <- 0.000012345678901
  resaved <- round_trip(d)

  expect_identical(resaved$parameters[[1]]$estimate, 0.000012345678901)
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

test_that("a schema_version of 2 serialised as a double or a string is also accepted", {
  # The v1-based test above exercises this same type-coercion logic but only
  # ever substitutes in place of "1" -- is_supported_schema_version() checks
  # membership in SUPPORTED_SCHEMA_VERSION generically, but pin the "2"
  # representations too rather than trusting that by inference alone.
  as_double <- tempfile(fileext = ".json")
  writeLines(sub('"schema_version": 2,', '"schema_version": 2.0,',
                 readLines(fixture_v2(), warn = FALSE), fixed = TRUE),
             as_double)
  double_version <- read_descriptor(as_double)$schema_version
  expect_type(double_version, "double")
  expect_equal(double_version, 2)

  as_string <- tempfile(fileext = ".json")
  raw <- jsonlite::fromJSON(fixture_v2(), simplifyVector = FALSE)
  raw$schema_version <- "2"
  jsonlite::write_json(raw, as_string, auto_unbox = TRUE)
  expect_equal(read_descriptor(as_string)$schema_version, "2")
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

test_that("an integer beyond the 64-bit signed range still rounds silently", {
  # bigint_as_char = TRUE only extends exactness to the signed 64-bit range
  # (up to 9223372036854775807); jsonlite itself falls back to a rounded
  # double beyond that. Documented as a known, accepted gap -- a seed in
  # the quintillions is not a realistic concern here -- rather than left
  # as an implicit claim the fix doesn't actually make.
  tmp <- tempfile(fileext = ".json")
  writeLines(sub('"seed": 7,', '"seed": 9223372036854775809,',
                 readLines(fixture(), warn = FALSE), fixed = TRUE),
             tmp)

  d <- read_descriptor(tmp)
  seed <- chosen_model(d)$seed

  expect_type(seed, "double")
  expect_equal(seed, 9223372036854775808)  # rounded, not the 809 in the file
})
