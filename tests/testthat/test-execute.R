test_that("execute() rejects a non-descriptor, non-path input with a clear message", {
  # "focex_descriptor" alone doesn't pin this: stopifnot(inherits(...))'s own
  # default message ("inherits(descriptor, \"focex_descriptor\") is not TRUE")
  # contains that substring too, so it would pass just the same if this ever
  # regressed back to stopifnot(). "must be a" is unique to the replacement.
  expect_error(execute(5), "must be a", fixed = TRUE)
  expect_error(execute(list()), "must be a", fixed = TRUE)
})

test_that("execute()'s own argument errors name execute()'s own parameters", {
  d <- read_descriptor(fixture())
  # Previously delegated straight to resolve_backend()/read_descriptor(),
  # surfacing THEIR parameter names ("name", "path") instead of the ones
  # this call actually used ("backend", "descriptor").
  expect_error(execute(d, backend = 1), "`backend` must be a short identifier",
               fixed = TRUE)
  expect_error(execute(c("a", "b")), "`descriptor` must be a single file path",
               fixed = TRUE)
})

test_that("execute accepts a path or a descriptor and reports both ways the same", {
  from_path <- execute(fixture())
  from_obj <- execute(read_descriptor(fixture()))

  expect_equal(from_path$model_id, from_obj$model_id)
  expect_s3_class(from_path, "focex_result")
})

test_that("the default backend runs nothing and says so", {
  res <- execute(fixture())

  expect_false(res$result$executed)
  expect_false(res$official)
})

test_that("the result carries the model the descriptor actually chose", {
  d <- read_descriptor(fixture())

  res <- execute(d)

  expect_equal(res$model_id, chosen_model(d)$model_id)
  expect_equal(res$result$would_run$compartments, chosen_model(d)$compartments)
  # would_run$model_id is reported by the inspect backend itself, a separate
  # code path from the outer $model_id execute() assigns directly -- this
  # would still pass if the backend reported the wrong id there while the
  # outer field stayed correct.
  expect_equal(res$result$would_run$model_id, chosen_model(d)$model_id)
})

test_that("would_run$covariates falls back to character() when there are none", {
  d <- read_descriptor(fixture())
  d$structural_selection$submitted_models[[2]]$covariates <- NULL

  res <- execute(d)

  expect_equal(res$result$would_run$covariates, character())
})

test_that("would_run$covariates reads the plain `covariates` field on a schema v1 descriptor", {
  d <- read_descriptor(fixture())

  res <- execute(d)

  expect_equal(res$result$would_run$covariates, c("weight", "age"))
})

test_that("would_run$covariates reports the retained relationships from the covariate-refined model", {
  # The v2 fixture's covariate_search resolved to cov-step3, which carries the
  # real, retained covariate relationships (weight on CL and V1, age on CL) --
  # NOT the declared candidate list (age/weight/sex) that was merely tested.
  # Reporting the candidates here would overstate the model: sex was declared
  # but not retained, and neither weight:V1 nor age:CL are simple names.
  d <- read_descriptor(fixture_v2())

  res <- execute(d)

  expect_equal(res$model_id, "cov-step3")
  expect_equal(sort(res$result$would_run$covariates), c("age:CL", "weight:CL", "weight:V1"))
})

test_that("would_run$covariates still reports the real relationships after a jsonlite round trip", {
  # cov-step3's declared_covariates is `null` in the original file. jsonlite's
  # default JSON writer serialises that NULL as `{}`, not `null` -- so after
  # an ordinary read-and-rewrite (no `null = "null"`), declared_covariates
  # comes back as an empty, non-NULL list. `%||%`-style "is it NULL" alone
  # would treat that as "present" and report an empty covariates list,
  # discarding the real, still-intact weight:CL/weight:V1/age:CL relationships
  # sitting right there in `covariates`.
  d <- read_descriptor(fixture_v2())
  resaved <- round_trip(d)
  res <- execute(resaved)

  expect_equal(res$model_id, "cov-step3")
  expect_equal(sort(res$result$would_run$covariates), c("age:CL", "weight:CL", "weight:V1"))
})

test_that("would_run$absorption carries the route of administration for a v2 descriptor", {
  d <- read_descriptor(fixture_v2())

  res <- execute(d)

  expect_equal(res$result$would_run$absorption, "iv-bolus")
})

test_that("would_run$absorption is NULL for a schema v1 descriptor, which has no such field", {
  d <- read_descriptor(fixture())

  res <- execute(d)

  expect_null(res$result$would_run$absorption)
})

test_that("would_run$absorption stays NULL, not an empty list, after a jsonlite round trip", {
  # An explicit `null` (not an absent key -- `["absorption"] <- list(NULL)`,
  # not `$absorption <- NULL`, which would remove the key instead) survives a
  # write_json()/read_descriptor() round trip as jsonlite's `{}` shape unless
  # read_descriptor() normalizes it back. Left unfixed, `model[["absorption"]]`
  # would be a length-0 list rather than NULL, which is.null() doesn't catch --
  # print.focex_descriptor() would emit a dangling ", )" and this field would
  # be a `list()`, not NULL.
  d <- read_descriptor(fixture())
  d$structural_selection$submitted_models[[2]]["absorption"] <- list(NULL)
  resaved <- round_trip(d)
  res <- execute(resaved)

  expect_null(res$result$would_run$absorption)
  expect_false(is.list(res$result$would_run$absorption))
})

test_that("would_run$covariates prefers the model's own covariates over declared_covariates when both are present", {
  # Constructed so both fields are simultaneously non-empty with DIFFERENT
  # content -- neither real fixture ever does this (only one of the two is
  # ever populated at a time for any given submitted model), so a test built
  # from either fixture alone cannot distinguish the correct precedence from
  # its reverse. The model's own, confirmed `covariates` must win over
  # `declared_covariates`, which is only ever a candidate list.
  d <- read_descriptor(fixture())
  d$structural_selection$submitted_models[[2]]$covariates <- list("weight:CL")
  d$structural_selection$submitted_models[[2]]$declared_covariates <- list("age", "weight", "sex")

  res <- execute(d)

  expect_equal(res$result$would_run$covariates, "weight:CL")
})

test_that("would_run$covariates does not drop covariates assigned as a plain character vector", {
  # read_descriptor() always produces `covariates` as a list (JSON arrays
  # parse with simplifyVector = FALSE), but nothing stops a caller from
  # assigning a plain atomic vector directly, as this test does -- and
  # `is.list(c("weight", "age"))` is FALSE even though the data is real. The
  # presence check must be shape-agnostic (`length(x) > 0L`, not
  # `is.list(x) && length(x) > 0L`), or real data in this shape is silently
  # discarded in favour of an empty fallback.
  d <- read_descriptor(fixture())
  d$structural_selection$submitted_models[[2]]$covariates <- c("weight", "age")

  res <- execute(d)

  expect_equal(res$result$would_run$covariates, c("weight", "age"))
})

test_that("would_run$covariates does not fall back to declared candidates for a covariate_search-confirmed empty model", {
  # cov-step0 in the real fixture ("Base (no covariates)") is exactly this
  # case: covariate_search ran and confirmed no covariate effects, so its
  # `covariates` is genuinely, deliberately empty -- not "not decided yet".
  # Falling back to declared_covariates there would misreport untested
  # candidates as if the base model had kept them. Neither real fixture
  # actually has a non-null declared_covariates on a covariate_search ladder
  # entry to exercise this with, so it's constructed directly.
  d <- read_descriptor(fixture_v2())
  d$covariate_search$final_model_id <- "cov-step0"
  d$covariate_search$submitted_ladder[[1]]$declared_covariates <- list("age", "weight", "sex")

  res <- execute(d)

  expect_equal(res$model_id, "cov-step0")
  expect_equal(res$result$would_run$covariates, character())
})

test_that("would_run$covariates falls back to the declared candidates when covariate_search hasn't run", {
  # A schema v2 descriptor can legitimately have covariate_search still null
  # -- structural selection done, covariate testing not yet started. There
  # chosen_model() resolves a structural_selection entry, whose `covariates`
  # is always `[]` in v2; declared_covariates is the only signal available.
  d <- read_descriptor(fixture_v2())
  d$covariate_search <- NULL

  res <- execute(d)

  expect_equal(res$model_id, "2cmt")
  expect_equal(sort(res$result$would_run$covariates), c("age", "sex", "weight"))
})

test_that("would_run$seed does not partial-match a similarly named field", {
  # The inspect backend reads model fields with `$`, which partial-matches:
  # a model missing `seed` but carrying `seed_source` would otherwise have
  # that field's value reported as the seed here.
  d <- read_descriptor(fixture())
  d$structural_selection$submitted_models[[2]]$seed <- NULL
  d$structural_selection$submitted_models[[2]]$seed_source <- "operator"

  res <- execute(d)

  expect_null(res$result$would_run$seed)
})

test_that("the result carries the descriptor's run_id, not a placeholder", {
  d <- read_descriptor(fixture())

  res <- execute(d)

  expect_equal(res$run_id, d$run_id)
  expect_true(nzchar(res$run_id))
})

test_that("the result names the backend that actually ran, not a fixed label", {
  register_backend("label-probe", function(...) list(ok = TRUE),
                    official = FALSE, description = "checks the recorded backend name")
  on.exit(rm("label-probe", envir = foceexecutorr:::.registry), add = TRUE)

  res <- execute(fixture(), backend = "label-probe")

  expect_equal(res$backend, "label-probe")
})

test_that("execute() reports official = TRUE for a backend registered as official", {
  # The mirror image of the "liar" test in test-backend.R, which shows a
  # backend cannot promote itself to official. This shows the flag actually
  # comes through when the registration itself says TRUE -- execute() must
  # read b$official, not always report FALSE.
  register_backend("official-probe", function(...) list(ok = TRUE),
                    official = TRUE, description = "a genuinely official backend")
  on.exit(rm("official-probe", envir = foceexecutorr:::.registry), add = TRUE)

  res <- execute(fixture(), backend = "official-probe")

  expect_true(res$official)
})

test_that("the data path a caller supplies reaches the backend", {
  # The descriptor names its dataset by catalogue id, so an executor running
  # elsewhere has to be told where the data is; losing that silently would make
  # a backend run against the wrong file.
  res <- execute(fixture(), data_path = "/tmp/analysis-ready.csv")

  expect_equal(res$result$would_run$data_path, "/tmp/analysis-ready.csv")
})

test_that("a caller cannot substitute the model or descriptor via `...`", {
  d <- read_descriptor(fixture())
  rogue <- chosen_model(d)
  rogue$model_id <- "rogue-model"

  expect_error(execute(d, model = rogue), "`model`")
  expect_error(execute(d, data_path = "/tmp/a", model = rogue), "`model`")
})

test_that("the backend receives the model and descriptor by name, not position", {
  # Formals are deliberately in the OPPOSITE order from the call site
  # (descriptor, model, data_path, ...) in execute(). A probe with the same
  # order as the call site would still bind correctly even if execute() went
  # back to calling run() positionally, defeating the point of this test --
  # swapping the order is what makes a positional-call regression visible:
  # `descriptor` would land in the `model` slot and vice versa.
  register_backend(
    "position-probe",
    run = function(model, descriptor, data_path, ...) {
      list(model_id_seen = model$model_id, descriptor_run_id_seen = descriptor$run_id)
    },
    official = FALSE,
    description = "asserts run() was called with named arguments, not positional"
  )
  on.exit(rm("position-probe", envir = foceexecutorr:::.registry), add = TRUE)

  d <- read_descriptor(fixture())
  res <- execute(d, backend = "position-probe")

  expect_equal(res$result$model_id_seen, chosen_model(d)$model_id)
  expect_equal(res$result$descriptor_run_id_seen, d$run_id)
})

test_that("print.focex_result reflects the actual official flag both ways", {
  unofficial <- execute(fixture())
  expect_output(print(unofficial), "official: NO -- not a reportable result", fixed = TRUE)

  register_backend("print-probe", function(...) list(ok = TRUE),
                    official = TRUE, description = "for the print test")
  on.exit(rm("print-probe", envir = foceexecutorr:::.registry), add = TRUE)
  official <- execute(fixture(), backend = "print-probe")

  expect_output(print(official), "official: yes", fixed = TRUE)
})
