test_that("the built-in inspect backend is registered and is not official", {
  reg <- backends()

  expect_true("inspect" %in% reg$name)
  expect_false(reg$official[reg$name == "inspect"])
})

test_that("the inspect backend's run() does not require a classed focex_descriptor", {
  # A backend's own contract only promises `descriptor[["covariate_search"]]`
  # is reachable, not that `descriptor` carries this package's S3 class --
  # execute() always supplies a real one, but resolve_backend() is exported,
  # so a caller invoking run() directly (bypassing execute()) with an
  # unclassed descriptor (e.g. unclass(d)) must not fail depending on
  # unrelated data. Previously this worked when the model's own `covariates`
  # was non-empty (v1's structural entries, or a covariate_search-resolved
  # model) but errored the moment `covariates` was empty (a v2
  # structural_selection candidate before covariate testing), since only that
  # branch called resolved_gate() -- which required a classed descriptor at
  # the time. resolved_gate() now accepts any list, closing this for good.
  run <- resolve_backend("inspect")$run

  d1 <- read_descriptor(fixture())
  m1 <- chosen_model(d1)
  # Not expect_no_error(): needs testthat >= 3.1.5, newer than this
  # package's own floor. An uncaught error already fails the test.
  run(descriptor = unclass(d1), model = m1, data_path = NULL)

  d2 <- read_descriptor(fixture_v2())
  d2$covariate_search <- NULL
  empty_covariates_model <- d2$structural_selection$submitted_models[[1]]
  out <- run(descriptor = unclass(d2), model = empty_covariates_model, data_path = NULL)
  expect_equal(sort(out$would_run$covariates), c("age", "sex", "weight"))
})

test_that("registering requires an explicit official flag of the right shape", {
  expect_error(register_backend("bad", function(...) NULL, official = NA), "official")
  expect_error(register_backend("bad", "not a function"), "function")
  expect_error(register_backend("", function(...) NULL), "short identifier")
  expect_error(register_backend(NA_character_, function(...) NULL), "short identifier")
  expect_error(register_backend("   ", function(...) NULL), "short identifier")
})

test_that("a name with leading, trailing, or internal whitespace is rejected", {
  # Rejecting outright, rather than trimming and registering under the
  # trimmed name, avoids " inspect" and "inspect" coexisting as visually
  # near-identical but distinct registry entries.
  expect_error(register_backend(" inspect", function(...) NULL), "short identifier")
  expect_error(register_backend("inspect ", function(...) NULL), "short identifier")
  expect_error(register_backend("in spect", function(...) NULL), "short identifier")
})

test_that("a name that is only a non-breaking space is rejected", {
  # nzchar(trimws(x)) alone treated this as non-empty: base trimws() strips
  # ASCII whitespace only, not exotic Unicode whitespace like U+00A0.
  expect_error(register_backend(" ", function(...) NULL), "short identifier")
  expect_error(resolve_backend(" "), "short identifier")
})

test_that("an oversized name is rejected with a clear message, not exists()'s own error", {
  # Previously reached exists() uncaught, which errors with R's own
  # "variable names are limited to 10000 bytes" instead of a package message.
  huge <- strrep("a", 10001L)
  expect_error(register_backend(huge, function(...) NULL), "short identifier")
  expect_error(resolve_backend(huge), "short identifier")
})

test_that("a name with invalid UTF-8 bytes is rejected, not registered as mojibake", {
  # perl = TRUE regex on invalid UTF-8 warns ("input string 1 is invalid
  # UTF-8") and its match result can't be trusted -- without a validUTF8()
  # check first, this slipped past the whitespace check and registered.
  bad <- rawToChar(as.raw(c(0x66, 0x6f, 0x6f, 0xff, 0xfe)))
  expect_error(register_backend(bad, function(...) NULL), "short identifier")
  expect_error(resolve_backend(bad), "short identifier")
})

test_that("registering requires a description that is a single string", {
  expect_error(register_backend("bad", function(...) NULL, description = NULL), "description")
  expect_error(register_backend("bad", function(...) NULL, description = c("a", "b")),
               "description")
  expect_error(register_backend("bad", function(...) NULL, description = NA_character_),
               "description")
})

test_that("register_backend() defaults official to FALSE", {
  register_backend("default-official-probe", function(...) NULL)
  on.exit(rm("default-official-probe", envir = foceexecutorr:::.registry), add = TRUE)

  expect_false(resolve_backend("default-official-probe")$official)
})

test_that("a dot-prefixed backend name is still listed and resolvable", {
  register_backend(".hidden-probe", function(...) NULL, official = FALSE, description = "x")
  on.exit(rm(".hidden-probe", envir = foceexecutorr:::.registry), add = TRUE)

  expect_true(".hidden-probe" %in% backends()$name)
  expect_error(resolve_backend("nonmem"), ".hidden-probe", fixed = TRUE)
})

test_that("resolve_backend() rejects a non-string or empty name with a clear error", {
  expect_error(resolve_backend(1), "short identifier")
  expect_error(resolve_backend(NA_character_), "short identifier")
  expect_error(resolve_backend(c("inspect", "nonmem")), "short identifier")
  # Previously leaked R's bare "invalid first argument" from exists(), since
  # the guard checked type and NA but not nzchar().
  expect_error(resolve_backend(""), "short identifier")
  expect_error(resolve_backend("   "), "short identifier")
})

test_that("an unknown backend names the ones that exist", {
  expect_error(resolve_backend("nonmem"), "no backend named")
  expect_error(resolve_backend("nonmem"), "inspect")
})

test_that("registering an existing name errors unless overwrite = TRUE", {
  # Restore "inspect" regardless of outcome, the way the "liar" test below
  # cleans up its own registration -- if this guard ever regresses, the
  # overwrite below would otherwise persist into every later test in the
  # suite (shared mutable `.registry`), turning one clear local failure into
  # a wall of unrelated ones in test-execute.R.
  original <- resolve_backend("inspect")
  on.exit(assign("inspect", original, envir = foceexecutorr:::.registry), add = TRUE)

  expect_error(
    register_backend("inspect", function(...) NULL, official = TRUE),
    "already registered"
  )
  # The exact failure this guard exists for: re-registering a trusted
  # unofficial backend under the same name must not silently make it official.
  expect_false(resolve_backend("inspect")$official)
})

test_that("overwrite = TRUE replaces a backend deliberately", {
  register_backend("demo-overwrite", function(...) NULL, official = FALSE)
  on.exit(rm("demo-overwrite", envir = foceexecutorr:::.registry), add = TRUE)

  register_backend("demo-overwrite", function(...) NULL, official = TRUE, overwrite = TRUE)

  expect_true(resolve_backend("demo-overwrite")$official)
})

test_that("a backend cannot promote its own result to official", {
  # The property the whole registry exists for: `official` is read from the
  # registration, so a backend returning official = TRUE changes nothing.
  register_backend("liar", function(descriptor, model, data_path, ...) list(official = TRUE),
                   official = FALSE, description = "claims to be official")
  on.exit(rm("liar", envir = foceexecutorr:::.registry), add = TRUE)

  res <- execute(fixture(), backend = "liar")

  expect_false(res$official)
})
