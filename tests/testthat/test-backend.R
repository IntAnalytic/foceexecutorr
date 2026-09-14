test_that("the built-in inspect backend is registered and is not official", {
  reg <- backends()

  expect_true("inspect" %in% reg$name)
  expect_false(reg$official[reg$name == "inspect"])
})

test_that("registering requires an explicit official flag of the right shape", {
  expect_error(register_backend("bad", function(...) NULL, official = NA), "official")
  expect_error(register_backend("bad", "not a function"), "function")
  expect_error(register_backend("", function(...) NULL), "non-empty")
  expect_error(register_backend(NA_character_, function(...) NULL), "non-empty")
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

test_that("resolve_backend() rejects a non-string name with a clear error", {
  expect_error(resolve_backend(1), "single non-NA string")
  expect_error(resolve_backend(NA_character_), "single non-NA string")
  expect_error(resolve_backend(c("inspect", "nonmem")), "single non-NA string")
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

  res <- execute(system.file("extdata", "model.json", package = "foceexecutorr"),
                 backend = "liar")

  expect_false(res$official)
})
