test_that("the built-in inspect backend is registered and is not official", {
  reg <- backends()

  expect_true("inspect" %in% reg$name)
  expect_false(reg$official[reg$name == "inspect"])
})

test_that("registering requires an explicit official flag of the right shape", {
  expect_error(register_backend("bad", function(...) NULL, official = NA), "official")
  expect_error(register_backend("bad", "not a function"), "function")
  expect_error(register_backend("", function(...) NULL), "non-empty")
})

test_that("an unknown backend names the ones that exist", {
  expect_error(resolve_backend("nonmem"), "no backend named")
  expect_error(resolve_backend("nonmem"), "inspect")
})

test_that("registering an existing name errors unless overwrite = TRUE", {
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
