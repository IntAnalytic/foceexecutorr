fixture <- function() system.file("extdata", "model.json", package = "foceexecutorr")

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
