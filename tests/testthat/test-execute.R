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
