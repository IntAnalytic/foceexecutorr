#' Execute a signed descriptor against a backend
#'
#' The package's single entry point. Reads the descriptor if given a path, picks
#' the model it selected, resolves the named backend, and runs it.
#'
#' The returned object always states which backend produced it and whether that
#' backend is official. That is not decoration: the reason this package exists is
#' that a preview estimator and a qualified one both return plausible numbers,
#' and once two engines can produce results there is nothing in a parameter table
#' that says which one you are looking at. The label is attached here, by this
#' function, from the backend's own registration -- never supplied by the caller.
#'
#' @param descriptor A `focex_descriptor`, or a path to a `model.json`.
#' @param backend Name of a registered backend. Defaults to `"inspect"`, which
#'   runs nothing -- a default that cannot be mistaken for a result.
#' @param data_path Optional path to the analysis-ready dataset. The descriptor
#'   names its dataset by catalogue id, not by path, so a caller running outside
#'   the originating system has to say where the data actually is.
#' @param ... Passed to the backend. May not include `descriptor`, `model`, or
#'   `data_path` -- those are always the ones execute() itself resolved.
#' @return An object of class `focex_result`.
#' @examples
#' d <- system.file("extdata", "model.json", package = "foceexecutorr")
#' res <- execute(d)
#' res$official
#' @export
execute <- function(descriptor, backend = "inspect", data_path = NULL, ...) {
  reserved <- intersect(names(list(...)), c("descriptor", "model", "data_path"))
  if (length(reserved) > 0L) {
    stop("execute()'s `...` may not include ", paste0("`", reserved, "`", collapse = ", "),
         " -- these are supplied by execute() itself, and accepting them through `...` ",
         "would let a caller substitute a different model or descriptor than the one ",
         "the result reports.", call. = FALSE)
  }
  if (is.character(descriptor)) {
    descriptor <- read_descriptor(descriptor)
  }
  stopifnot(inherits(descriptor, "focex_descriptor"))
  b <- resolve_backend(backend)
  model <- chosen_model(descriptor)
  out <- b$run(descriptor = descriptor, model = model, data_path = data_path, ...)
  structure(
    list(
      run_id = descriptor$run_id,
      model_id = model$model_id,
      backend = b$name,
      # Taken from the registry, not from `out`: a backend cannot promote its own
      # results to official by returning a field that says so.
      official = b$official,
      result = out
    ),
    class = "focex_result"
  )
}

#' @export
print.focex_result <- function(x, ...) {
  cat("<focex_result>\n")
  cat("  run:     ", x$run_id, "\n", sep = "")
  cat("  model:   ", x$model_id, "\n", sep = "")
  cat("  backend: ", x$backend, "\n", sep = "")
  cat("  official:", if (isTRUE(x$official)) "yes" else "NO -- not a reportable result", "\n")
  invisible(x)
}
