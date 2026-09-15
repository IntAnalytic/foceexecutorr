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
#'   the originating system has to say where the data actually is. Always
#'   passed to the backend, even when omitted here -- an omitted `data_path`
#'   reaches `run()` as an explicit `NULL`, not as an absent argument, so a
#'   backend wanting its own default for a missing data path must check for
#'   `NULL` itself rather than relying on argument matching.
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
  # Validated here, using execute()'s own parameter names, rather than left
  # to read_descriptor()/resolve_backend(): those report failures about
  # `path`/`name`, arguments this function's caller never actually passed,
  # e.g. execute(backend = 1) would otherwise surface a message about `name`
  # instead of naming `backend`.
  if (is.character(descriptor)) {
    if (length(descriptor) != 1L || is.na(descriptor)) {
      stop("`descriptor` must be a single file path when given as a string.", call. = FALSE)
    }
    descriptor <- read_descriptor(descriptor)
  }
  if (!inherits(descriptor, "focex_descriptor")) {
    stop("`descriptor` must be a `focex_descriptor` (from read_descriptor()) or a path ",
         "to a model.json, not ", article_for(class(descriptor)[1]), " ", class(descriptor)[1],
         ".", call. = FALSE)
  }
  if (!is_nonempty_name(backend)) {
    stop("`backend` must be a short identifier naming a registered backend: ",
         "1-200 bytes, no whitespace.", call. = FALSE)
  }
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
