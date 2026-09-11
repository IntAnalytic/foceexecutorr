#' Read a signed model descriptor
#'
#' Reads the `model.json` that the sentinel-poppk workflow writes to a run's
#' workspace at report sign-off, and checks that it is one this package knows how
#' to execute.
#'
#' The descriptor is deliberately the *only* input. It is self-contained by
#' design: it carries the full model tuple as originally submitted, each paired
#' with the fingerprint that identifies it, so an executor never needs to call
#' back into the application that produced it.
#'
#' Validation here is intentionally shallow -- presence and shape of the fields
#' this package reads, not a full schema check. A descriptor is produced by a
#' signed, audited pipeline; treating it as hostile input would be theatre. What
#' is checked is the thing that actually goes wrong in practice: being handed a
#' descriptor from a schema version this package predates.
#'
#' @param path Path to a `model.json` file.
#' @return An object of class `focex_descriptor`: the parsed descriptor with its
#'   source path attached.
#' @seealso [chosen_model()] to resolve which model it selected, [execute()] to run it.
#' @examples
#' path <- system.file("extdata", "model.json", package = "foceexecutorR")
#' d <- read_descriptor(path)
#' d$run_id
#' @export
read_descriptor <- function(path) {
  if (!is.character(path) || length(path) != 1L) {
    stop("`path` must be a single file path.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("no descriptor at '", path, "'.", call. = FALSE)
  }
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  required <- c("schema_version", "run_id", "dataset_path", "structural_selection")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0L) {
    stop("descriptor is missing required field(s): ", paste(missing, collapse = ", "),
         ". Is this a model.json?", call. = FALSE)
  }
  if (!identical(raw$schema_version, SUPPORTED_SCHEMA_VERSION)) {
    stop("descriptor is schema version ", raw$schema_version, "; this package supports ",
         SUPPORTED_SCHEMA_VERSION, ". Upgrade foceexecutorR rather than editing the descriptor -- ",
         "it is a signed record.", call. = FALSE)
  }
  structure(c(raw, list(.source = path)), class = "focex_descriptor")
}

#' The descriptor schema version this package executes
#'
#' Exported deliberately. An embedder that wants to check compatibility before
#' handing over a descriptor should be able to ask, rather than parse an error
#' message -- a lesson taken from integrating against a library that offered no
#' such constant.
#' @export
SUPPORTED_SCHEMA_VERSION <- 1L

#' @export
print.focex_descriptor <- function(x, ...) {
  cat("<focex_descriptor>\n")
  cat("  run:      ", x$run_id, "\n", sep = "")
  cat("  dataset:  ", x$dataset_path, "\n", sep = "")
  chosen <- tryCatch(chosen_model(x), error = function(e) NULL)
  if (!is.null(chosen)) {
    cat("  model:    ", chosen$model_id, " (", chosen$compartments, "-compartment, ",
        chosen$error_model, " error)\n", sep = "")
  }
  if (!is.null(x$sign_off$actor)) {
    cat("  signed by:", x$sign_off$actor, "at", x$sign_off$decided_at, "\n")
  }
  invisible(x)
}

#' Resolve the model a descriptor selected
#'
#' A descriptor carries every candidate that was submitted, in the order it was
#' entered, plus the id of the one chosen at the structural-selection gate. This
#' returns the chosen one.
#'
#' It reads the submitted tuple rather than trusting a single embedded copy of
#' the winner on purpose: the tuple is what the fingerprint covers, and replaying
#' one model in isolation does not reproduce the numbers the original run
#' produced.
#'
#' @param descriptor A `focex_descriptor` from [read_descriptor()].
#' @return A list describing the chosen model.
#' @examples
#' d <- read_descriptor(system.file("extdata", "model.json", package = "foceexecutorR"))
#' chosen_model(d)$model_id
#' @export
chosen_model <- function(descriptor) {
  stopifnot(inherits(descriptor, "focex_descriptor"))
  sel <- descriptor$structural_selection
  submitted <- sel$submitted_models
  ids <- vapply(submitted, function(m) m$model_id, character(1))
  hit <- which(ids == sel$chosen_model_id)
  if (length(hit) != 1L) {
    stop("descriptor names chosen model '", sel$chosen_model_id,
         "' but the submitted tuple contains: ", paste(ids, collapse = ", "),
         call. = FALSE)
  }
  submitted[[hit]]
}
