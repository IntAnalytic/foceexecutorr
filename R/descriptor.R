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
#' path <- system.file("extdata", "model.json", package = "foceexecutorr")
#' d <- read_descriptor(path)
#' d$run_id
#' @export
read_descriptor <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("`path` must be a single file path.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("no descriptor at '", path, "'.", call. = FALSE)
  }
  if (dir.exists(path)) {
    stop("'", path, "' is a directory, not a descriptor file.", call. = FALSE)
  }
  if (file.info(path)$size == 0L) {
    stop("'", path, "' is empty.", call. = FALSE)
  }
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  required <- c("schema_version", "run_id", "dataset_path", "structural_selection")
  # A key that is present but explicitly `null` is indistinguishable from an
  # absent one here on purpose: both leave nothing downstream can use.
  missing <- required[vapply(required, function(f) is.null(raw[[f]]), logical(1))]
  if (length(missing) > 0L) {
    stop("descriptor is missing required field(s): ", paste(missing, collapse = ", "),
         ". Is this a model.json?", call. = FALSE)
  }
  if (!is_supported_schema_version(raw$schema_version)) {
    found <- describe_schema_version(raw$schema_version)
    stop("descriptor is schema version ", found, "; this package supports ",
         SUPPORTED_SCHEMA_VERSION, ". Upgrade foceexecutorr rather than editing the descriptor -- ",
         "it is a signed record.", call. = FALSE)
  }
  # Strip any field the descriptor itself happens to carry under this name
  # first -- `c()` keeps the first match for a duplicated list name, so an
  # untrusted `.source` in the JSON would otherwise shadow the real path.
  raw[[".source"]] <- NULL
  structure(c(raw, list(.source = path)), class = "focex_descriptor")
}

# schema_version travels through JSON, where an integer, a double and a
# numeric string are all the same "1" to anything that isn't R -- so the
# check has to be on value, not representation, or a descriptor gets
# rejected for a serialisation detail a signed pipeline never promised to
# avoid.
is_supported_schema_version <- function(version) {
  if (is.null(version) || is.list(version) || length(version) != 1L) {
    return(FALSE)
  }
  numeric_version <- suppressWarnings(as.numeric(version))
  !is.na(numeric_version) && numeric_version == SUPPORTED_SCHEMA_VERSION
}

describe_schema_version <- function(version) {
  if (is.null(version)) {
    return("<missing>")
  }
  if (is.list(version) || length(version) != 1L) {
    return(paste0("<", class(version)[1], ">"))
  }
  as.character(version)
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
#' d <- read_descriptor(system.file("extdata", "model.json", package = "foceexecutorr"))
#' chosen_model(d)$model_id
#' @export
chosen_model <- function(descriptor) {
  stopifnot(inherits(descriptor, "focex_descriptor"))
  # `[[` throughout, not `$`: partial name matching on a field like
  # `chosen_model_id` would let a similarly-named field (e.g. a
  # `chosen_model_id_prev` left by an older pipeline version) resolve
  # silently instead of the one the descriptor actually names.
  sel <- descriptor[["structural_selection"]]
  submitted <- sel[["submitted_models"]]
  if (!is.list(submitted) || length(submitted) == 0L) {
    stop("descriptor's structural_selection has no submitted_models to choose from.",
         call. = FALSE)
  }
  ids <- vapply(submitted, function(m) {
    id <- m[["model_id"]]
    if (is.null(id) || length(id) != 1L || !is.character(id)) {
      stop("a submitted model is missing a valid `model_id`.", call. = FALSE)
    }
    id
  }, character(1))

  chosen_id <- sel[["chosen_model_id"]]
  if (is.null(chosen_id)) {
    stop("descriptor's structural_selection has no chosen_model_id.", call. = FALSE)
  }
  if (length(chosen_id) != 1L || !is.character(chosen_id)) {
    stop("descriptor's chosen_model_id must be a single string, not a ",
         class(chosen_id)[1], ".", call. = FALSE)
  }

  hit <- which(ids == chosen_id)
  if (length(hit) == 0L) {
    stop("descriptor names chosen model '", chosen_id,
         "' but the submitted tuple contains: ", paste(ids, collapse = ", "),
         call. = FALSE)
  }
  if (length(hit) > 1L) {
    stop("descriptor names chosen model '", chosen_id,
         "', which appears more than once in the submitted tuple: ",
         paste(ids, collapse = ", "), call. = FALSE)
  }
  submitted[[hit]]
}
