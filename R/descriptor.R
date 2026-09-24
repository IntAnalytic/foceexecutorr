#' Read a signed model descriptor
#'
#' Reads the descriptor that the sentinel-poppk workflow writes to a run's
#' workspace at report sign-off -- named after the run (e.g.
#' `model-run-<run-id>.json`), not literally `model.json` -- and checks that it
#' is one this package knows how to execute.
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
#' @param path Path to a descriptor file, e.g. `model-run-<run-id>.json`.
#' @return An object of class `focex_descriptor`: the parsed descriptor with its
#'   source path attached.
#' @seealso [chosen_model()] to resolve which model it selected, [execute()] to run it.
#' @examples
#' path <- system.file("extdata", "model.json", package = "foceexecutorr")
#' d <- read_descriptor(path)
#' d$run_id
#'
#' # schema version 2, named the way sentinel-poppk actually names a descriptor
#' path2 <- system.file("extdata", "model-run-fbd67a28-06a8-40b4-9cb1-e14e6cc49ff3.json",
#'                       package = "foceexecutorr")
#' read_descriptor(path2)$schema_version
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
  # read_json(), not fromJSON(): fromJSON() checks whether its string
  # argument itself validates as JSON before treating it as a path, so a
  # file whose name happens to be valid JSON on its own (e.g. "2026") would
  # be parsed as that literal value instead of ever being opened.
  #
  # bigint_as_char = TRUE: an R double only represents integers exactly up
  # to 2^53. Past that, the default silently rounds -- corrupting a value
  # in what is meant to be a signed, exact record without any indication.
  # Returning the digits as a string instead is honest about what was
  # actually in the file, even though it changes that field's R type.
  # This only extends exactness to the signed 64-bit range (up to
  # 9223372036854775807): jsonlite itself falls back to a rounded double
  # beyond that, silently again. Seed values in the quintillions are not a
  # realistic concern for this package, so that residual gap is accepted
  # rather than guarded against.
  raw <- jsonlite::read_json(path, simplifyVector = FALSE, bigint_as_char = TRUE)
  raw <- normalize_empty_objects(raw)
  required <- c("schema_version", "run_id", "dataset_path", "structural_selection")
  # A top-level JSON scalar ("hello", 5, true) parses to an atomic vector,
  # not a list; `[[` on it errors ("subscript out of bounds") instead of
  # returning NULL, so treat it as an object with none of the required
  # fields rather than indexing into it directly.
  fields <- if (is.list(raw)) raw else list()
  # A key that is present but explicitly `null` is indistinguishable from an
  # absent one here on purpose: both leave nothing downstream can use.
  missing <- required[vapply(required, function(f) is.null(fields[[f]]), logical(1))]
  if (length(missing) > 0L) {
    stop("descriptor is missing required field(s): ", paste(missing, collapse = ", "),
         ". Is this a sentinel-poppk descriptor?", call. = FALSE)
  }
  if (!is_supported_schema_version(raw$schema_version)) {
    found <- describe_schema_version(raw$schema_version)
    stop("descriptor is schema version ", found, "; this package supports ",
         paste(SUPPORTED_SCHEMA_VERSION, collapse = ", "),
         ". Upgrade foceexecutorr rather than editing the descriptor -- ",
         "it is a signed record.", call. = FALSE)
  }
  # Strip every field the descriptor itself carries under this name first --
  # a duplicated JSON key parses to duplicate list entries, and `[[<-`
  # clears only the first match, so a repeated `.source` key would still
  # shadow the real path with `raw[[".source"]] <- NULL` alone.
  raw <- raw[names(raw) != ".source"]
  structure(c(raw, list(.source = path)), class = "focex_descriptor")
}

# jsonlite's default JSON writer (`jsonlite::write_json()`/`toJSON()`,
# without `null = "null"`) serialises an R NULL as `{}`, not `null`. Read a
# descriptor with `jsonlite::read_json(..., simplifyVector = FALSE)`, save it
# back out that ordinary way, and read it again, and every field that was
# originally `null` -- at any depth, not just the ones this package happens
# to read today -- comes back as an empty *named* list instead of NULL:
# `is.null(names(x))` is what actually distinguishes it from a genuine empty
# JSON array (`[]`, an empty *unnamed* list, which must NOT be normalized
# away -- nothing in this schema uses `{}` as a meaningful value in its own
# right, but a real empty array, e.g. `"imputations": []`, is not the same
# thing as absent).
#
# Normalized once, here, right after parsing, rather than at each of the
# many places downstream that read an optional field -- that approach was
# tried first and missed fields (`absorption`, `seed`) that nothing had
# reason to specifically suspect until they broke.
normalize_empty_objects <- function(x) {
  if (is.list(x)) {
    if (length(x) == 0L && !is.null(names(x))) {
      return(NULL)
    }
    return(lapply(x, normalize_empty_objects))
  }
  x
}

# schema_version travels through JSON, where an integer, a double and a
# numeric string are all the same "1" to anything that isn't R -- so the
# check has to be on value, not representation, or a descriptor gets
# rejected for a serialisation detail a signed pipeline never promised to
# avoid. That leniency stops at type, though: a JSON boolean is not a
# version number, and a string is only a number if it's nothing else --
# `as.numeric()` alone would also accept "0x1", " 1 " and "1e0".
is_supported_schema_version <- function(version) {
  if (is.null(version) || is.list(version) || length(version) != 1L) {
    return(FALSE)
  }
  # No separate is.logical() rejection needed: is.numeric(TRUE) and
  # is.character(TRUE) are both FALSE, so a logical already falls through
  # to the final FALSE below on its own.
  if (is.numeric(version)) {
    return(!is.na(version) && version %in% SUPPORTED_SCHEMA_VERSION)
  }
  if (is.character(version) && grepl("^[0-9]+$", version)) {
    return(as.numeric(version) %in% SUPPORTED_SCHEMA_VERSION)
  }
  FALSE
}

# "not a integer" reads wrong; "not an integer" doesn't -- the only common
# class() result here where it matters, but cheap to get right in general.
article_for <- function(word) {
  if (grepl("^[aeiouAEIOU]", word)) "an" else "a"
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

#' The descriptor schema versions this package can read
#'
#' Exported deliberately. An embedder that wants to check compatibility before
#' handing over a descriptor should be able to ask, rather than parse an error
#' message -- a lesson taken from integrating against a library that offered no
#' such constant.
#'
#' This is an integer vector, not a scalar: `schema_version %in%
#' SUPPORTED_SCHEMA_VERSION` is the correct check, not equality against a
#' single value. Equality would quietly reject a valid, newer descriptor
#' instead of accepting it -- e.g. `schema_version == SUPPORTED_SCHEMA_VERSION`
#' is `FALSE` for a schema v2 descriptor even though `read_descriptor()`
#' accepts it fine.
#' @format An integer vector, ascending, of every schema version this package
#'   accepts -- not just the newest one. A schema v1 descriptor signed before
#'   sentinel-poppk added v2 is still a valid, executable record.
#' @export
SUPPORTED_SCHEMA_VERSION <- c(1L, 2L)

#' @export
print.focex_descriptor <- function(x, ...) {
  cat("<focex_descriptor>\n")
  cat("  run:      ", x$run_id, "\n", sep = "")
  cat("  dataset:  ", x$dataset_path, "\n", sep = "")
  chosen <- tryCatch(chosen_model(x), error = function(e) NULL)
  if (!is.null(chosen)) {
    absorption <- if (is.null(chosen[["absorption"]])) "" else paste0(", ", chosen[["absorption"]])
    cat("  model:    ", chosen$model_id, " (", chosen$compartments, "-compartment, ",
        chosen$error_model, " error", absorption, ")\n", sep = "")
  }
  # `[[`, not `$`: with `sign_off` absent but a similarly-named field (e.g.
  # a draft `sign_off_draft`) present, `$` would partial-match onto it and
  # print that field's contents as if the descriptor were actually signed.
  sign_off <- x[["sign_off"]]
  if (is.list(sign_off) && !is.null(sign_off[["actor"]])) {
    cat("  signed by:", sign_off[["actor"]], "at", sign_off[["decided_at"]], "\n")
  }
  invisible(x)
}

#' Resolve the model a descriptor selected
#'
#' A descriptor can carry more than one selection gate. `structural_selection`
#' always exists; `covariate_search` -- present once a run went on to test
#' covariate relationships -- refines it further. This returns the model
#' chosen at the *last* gate that actually ran: `covariate_search`'s
#' `final_model_id` when `covariate_search` is present, otherwise
#' `structural_selection`'s `chosen_model_id`.
#'
#' Reading only `structural_selection` would report a bare structural
#' candidate -- no covariate effects at all -- as "the chosen model" even when
#' the descriptor's own `sign_off` describes a covariate-refined model as what
#' was actually reported and qualified.
#'
#' It reads the submitted tuple at that gate rather than trusting a single
#' embedded copy of the winner on purpose: the tuple is what the fingerprint
#' covers, and replaying one model in isolation does not reproduce the
#' numbers the original run produced.
#'
#' @param descriptor A `focex_descriptor` from [read_descriptor()].
#' @return A list describing the chosen model.
#' @examples
#' d <- read_descriptor(system.file("extdata", "model.json", package = "foceexecutorr"))
#' chosen_model(d)$model_id
#' @export
chosen_model <- function(descriptor) {
  if (!inherits(descriptor, "focex_descriptor")) {
    stop("`descriptor` must be a `focex_descriptor` from read_descriptor(), not ",
         article_for(class(descriptor)[1]), " ", class(descriptor)[1], ".", call. = FALSE)
  }
  # `[[`, not `$`: a `covariate_search` that is `null` -- the convention
  # read_descriptor() already uses for "this doesn't apply" -- means the gate
  # never ran, same as the key being absent entirely. A plain `is.null()`
  # check is enough: read_descriptor() already normalizes the empty-object
  # shape jsonlite's own JSON writer produces for a round-tripped NULL back
  # to a real NULL before this ever runs (see normalize_empty_objects()). A
  # covariate_search that is present but genuinely malformed (some other,
  # non-empty, non-object value) still fails loudly in resolve_gate() below.
  cov <- descriptor[["covariate_search"]]
  if (!is.null(cov)) {
    return(resolve_gate(cov, "covariate_search", "submitted_ladder", "final_model_id"))
  }
  resolve_gate(descriptor[["structural_selection"]], "structural_selection",
               "submitted_models", "chosen_model_id")
}

# Shared by chosen_model()'s two gates -- structural_selection and
# covariate_search have the same shape (a submitted list plus the id of the
# one chosen from it), just under different field names.
#
# `[[` throughout, not `$`: partial name matching on a field like
# `chosen_model_id` would let a similarly-named field (e.g. a
# `chosen_model_id_prev` left by an older pipeline version) resolve silently
# instead of the one the descriptor actually names.
resolve_gate <- function(gate, gate_name, submitted_field, chosen_field) {
  if (!is.list(gate)) {
    stop("descriptor's ", gate_name, " must be an object, not ",
         article_for(class(gate)[1]), " ", class(gate)[1], ".", call. = FALSE)
  }
  submitted <- gate[[submitted_field]]
  if (!is.list(submitted) || length(submitted) == 0L) {
    stop("descriptor's ", gate_name, " has no ", submitted_field, " to choose from.",
         call. = FALSE)
  }
  ids <- vapply(submitted, function(m) {
    if (!is.list(m)) {
      stop("a submitted model in ", gate_name, " must be an object, not ",
           article_for(class(m)[1]), " ", class(m)[1], ".", call. = FALSE)
    }
    id <- m[["model_id"]]
    if (is.null(id) || length(id) != 1L || !is.character(id)) {
      stop("a submitted model in ", gate_name, " is missing a valid `model_id`.", call. = FALSE)
    }
    id
  }, character(1))

  chosen_id <- gate[[chosen_field]]
  if (is.null(chosen_id)) {
    stop("descriptor's ", gate_name, " has no ", chosen_field, ".", call. = FALSE)
  }
  if (length(chosen_id) != 1L || !is.character(chosen_id)) {
    stop("descriptor's ", chosen_field, " must be a single string, not ",
         article_for(class(chosen_id)[1]), " ", class(chosen_id)[1], ".", call. = FALSE)
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
