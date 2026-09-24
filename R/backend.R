# The pluggable backend seam (ADR-0029).
#
# Registration rather than a hard-coded switch, because the two named backends --
# an approximate FOCE estimator and NONMEM -- cannot both be present on any one
# machine: NONMEM needs a licence, the estimator is a separate build. A registry
# lets the package be installed, tested and demonstrated with neither.

.registry <- new.env(parent = emptyenv())

# Shared by register_backend() and resolve_backend(). A backend name is a
# short identifier, not free text, so no character in it should ever be
# whitespace -- rejecting outright rather than trimming avoids two look-alike
# entries (" inspect" vs "inspect") coexisting as distinct registry keys, and
# `\p{Z}` (Unicode "separator, space") catches exotic whitespace ASCII
# trimws() doesn't, such as a name that is only a non-breaking space, which
# `nzchar(trimws(name))` alone treated as non-empty. The length cap keeps a
# too-long name from ever reaching exists(), which errors with R's own
# "variable names are limited to 10000 bytes" instead of a package message.
#
# validUTF8() runs first: perl = TRUE regex on invalid UTF-8 bytes warns
# ("input string 1 is invalid UTF-8") and its match result can't be trusted,
# which would otherwise let a mojibake name slip past the whitespace check
# entirely and register under whatever garbled bytes it decodes to.
is_nonempty_name <- function(name) {
  is.character(name) && length(name) == 1L && !is.na(name) &&
    nzchar(name) && validUTF8(name) &&
    !grepl("[\\p{Z}\\s]", name, perl = TRUE) &&
    nchar(name, type = "bytes") <= 200L
}

#' Register an estimation backend
#'
#' @param name Short identifier, e.g. `"nonmem"`.
#' @param run A function of `(descriptor, model, data_path, ...)` returning a list.
#'   It is called with the resolved descriptor and the model [chosen_model()]
#'   picked FROM THAT SAME descriptor, so a backend never re-implements
#'   descriptor parsing -- but also never re-derives which one `model` came
#'   from. [execute()] always calls `run()` with a matched pair; calling a
#'   backend's `run()` directly (e.g. via [resolve_backend()]) with a `model`
#'   from a *different* descriptor than the `descriptor` argument is the
#'   caller's own mismatch to avoid -- `descriptor[["dataset_path"]]` would
#'   already be wrong for that `model` on this basis alone, the same as
#'   [resolved_gate(descriptor)][resolved_gate] would be. Integer
#'   fields on `model` (e.g. `seed`) are ordinarily numeric, but arrive as a
#'   character string instead if the descriptor's value exceeds 2^53 -- an R
#'   double can't represent an integer beyond that exactly, so
#'   [read_descriptor()] preserves the digits as a string rather than
#'   silently rounding it. Check `is.character()` before doing arithmetic on
#'   such a field if that's a realistic possibility for your backend. `model`
#'   is exactly the entry [chosen_model()] found in the descriptor's own
#'   submitted tuple -- nothing added, nothing removed -- so it is safe to
#'   compare, hash, or forward to an external engine unchanged. Whether an
#'   empty `covariates` on `model` means "not decided yet" or "confirmed to
#'   have none" depends on which gate it came from -- call
#'   [resolved_gate(descriptor)][resolved_gate] rather than re-implementing
#'   that check.
#' @param official `TRUE` only if results from this backend may be treated as an
#'   official, reportable answer. **Defaults to `FALSE`, and that default is the
#'   point**: a preview engine and a qualified one both return plausible
#'   parameter estimates, and nothing downstream can tell them apart unless the
#'   backend says which it is. Anything unstated is unofficial.
#' @param description One line, shown by [backends()].
#' @param overwrite Must be `TRUE` to replace an already-registered `name`.
#'   **Defaults to `FALSE` and errors on collision** -- silently replacing an
#'   existing backend is exactly how a trusted `official = FALSE` registration
#'   (the built-in `"inspect"` backend, for instance) would get quietly swapped
#'   for one claiming `official = TRUE`, with every default `execute()` call
#'   reporting official results afterwards. Replacing a backend on purpose is
#'   legitimate; doing it by name collision is not, and this makes the two
#'   distinguishable.
#' @return Invisibly, the registered name.
#' @examples
#' # overwrite = TRUE makes this example safe to run more than once in the
#' # same session -- without it, registering "demo" a second time would hit
#' # the overwrite guard documented above.
#' register_backend("demo", function(descriptor, model, data_path, ...) list(ok = TRUE),
#'                  official = FALSE, description = "example", overwrite = TRUE)
#' "demo" %in% backends()$name
#' @export
register_backend <- function(name, run, official = FALSE, description = "", overwrite = FALSE) {
  if (!is_nonempty_name(name)) {
    stop("`name` must be a short identifier: 1-200 bytes, no whitespace.", call. = FALSE)
  }
  if (!is.function(run)) {
    stop("`run` must be a function.", call. = FALSE)
  }
  if (!is.logical(official) || length(official) != 1L || is.na(official)) {
    stop("`official` must be TRUE or FALSE -- an unstated provenance is exactly ",
         "what this flag exists to prevent.", call. = FALSE)
  }
  if (!is.character(description) || length(description) != 1L || is.na(description)) {
    stop("`description` must be a single string.", call. = FALSE)
  }
  if (exists(name, envir = .registry, inherits = FALSE) && !isTRUE(overwrite)) {
    existing <- get(name, envir = .registry, inherits = FALSE)
    stop("a backend named '", name, "' is already registered (official = ",
         existing$official, "). Pass `overwrite = TRUE` to replace it ",
         "deliberately -- silently replacing a registered backend is exactly ",
         "the failure this guard exists to prevent.", call. = FALSE)
  }
  assign(name, list(name = name, run = run, official = official,
                    description = description), envir = .registry)
  invisible(name)
}

#' List registered backends
#'
#' @return A data frame with one row per backend: `name`, `official`, `description`.
#' @examples
#' backends()
#' @export
backends <- function() {
  names_ <- sort(ls(.registry, all.names = TRUE))
  if (length(names_) == 0L) {
    return(data.frame(name = character(), official = logical(),
                      description = character(), stringsAsFactors = FALSE))
  }
  entries <- lapply(names_, get, envir = .registry)
  data.frame(
    name = vapply(entries, `[[`, character(1), "name"),
    official = vapply(entries, `[[`, logical(1), "official"),
    description = vapply(entries, `[[`, character(1), "description"),
    stringsAsFactors = FALSE
  )
}

#' Resolve a backend by name
#'
#' @param name Backend identifier.
#' @return The registered backend, as a list.
#' @examples
#' resolve_backend("inspect")$official
#' @export
resolve_backend <- function(name) {
  if (!is_nonempty_name(name)) {
    stop("`name` must be a short identifier: 1-200 bytes, no whitespace.", call. = FALSE)
  }
  if (!exists(name, envir = .registry, inherits = FALSE)) {
    stop("no backend named '", name, "'. Registered: ",
         paste(sort(ls(.registry, all.names = TRUE)), collapse = ", "),
         ". Register one with register_backend().", call. = FALSE)
  }
  get(name, envir = .registry)
}

# The one backend that ships. It runs no estimation: it reports what would be
# executed. That makes the package installable, testable and demonstrable with
# no licence and no estimator, and it is the honest shape for a scaffold -- a
# stub that returned invented parameter estimates would be indistinguishable
# from a real fit to anything downstream.
.register_builtin_backends <- function() {
  register_backend(
    "inspect",
    run = function(descriptor, model, data_path, ...) {
      # `[[`, not `$`: a submitted model missing e.g. `seed` but carrying a
      # similarly-named field (`seed_source`) would otherwise have that
      # field's value silently reported as the seed in this summary.
      #
      # covariates: prefer the model's own, confirmed `covariates`
      # (`length()`, not `is.list()`, so a caller-assigned plain vector isn't
      # mistaken for absent -- read_descriptor() itself always produces a
      # list, but nothing stops direct assignment of an atomic one). Fall
      # back to the candidate `declared_covariates` only when `model` came
      # from structural_selection: there, an empty `covariates` means "not
      # decided yet". For a covariate_search model an empty `covariates`
      # means "confirmed to have none" (e.g. cov-step0) -- falling back there
      # would misreport untested candidates as if they were kept.
      #
      # Which gate `model` came from is asked via resolved_gate(descriptor) --
      # the same question chosen_model() itself answers to decide which gate
      # to resolve, shared rather than re-implemented here so the two can't
      # drift out of sync -- rather than read from anything attached to
      # `model`. `model` is deliberately left exactly as chosen_model() found
      # it in the signed record: safe to compare, hash, or forward to a real
      # backend unchanged. resolved_gate() accepts any list, not just a
      # classed `focex_descriptor` -- this backend's own `descriptor`
      # argument only needs `[["covariate_search"]]` to be reachable, and
      # requiring more would make this branch fail or not depending on
      # unrelated data (whether `own_covariates` happened to be empty).
      #
      # NOTE: when the fallback fires, `covariates` is bare candidate names
      # (e.g. "age"); once covariate_search has resolved a model, it's
      # "param:covariate" relationship strings (e.g. "age:CL") instead. Not
      # one consistent shape, and a caller can't tell which from the field
      # alone -- a known, accepted rough edge in this backend's human-facing
      # summary, not (yet) a stable machine-readable contract.
      own_covariates <- model[["covariates"]]
      covariates <- if (length(own_covariates) > 0L) {
        own_covariates
      } else if (resolved_gate(descriptor) == "structural_selection") {
        model[["declared_covariates"]]
      } else {
        NULL
      }
      list(
        executed = FALSE,
        reason = "the 'inspect' backend runs no estimation",
        would_run = list(
          model_id = model[["model_id"]],
          compartments = model[["compartments"]],
          error_model = model[["error_model"]],
          # `length() == 0L`, not `is.null()`: a v1 model has no absorption
          # field at all (genuinely absent), but a v2 one could have it
          # assigned an empty vector rather than NULL -- both mean "nothing
          # to report", and is.null() alone would only catch the first.
          absorption = if (length(model[["absorption"]]) == 0L) NULL else model[["absorption"]],
          covariates = unlist(covariates) %||% character(),
          estimation_method = model[["estimation_method"]],
          seed = model[["seed"]],
          dataset = descriptor[["dataset_path"]],
          data_path = data_path
        )
      )
    },
    official = FALSE,
    description = "Reports what would be executed; runs nothing."
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

.onLoad <- function(libname, pkgname) {
  .register_builtin_backends()
}
