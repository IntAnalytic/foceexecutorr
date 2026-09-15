# foceexecutorr 0.1.0

Initial scaffold. Nothing here executes a real estimation yet; what exists is the
contract the rest will be built against.

* `read_descriptor()` reads and validates a signed `model.json` descriptor
  (schema version 1), the artifact `sentinel-poppk` writes at report sign-off.
* `chosen_model()` resolves the model a descriptor actually selected, from the
  full submitted tuple it carries.
* A pluggable backend registry — `register_backend()`, `backends()`,
  `resolve_backend()` — so estimation engines are registered rather than
  hard-coded (ADR-0029).
* `execute()` ties the two together and is the package's single entry point.
* A built-in `"inspect"` backend that runs no estimation and reports what *would*
  be executed. It exists so the package is testable and demonstrable with no
  NONMEM licence and no estimator present.
* `register_backend()` now refuses to replace an already-registered name unless
  `overwrite = TRUE` is passed explicitly. Without this, re-registering
  `"inspect"` under its own name with `official = TRUE` silently overwrote the
  trusted built-in and every default `execute()` call reported official
  results afterwards.
* `execute()` now calls a backend's `run()` with named arguments and refuses
  `descriptor`, `model`, or `data_path` passed through its own `...`. It
  previously called `run()` positionally, so a caller could pass `model =`
  through `...` and have it bind ahead of the descriptor's actual chosen
  model, while the result still reported the original model's id.
* `register_backend()` now validates `description` (previously a `NULL` or
  non-scalar value registered successfully and broke `backends()` for every
  caller) and rejects `NA` as a `name` (previously accepted, registering a
  backend under the literal name `"NA"`); both `register_backend()` and
  `resolve_backend()` now also reject an empty or whitespace-only `name`.
* `backends()` and `resolve_backend()`'s "no backend named" error now list
  dot-prefixed backend names too; they previously registered and ran
  correctly but never appeared in either.
* `read_descriptor()` now rejects a directory or an empty file before handing
  them to the JSON parser, treats a required field that is present but
  explicitly `null` the same as a missing one, and strips every `.source` key
  the JSON itself carries (not just the first, for a duplicated key) before
  attaching the real source path -- previously an untrusted `.source` field
  could shadow the real one.
* `read_descriptor()`'s schema-version check now compares by numeric value
  rather than exact type, so `1`, `1.0`, and `"1"` are all accepted as schema
  version 1; a JSON boolean or a non-digit string (`"0x1"`, `" 1 "`, `"1e0"`)
  is still refused, and a scalar top-level JSON document (e.g. a bare string
  or number, previously "subscript out of bounds") is refused by name like
  any other descriptor missing every required field.
* `chosen_model()` no longer uses `$` to read `structural_selection`'s fields,
  which could partial-match a similarly-named field (e.g. a stray
  `chosen_model_id_prev`) instead of the one the descriptor actually names.
  It also gives a distinct, descriptor-level error for a chosen id that's
  duplicated, missing, or not a plain string; a submitted model missing or
  malformed `model_id`; and a `structural_selection` or submitted-model entry
  that isn't an object at all (previously all either silently misbehaved or
  died inside `vapply()`/`[[` with a low-level "subscript out of bounds").
* `print.focex_descriptor()` no longer uses `$` to read `sign_off`, which
  could partial-match a similarly-named field (e.g. a draft `sign_off_draft`)
  and print its contents as if the descriptor were actually signed.
* `read_descriptor()` now parses the file with `jsonlite::read_json()`
  instead of `fromJSON()`, and reads integers with `bigint_as_char = TRUE`.
  Previously: (a) a file whose bare filename happened to itself be valid
  JSON (e.g. one literally named `2026`) was parsed as that literal value
  instead of ever being opened; (b) an integer field (e.g. `seed`) past
  2^53 silently rounded, since an R double can't represent it exactly past
  that point. **Backend authors: a field like `model$seed` may now arrive
  as a character string rather than a number**, if and only if its value
  in the descriptor exceeds 2^53 -- check with `is.character()` before
  doing arithmetic on it if that's a realistic possibility for your
  backend. (Values beyond the signed 64-bit range, roughly 9.2
  quintillion, still round silently; not a realistic concern for a seed.)
* The built-in `"inspect"` backend now reads submitted-model fields
  (`model_id`, `compartments`, `error_model`, `covariates`,
  `estimation_method`, `seed`) with `[[` instead of `$`, closing the same
  partial-match class of bug fixed elsewhere: a model missing `seed` but
  carrying a similarly-named `seed_source` no longer has that value
  reported as the seed.
* `register_backend()`/`resolve_backend()` reject a `name` containing any
  whitespace anywhere (leading, trailing, or internal -- including exotic
  Unicode whitespace such as a lone non-breaking space, previously
  accepted) or invalid UTF-8, and cap it at 200 bytes. A name like
  `" inspect"` previously registered as a distinct, visually near-identical
  entry alongside the real `"inspect"`, and a name over 10000 bytes crashed
  with R's own internal "variable names are limited to 10000 bytes" instead
  of a package message. The error for an invalid name now says what the
  rule actually is (`must be a short identifier: 1-200 bytes, no
  whitespace`) rather than just `must be a non-empty string`.
* `execute()` now validates `backend` and `descriptor` itself, so a mistake
  there is reported using `execute()`'s own parameter names instead of
  `resolve_backend()`'s/`read_descriptor()`'s (`name`/`path`).
