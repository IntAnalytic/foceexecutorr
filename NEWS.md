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
