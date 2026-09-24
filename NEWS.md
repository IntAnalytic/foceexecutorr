# foceexecutorr 0.1.0

Initial scaffold. Nothing here executes a real estimation yet; what exists is the
contract the rest will be built against.

* `read_descriptor()` reads and validates a signed descriptor, the artifact
  `sentinel-poppk` writes at report sign-off.
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
* `read_descriptor()` now accepts schema version 2 as well as 1.
  `SUPPORTED_SCHEMA_VERSION` is an integer vector of every version this
  package can read; check membership (`schema_version %in%
  SUPPORTED_SCHEMA_VERSION`), not equality against a single value -- a
  scalar would make that check silently reject a valid but non-default
  version.
* `chosen_model()` now resolves through `covariate_search` when a run went on
  to test covariate relationships, not just `structural_selection` -- the
  bare structural candidate a run started from (e.g. a plain 2-compartment
  model) is not the model its `sign_off` actually describes as reported and
  qualified once covariate testing has refined it further. A
  `covariate_search` that is present but incomplete (a submitted ladder with
  no `final_model_id` recorded, say) is a genuinely inconsistent signed
  record -- `sign_off` cannot meaningfully describe a "final model" that gate
  never named -- so this errors rather than silently falling back to
  `structural_selection` and reporting a materially different, less-refined
  model in its place. Every error from `chosen_model()` -- a chosen id that's
  duplicated, not found, or not a plain string; a submitted model missing or
  malformed `model_id` -- now names which gate it came from, since a
  descriptor can have two. `chosen_model()`'s return value is unchanged from
  the signed record's own submitted entry -- safe to compare, hash, or
  forward to a real backend as-is. A new exported `resolved_gate(descriptor)`
  answers which gate `chosen_model()` would resolve (`"structural_selection"`
  or `"covariate_search"`), for a custom backend that needs to know whether
  an empty `covariates` on the model it was handed means "not decided yet" or
  "confirmed to have none" -- see [register_backend()]. It accepts any list
  shaped like a descriptor, not only a classed `focex_descriptor`, since a
  backend called directly (bypassing `execute()`) may be handed an unclassed
  one.
* The built-in `"inspect"` backend reads a submitted model's
  `declared_covariates` field when its own `covariates` is empty AND the
  descriptor's `covariate_search` is `NULL` (the same check `chosen_model()`
  itself uses) -- there, an empty `covariates` means "not decided yet"
  (schema v2's shape before covariate testing has run). A
  `covariate_search`-resolved model's empty `covariates` means "confirmed to
  have none" instead (e.g. the base, no-covariates step of a ladder), and is
  never subject to the fallback, so it can't be misreported as carrying
  untested candidates. The backend also reports `absorption`;
  `print.focex_descriptor()` includes the absorption route too when present,
  and now reports when a model could not be resolved at all instead of
  silently omitting the line. `would_run$covariates` does not have one
  consistent shape across the fallback and non-fallback cases -- bare
  candidate names (e.g. "age") vs. `param:covariate` relationship strings
  (e.g. "age:CL") -- a known rough edge in the `"inspect"` backend's
  human-facing summary, not (yet) a stable machine-readable contract.
* `jsonlite::write_json()`'s default `digits` (4 *decimal places*, not
  significant figures) silently truncates a floating-point estimate --
  4.057944 comes back as 4.0579, and a small value like 0.000012345 comes
  back as `0` outright, not just rounded. This is not something
  `read_descriptor()` can fix after the fact (it only reads), but it matters
  here specifically because this package cares about exact fidelity to a
  signed numeric record (see the `bigint_as_char` handling below); anyone
  re-serialising a descriptor with `jsonlite::write_json()` -- to cache, log,
  or forward it -- needs `digits = NA` (jsonlite's high-precision mode, ~15
  significant digits -- ample for a fitted PK estimate, though not a
  bit-for-bit guarantee for an arbitrary double) to avoid silently
  corrupting it. The package's own tests that round-trip a descriptor now do
  this via a shared `round_trip()` test
  helper, not ad hoc, so a lossy default can't creep back in unnoticed.
* `read_descriptor()` normalizes jsonlite's `{}` (an empty *named* list --
  what its JSON writer produces for a round-tripped R `NULL`, distinct from a
  genuine empty array `[]`) back to `NULL`, recursively, at every depth. This
  matters for any descriptor that has been read and saved back out with
  `jsonlite::write_json()` (without `null = "null"`) and read again: every
  field that was originally `null` -- `covariate_search`,
  `declared_covariates`, `absorption`, `seed`, or any other optional field --
  would otherwise come back as an empty list rather than `NULL`, which broke
  `chosen_model()`/`execute()` outright for a resaved `covariate_search:
  null` and printed a dangling `, )` for a resaved `absorption: null`.
* Documentation no longer implies the descriptor sentinel-poppk writes is
  literally named `model.json` -- it is named after the run (e.g.
  `model-run-<run-id>.json`). `read_descriptor()` accepted any path all
  along; only the docs, `DESCRIPTION`, and the README example were wrong.
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
