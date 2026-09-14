# foceexecutorr 0.0.0.9000

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
