# foceexecutorR

Execute a signed population-PK model descriptor against a pluggable estimation backend.

`sentinel-poppk` emits a `model.json` at report sign-off: a self-contained, signed
description of the analysis — the full model tuple as submitted, the one that was
chosen, its fitted parameters, the accepted data-handling, and who signed it. This
package takes that file and runs the model it specifies.

It owns **execution only**. Assembling data, imputation and BLQ handling, and
organising outputs belong to the orchestrator, which is a separate package
([ADR-0029](https://github.com/IntAnalytic/sentinel-poppk/blob/main/docs/decisions/0029-second-lane-two-r-packages-executor-beside-orchestrator.md)).

## Status

**Scaffold.** The descriptor contract, the backend seam and the entry point are
real and tested. No estimation backend ships yet — the two intended ones are an
approximate FOCE estimator and NONMEM, and neither can be assumed present on any
given machine.

## Usage

```r
library(foceexecutorR)

d <- read_descriptor("model.json")
d
#> <focex_descriptor>
#>   run:      demo-2cmt-wt-age-signed
#>   dataset:  poppk_phase1_demo
#>   model:    2cmt (2-compartment, combined error)
#>   signed by: Safi Ahmed at 2026-09-11T10:05:00+00:00

execute(d, backend = "inspect")
#> <focex_result>
#>   run:     demo-2cmt-wt-age-signed
#>   model:   2cmt
#>   backend: inspect
#>   official: NO — not a reportable result
```

## Registering a backend

```r
register_backend(
  "nonmem",
  run = function(descriptor, model, data_path, ...) { ... },
  official = TRUE,
  description = "NONMEM 7.5 via PsN"
)
```

`official` has no default you can drift into — it defaults to `FALSE`, and
`execute()` reads it from the registration rather than from whatever the backend
returns. A preview estimator and a qualified one both produce plausible parameter
estimates; nothing downstream can tell them apart unless the executor says which
it was. That is the one thing this package refuses to leave implicit.

## Licence

Apache 2.0.
