## Submission

This is a new release (first submission to CRAN).

## Test environments

* local: macOS, R 4.5.2 (2025-10-31), platform x86_64-apple-darwin23.6.0
* GitHub Actions (`.github/workflows/R-CMD-check.yaml`): ubuntu-latest,
  macos-latest, windows-latest on R release; ubuntu-latest on R-devel and
  R 4.1 (the `Depends` floor)
* win-builder (devel and release): submitted 2026-09-15; results pending
  by e-mail to the Maintainer address
* R-hub (linux, windows, macos; R-devel), via the
  `.github/workflows/rhub.yaml` GitHub Actions workflow: clean on all
  three -- `Status: OK`, no errors/warnings/notes
  (https://github.com/IntAnalytic/foceexecutorr/actions/runs/34979143754)

## R CMD check results

Local `R CMD check --as-cran --no-manual`: 1 warning, 2 notes.

* NOTE -- "CRAN incoming feasibility": `New submission`. Expected and
  unavoidable for a first submission.
* NOTE -- "checking for future file timestamps": `unable to verify current
  time`. This machine could not reach the time-verification service; not a
  package issue.
* WARNING -- `qpdf` is needed for checks on size reduction of PDFs: not
  installed on this machine.

`--no-manual` is used locally because this machine has no working
`pdflatex`, so the PDF manual has **not** been built or checked here at
all -- that is a real gap in local coverage, not a clean result to report
on its own. The full `R CMD check --as-cran` (manual included) needs to be
run on win-builder or R-hub, which have complete toolchains, before
submission.

DESCRIPTION uses 'sentinel-poppk', 'NONMEM', and BLQ -- a workflow name, a
piece of estimation software, and a pharmacometrics term (below limit of
quantification, now spelled out on first use), not typos. A "possibly
mis-spelled words" NOTE for these may still appear on a spell-checked
platform and can be disregarded.

## Downstream dependencies

There are no downstream dependencies for this package (first release).
