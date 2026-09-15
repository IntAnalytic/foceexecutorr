## Submission

This is a new release (first submission to CRAN).

## Test environments

* local: macOS, R 4.5.2 (2025-10-31), platform x86_64-apple-darwin23.6.0
* GitHub Actions (`.github/workflows/R-CMD-check.yaml`): ubuntu-latest,
  macos-latest, windows-latest, all on R release
* win-builder (devel and release): **not yet run** -- run
  `devtools::check_win_devel()` / `check_win_release()` before submitting
* R-hub: **not yet run** -- run `rhub::rhub_check()` before submitting

## R CMD check results

Local `R CMD check --as-cran`: 1 error, 2 warnings, 3 notes. Every one of
these traces to this machine's incomplete LaTeX/qpdf/HTML-tidy toolchain, not
to package content -- `checking Rd contents`, `checking Rd \usage sections`,
and every other content-level check pass cleanly. This is **not** a clean
local result and each item below needs to be re-verified on win-builder or
R-hub, which have complete toolchains, before submission:

* ERROR -- "PDF version of manual without index": `pdflatex is not
  available` on this machine, so the manual PDF could not be built or
  checked here at all.
* WARNING -- "PDF version of manual": LaTeX errors, the direct consequence
  of the missing `pdflatex` above rather than a separate Rd problem.
* WARNING -- `qpdf` is needed for checks on size reduction of PDFs: not
  installed here.
* NOTE -- "CRAN incoming feasibility": `New submission`. Expected and
  unavoidable for a first submission.
* NOTE -- HTML version of manual: the installed HTML Tidy (2006) is too old
  for R CMD check to validate against; not attempted.
* NOTE -- "non-standard things in the check directory": a leftover
  `foceexecutorr-manual.tex`, itself a byproduct of the failed PDF build
  above.

DESCRIPTION uses 'sentinel-poppk', 'NONMEM', and BLQ -- a workflow name, a
piece of estimation software, and a pharmacometrics term (below limit of
quantification) respectively, not typos. The first two are now single-quoted;
a "possibly mis-spelled words" NOTE for these (and/or BLQ) may still appear
on a spell-checked platform and can be disregarded.

## Downstream dependencies

There are no downstream dependencies for this package (first release).
