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

Local `R CMD check --as-cran`: 0 errors, 0 warnings, 1 note.

```
New submission
```

This note is expected and unavoidable for a first submission.

Locally, the check also reports a PDF-manual error and two notes
(`pdflatex is not available`, `qpdf` missing, HTML Tidy too old) that trace to
missing LaTeX/qpdf/tidy installations on this machine, not to package content
-- `checking Rd contents`, `checking Rd \usage sections`, and the rest of the
Rd-related checks all pass cleanly. These should be re-verified on win-builder
or R-hub, which have complete toolchains, before submission.

## Downstream dependencies

There are no downstream dependencies for this package (first release).
