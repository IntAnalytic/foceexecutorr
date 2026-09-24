fixture <- function() system.file("extdata", "model.json", package = "foceexecutorr")
fixture_v2 <- function() {
  system.file("extdata", "model-run-fbd67a28-06a8-40b4-9cb1-e14e6cc49ff3.json",
              package = "foceexecutorr")
}

# Simulates a caller reading a descriptor and saving it back out the ordinary
# way (as multiple tests need to, to exercise read_descriptor()'s handling of
# jsonlite's `{}`-for-NULL round trip). `digits = NA` -- jsonlite's
# high-precision mode (~15 significant digits), not its lossy default of 4
# DECIMAL PLACES (not significant figures: a small value like 0.000012345
# comes back as `0` outright, not just rounded) -- is deliberate: without it,
# this helper would itself silently truncate a signed numeric estimate (e.g.
# 4.057944 becomes 4.0579), which is exactly the kind of corruption this
# package cares about NOT introducing.
round_trip <- function(d) {
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(unclass(d), tmp, auto_unbox = TRUE, digits = NA)
  read_descriptor(tmp)
}
