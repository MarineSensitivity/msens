test_that("widget_png validates its arguments before doing any work", {
  expect_error(widget_png(NULL, ""))
  expect_error(widget_png(NULL, character(0)))
})

test_that("widget_png screenshots a widget to a real PNG", {
  # REGRESSION: interactive widgets serialise their whole payload into the HTML,
  # which is what produced 43-53 MB published pages (one map embedded 26 MB of
  # GeoJSON). A PNG must come out, and it must be far smaller than the widget.
  skip_if_not_installed("webshot2")
  skip_if_not_installed("leaflet")
  skip_on_cran()
  skip_if_not(nzchar(Sys.which("google-chrome")) ||
              nzchar(Sys.which("chromium")) ||
              file.exists("/Applications/Google Chrome.app") ||
              nzchar(Sys.getenv("CHROMOTE_CHROME")),
              "headless Chrome unavailable")

  w   <- leaflet::addTiles(leaflet::leaflet())
  png <- withr::local_tempfile(fileext = ".png")
  out <- widget_png(w, png, width = 400, height = 300, delay = 3, zoom = 1)

  expect_equal(out, png)
  expect_true(file.exists(png))
  expect_gt(file.size(png), 1000)                    # a real image, not a stub
  # PNG magic bytes
  expect_equal(as.integer(readBin(png, "raw", 4)), c(137L, 80L, 78L, 71L))
})

test_that("cog_tile_url renders a flat mask via an explicit colormap", {
  # the stock-titiler replacement for the custom factory's `color=` flat render,
  # which the scores app's "outside Program Areas" overlay is the last user of
  u <- cog_tile_url("https://x/y.tif", color = "#222222")
  expect_match(u, "colormap=", fixed = TRUE)
  expect_match(u, "34%2C34%2C34", fixed = TRUE)   # 0x22 = 34, URL-encoded commas
  expect_false(grepl("colormap_name", u))
  expect_false(grepl("rescale", u))
  # the ordinary ramp path is untouched
  expect_match(cog_tile_url("https://x/y.tif"), "colormap_name=spectral_r", fixed = TRUE)
})

test_that("publish_pmtiles simplifies only the LOW zooms, and drops nothing tiny", {
  # `--simplification` alone applies at every zoom including the max, so the
  # deepest tiles were coarser than the source -- and every higher zoom
  # overzooms from them, so no view anywhere showed the real boundary.
  skip_if_not_installed("sf")
  fake <- withr::local_tempfile(fileext = ".sh")
  writeLines(c("#!/bin/sh", 'echo "$@" > "$TIPPE_ARGS"', "exit 0"), fake)
  Sys.chmod(fake, "0755")
  argf <- withr::local_tempfile()
  withr::local_envvar(TIPPE_ARGS = argf)

  sq <- sf::st_sf(mdl_key = "am|x", ds_key = "am", geometry = sf::st_sfc(
    sf::st_polygon(list(cbind(c(0,1,1,0,0), c(0,0,1,1,0)))), crs = 4326))
  publish_pmtiles(sq, tempfile(fileext = ".pmtiles"), "lyr", tippecanoe = fake)

  a <- readLines(argf)
  expect_match(a, "--simplify-only-low-zooms", fixed = TRUE)
  expect_match(a, "--no-tiny-polygon-reduction", fixed = TRUE)
  expect_match(a, "-z 10", fixed = TRUE)          # was 6: ~2.4 km at its finest
  expect_false(grepl("--drop-densest|--coalesce", a))
})

test_that("publish_pmtiles carries caller-chosen attributes", {
  skip_if_not_installed("sf")
  fake <- withr::local_tempfile(fileext = ".sh")
  writeLines(c("#!/bin/sh", 'echo "$@" > "$TIPPE_ARGS"', "exit 0"), fake); Sys.chmod(fake, "0755")
  argf <- withr::local_tempfile(); withr::local_envvar(TIPPE_ARGS = argf)
  sq <- sf::st_sf(programarea_key = "GAB", geometry = sf::st_sfc(
    sf::st_polygon(list(cbind(c(0,1,1,0,0), c(0,0,1,1,0)))), crs = 4326))
  publish_pmtiles(sq, tempfile(fileext = ".pmtiles"), "zones",
                  keep_attrs = "programarea_key", tippecanoe = fake)
  a <- readLines(argf)
  expect_match(a, "-y programarea_key", fixed = TRUE)
  expect_false(grepl("mdl_key", a))   # zone layers have no mdl_key
})
