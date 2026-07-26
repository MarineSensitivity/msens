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
