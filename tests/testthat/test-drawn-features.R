# drawn_features_sf() — the mapgl draw-control → sf contract used by the
# scores app's Report tab ("Add drawn polygon").

# a two-feature FeatureCollection shaped exactly like MapboxDraw's getAll()
fc_json <- paste0(
  '{"type":"FeatureCollection","features":[',
  '{"id":"a1","type":"Feature","properties":{},',
  '"geometry":{"type":"Polygon","coordinates":',
  '[[[-121,34],[-120,34],[-120,35],[-121,35],[-121,34]]]}},',
  '{"id":"b2","type":"Feature","properties":{},',
  '"geometry":{"type":"Polygon","coordinates":',
  '[[[-119,33],[-118,33],[-118,34],[-119,34],[-119,33]]]}}]}')

# what Shiny hands the server when the JS sends the OBJECT (not a string):
# jsonlite parses it into a nested list with no simplification
fc_list <- jsonlite::fromJSON(fc_json, simplifyVector = FALSE)

test_that("drawn_features_sf parses the stringified FeatureCollection", {
  d <- drawn_features_sf(fc_json)

  expect_s3_class(d, "sf")
  expect_equal(nrow(d), 2)
  expect_equal(sf::st_crs(d)$epsg, 4326)
  expect_true(all(sf::st_geometry_type(d) == "POLYGON"))
  expect_equal(
    as.numeric(sf::st_bbox(d)),
    c(-121, 33, -118, 35))
})

test_that("drawn_features_sf parses the parsed-list FeatureCollection", {
  # REGRESSION: mapgl's `_mapglSyncDrawnFeatures` sends `draw.getAll()` as an
  # OBJECT, so `input$<map>_drawn_features` arrives as a list, not a string.
  # The scores + scores_v8 Report tabs gated on `is.character()` and therefore
  # reported "Draw a polygon on the map first." for every polygon the user drew.
  # Both payload shapes must yield the identical sf.
  d_list <- drawn_features_sf(fc_list)
  d_json <- drawn_features_sf(fc_json)

  expect_s3_class(d_list, "sf")
  expect_equal(nrow(d_list), 2)
  expect_equal(sf::st_crs(d_list)$epsg, 4326)
  expect_equal(
    sf::st_as_text(sf::st_geometry(d_list)),
    sf::st_as_text(sf::st_geometry(d_json)))
})

test_that("drawn_features_sf keeps feature order so the last row is the newest", {
  # the app takes `d[nrow(d), ]` as "the polygon just drawn"
  for (x in list(fc_json, fc_list)) {
    d <- drawn_features_sf(x)
    expect_equal(
      as.numeric(sf::st_bbox(d[nrow(d), ])),
      c(-119, 33, -118, 34))
  }
})

test_that("drawn_features_sf returns NULL for nothing-drawn payloads", {
  empty_json <- '{"type":"FeatureCollection","features":[]}'

  expect_null(drawn_features_sf(NULL))
  expect_null(drawn_features_sf(""))
  expect_null(drawn_features_sf(empty_json))
  expect_null(drawn_features_sf(
    jsonlite::fromJSON(empty_json, simplifyVector = FALSE)))
  expect_null(drawn_features_sf("not geojson at all"))
})
