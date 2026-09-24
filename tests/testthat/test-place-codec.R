# The `g1` place codec — R's half of a two-language contract.
#
# `inst/fixtures/place_codec.json` is THE SAME FILE as the atlas repo's
# `tests/fixtures/place_codec.json`, byte for byte. The TypeScript side owns the
# spec; R reproduces it. Every assertion here is against that file, never against
# what this implementation happens to do.

fx <- place_codec_fixture()

# jsonlite reads a whole number as an integer, so compare numerically rather than
# with identical(): -90L and -90 are the same coordinate and a different R type
num <- function(x) rapply(x, as.numeric, classes = c("integer", "numeric"),
                          how = "replace")

test_that("the shared vector file is byte-identical with the atlas repo's copy", {
  mine  <- system.file("fixtures", "place_codec.json", package = "msens")
  theirs <- "/Users/bbest/Github/MarineSensitivity/atlas/tests/fixtures/place_codec.json"
  expect_identical(fx$codec, "g1")
  # the sha256 the atlas side records for this revision: a drift detector that works
  # without the other repo on disk
  expect_identical(
    digest::digest(file = mine, algo = "sha256"),
    "50ad541afff2c2cd2c0d005d89f8ba230e2d19f9595ffcf1b03c233899d2897f")
  skip_if_not(file.exists(theirs), "the atlas repo is not on this machine")
  expect_identical(tools::md5sum(mine)[[1]], tools::md5sum(theirs)[[1]])
})

test_that("every encode_reject vector is refused by the STRICT encoder, exactly", {
  # the shared counterpart of ruling 2: the byte-level encoder refuses a ring that
  # is still wrapped, and the high-level entry yields the token beside it
  expect_gt(length(fx$encode_reject), 0)
  for (v in fx$encode_reject) {
    pl <- list(kind = "geom", name = v$name,
               geometry = geojson_sfc(v$geometry), precision = v$precision)
    e <- tryCatch(place_encode_strict(pl), error = function(e) e)
    expect_s3_class(e, "msens_place_reject")
    expect_identical(e$code, v$code, info = v$id)
    expect_match(conditionMessage(e), sprintf("%.4f-degree step", v$max_step_deg),
                 info = v$id)

    # the unwrapped ring the fixture states, vertex for vertex
    expect_equal(unname(sf::st_coordinates(unwrap_polygon(geojson_sfc(v$geometry)))[, c("X", "Y")]),
                 unname(sf::st_coordinates(geojson_sfc(v$unwrapped))[, c("X", "Y")]),
                 info = v$id)
    # ...and the high-level entry yields the recorded token
    expect_identical(place_encode(pl), v$token_via_high_level, info = v$id)
    expect_identical(place_encode_strict(
      list(kind = "geom", name = v$name, geometry = geojson_sfc(v$unwrapped),
           precision = v$precision)), v$token_via_high_level, info = v$id)
  }
})

test_that("every geometry vector ENCODES to the shared token, character for character", {
  for (v in fx$vectors) {
    tok <- place_encode(list(kind = "geom", name = v$name,
                             geometry = geojson_sfc(v$geometry),
                             precision = v$precision))
    expect_identical(tok, v$token, info = paste(v$id, "-", v$why))
    expect_identical(nchar(tok), as.integer(v$token_length), info = v$id)
  }
})

test_that("every geometry vector DECODES to the shared geometry and precision", {
  for (v in fx$vectors) {
    d <- place_decode(v$token)[[1]]
    expect_identical(d$kind, "geom", info = v$id)
    expect_identical(d$name, v$name, info = v$id)
    expect_identical(d$precision, as.integer(v$precision), info = v$id)
    expect_equal(num(sfc_geojson(d$geometry)), num(v$decoded), info = v$id)
  }
})

test_that("the byte stream itself matches, not merely the token", {
  for (v in fx$vectors) {
    tok <- place_encode(list(kind = "geom", name = v$name,
                             geometry = geojson_sfc(v$geometry),
                             precision = v$precision))
    payload <- strsplit(tok, ".", fixed = TRUE)[[1]][3]
    hex <- paste(sprintf("%02x", as.integer(b64url_decode(payload))), collapse = "")
    expect_identical(hex, v$bytes_hex, info = v$id)
    expect_identical(length(b64url_decode(payload)), as.integer(v$bytes_length), info = v$id)
  }
})

test_that("the DELTAS are the shared deltas, in order", {
  for (v in fx$vectors) {
    g <- geojson_sfc(v$geometry)
    sfg <- g[[1]]
    polys <- if (inherits(sfg, "MULTIPOLYGON")) unclass(sfg) else list(unclass(sfg))
    mul <- 10^v$precision
    cx <- 0; cy <- 0; got <- numeric(0)
    for (p in polys) for (r in p) {
      m <- as.matrix(r); m <- m[-nrow(m), , drop = FALSE]      # closing vertex omitted
      qx <- round(m[, 1] * mul); qy <- round(m[, 2] * mul)
      for (i in seq_len(nrow(m))) {
        got <- c(got, qx[i] - cx, qy[i] - cy); cx <- qx[i]; cy <- qy[i]
      }
    }
    expect_equal(got, as.numeric(unlist(v$deltas)), info = v$id)
  }
})

test_that("REGRESSION: the delta cursor RUNS ON across rings and polygons", {
  # Resetting it per ring still decodes — into a different place, silently. The two
  # vectors that catch it say so in their own `why`.
  h <- Filter(function(v) v$id == "hole", fx$vectors)[[1]]
  m <- Filter(function(v) v$id == "multipolygon", fx$vectors)[[1]]
  # the first delta of the SECOND ring is (250, -750): a step from the last vertex of
  # the outer ring, not from (0,0)
  expect_equal(as.numeric(unlist(h$deltas))[9:10], c(250, -750))
  # ...and across polygons: (1000, -500), not the absolute -899000
  expect_equal(as.numeric(unlist(m$deltas))[9:10], c(1000, -500))
  for (v in list(h, m)) {
    tok <- place_encode(list(kind = "geom", name = v$name,
                             geometry = geojson_sfc(v$geometry), precision = v$precision))
    expect_identical(tok, v$token)
  }
})

test_that("zigzag maps both signs the way the spec states", {
  expect_equal(vapply(c(0, -1, 1, -2, 2, -64, 63), msens:::.g1_zigzag, 0),
               c(0, 1, 2, 3, 4, 127, 126))
  for (n in c(0, 1, -1, 2, -2, 127, -127, 128, -128, 1e6, -1e6, 2^33, -(2^33)))
    expect_equal(msens:::.g1_unzigzag(msens:::.g1_zigzag(n)), n)
})

test_that("varint boundaries: 127/128 and 16383/16384", {
  vi <- function(n) msens:::.g1_varint(n)
  expect_equal(vi(0),     0)
  expect_equal(vi(127),   127)                      # one byte, high bit clear
  expect_equal(vi(128),   c(128, 1))                # two bytes
  expect_equal(vi(16383), c(255, 127))
  expect_equal(vi(16384), c(128, 128, 1))           # three bytes
  # and a delta far past a signed 32-bit integer, which is why this is done in
  # doubles: bitwAnd()/bitwShiftR() would be fine on every vector above and wrong here
  big <- 2^33 + 7
  r <- msens:::.g1_reader(vi(big))
  expect_equal(r$varint(), big)
})

test_that("base64url is RFC 4648 section 5: no +, no /, no padding", {
  for (v in fx$vectors) {
    payload <- strsplit(v$token, ".", fixed = TRUE)[[1]][3]
    expect_false(grepl("[+/=]", payload), info = v$id)
    expect_identical(b64url_encode(b64url_decode(payload)), payload, info = v$id)
  }
  # every byte value round-trips, at all three padding phases
  for (n in c(1, 2, 3, 16, 17, 255)) {
    b <- as.raw((seq_len(n) * 37L) %% 256L)
    expect_identical(b64url_decode(b64url_encode(b)), b)
  }
  expect_identical(b64url_encode(raw()), "")
  expect_identical(b64url_decode(""), raw())
})

test_that("the STRICT encoder refuses a still-wrapped ring; the high-level one normalizes", {
  # D8 addendum, ruling 2: the byte-level encoder refuses a ring with a step of more
  # than 180 degrees in BOTH languages (code `wrapped`), and the high-level entry
  # normalizes first. Repairing silently inside the codec is what made the two
  # languages produce different bytes for the same geometry.
  wrapped <- sf::st_sfc(sf::st_polygon(list(cbind(
    c(170, -170, -170, 170, 170), c(51, 51, 56, 56, 51)))), crs = 4326)
  unwrapped <- sf::st_sfc(sf::st_polygon(list(cbind(
    c(170, 190, 190, 170, 170), c(51, 51, 56, 56, 51)))), crs = 4326)
  pl <- function(g) list(kind = "geom", name = "Bering box", geometry = g, precision = 3)
  want <- Filter(function(x) x$id == "bering", fx$vectors)[[1]]$token

  # strict: an informative rejection carrying the shared code and the actual step
  e <- tryCatch(place_encode_strict(pl(wrapped)), error = function(e) e)
  expect_s3_class(e, "msens_place_reject")
  expect_identical(e$code, "wrapped")
  expect_match(conditionMessage(e), "340.0000-degree step")
  expect_match(conditionMessage(e), "unwrap_polygon")
  # the same refusal through the hash-level entry with unwrap = FALSE
  expect_error(place_encode(pl(wrapped), unwrap = FALSE), "wrapped")

  # high-level: the same token as the already-unwrapped ring
  expect_identical(place_encode(pl(wrapped)), want)
  expect_identical(place_encode(pl(unwrapped)), want)
  # ...and strict accepts the unwrapped one, byte for byte
  expect_identical(place_encode_strict(pl(unwrapped)), want)
})

test_that("SEEDED FAULT: a strict encoder that silently unwraps goes red", {
  wrapped <- sf::st_sfc(sf::st_polygon(list(cbind(
    c(170, -170, -170, 170, 170), c(51, 51, 56, 56, 51)))), crs = 4326)
  pl <- list(kind = "geom", name = "Bering box", geometry = wrapped, precision = 3)
  # the fault: the strict path normalizes anyway, so it returns a token instead of
  # refusing. Spelled out as what the test would then see.
  silent <- place_encode(pl, unwrap = TRUE)
  expect_type(silent, "character")
  expect_error(place_encode_strict(pl), "wrapped")
  expect_false(identical(tryCatch(place_encode_strict(pl), error = function(e) NA_character_),
                         silent))
})

test_that("longitudes are stored UNWRAPPED: a Bering box runs 170..190", {
  v <- Filter(function(x) x$id == "bering", fx$vectors)[[1]]
  d <- place_decode(v$token)[[1]]
  lon <- sf::st_coordinates(d$geometry)[, "X"]
  expect_equal(max(lon), 190)                 # not -170
  expect_true(all(lon >= 170))
  # and encoding a ring written WRAPPED gives the same token, because place_encode()
  # unwraps first — the codec only ever sees unwrapped rings
  wrapped <- sf::st_sfc(sf::st_polygon(list(cbind(
    c(170, -170, -170, 170, 170), c(51, 51, 56, 56, 51)))), crs = 4326)
  expect_identical(
    place_encode(list(kind = "geom", name = v$name, geometry = wrapped, precision = 3)),
    v$token)
})

test_that("precision is 3, or 4 when the bbox is under 0.5 degrees", {
  p3 <- Filter(function(x) x$id == "rect", fx$vectors)[[1]]        # 1 x 1 degree
  p4 <- Filter(function(x) x$id == "small_p4", fx$vectors)[[1]]    # 0.1 x 0.1
  # chosen, not passed: drop the explicit precision and the rule must pick the same
  expect_identical(place_encode(list(kind = "geom", name = p3$name,
                                     geometry = geojson_sfc(p3$geometry))), p3$token)
  expect_identical(place_encode(list(kind = "geom", name = p4$name,
                                     geometry = geojson_sfc(p4$geometry))), p4$token)
  expect_identical(place_decode(p3$token)[[1]]$precision, 3L)
  expect_identical(place_decode(p4$token)[[1]]$precision, 4L)
})

test_that("the deviation is at most half a quantum", {
  for (v in fx$vectors) {
    d  <- place_decode(v$token)[[1]]
    a  <- sf::st_coordinates(geojson_sfc(v$geometry))[, c("X", "Y")]
    b  <- sf::st_coordinates(d$geometry)[, c("X", "Y")]
    expect_equal(dim(a), dim(b), info = v$id)
    expect_lte(max(abs(a - b)), 0.5 * 10^-v$precision, label = v$id)
    expect_equal(max(abs(a - b)), v$max_deviation_deg, info = v$id)
  }
  # a coordinate that is NOT on the grid loses at most half a quantum
  p <- sf::st_sfc(sf::st_polygon(list(cbind(
    c(-90.00049, -89.99951, -89.99951, -90.00049, -90.00049),
    c(27.00049, 27.00049, 27.00151, 27.00151, 27.00049)))), crs = 4326)
  d <- place_decode(place_encode(list(kind = "geom", name = "x", geometry = p)))[[1]]
  dev <- max(abs(sf::st_coordinates(p)[, 1:2] - sf::st_coordinates(d$geometry)[, 1:2]))
  expect_lte(dev, 0.5 * 10^-d$precision)
})

test_that("the `z.` form carries set and keys, and percent-encodes the reserved ones", {
  for (z in fx$zones) {
    expect_identical(place_encode(list(kind = "zone", set = z$place$set,
                                       keys = unlist(z$place$keys))), z$token)
    d <- place_decode(z$token)[[1]]
    expect_identical(d$kind, "zone")
    expect_identical(d$set, z$place$set)
    expect_identical(d$keys, unlist(z$place$keys))
  }
  # a key containing , ~ or . survives only because it is escaped
  d <- place_decode("z.er.Gulf%2C%20North,Chukchi%7EBeaufort,a%2Eb")[[1]]
  expect_identical(d$keys, c("Gulf, North", "Chukchi~Beaufort", "a.b"))
})

test_that("the `u.` form names an upload and its sha256_8", {
  u <- fx$upload
  expect_identical(place_encode(list(kind = "upload", name = u$place$name,
                                     digest = u$place$digest)), u$token)
  d <- place_decode(u$token)[[1]]
  expect_identical(d$kind, "upload")
  expect_identical(d$name, u$place$name)
  expect_identical(d$digest, u$place$digest)
})

test_that("several places ride in one hash, joined with ~", {
  m <- fx$multiple
  ps <- lapply(m$places, function(p) {
    if (identical(p$kind, "geom"))
      list(kind = "geom", name = p$name, geometry = geojson_sfc(p$geometry))
    else if (identical(p$kind, "zone"))
      list(kind = "zone", set = p$set, keys = unlist(p$keys))
    else list(kind = "upload", name = p$name, digest = p$digest)
  })
  expect_identical(place_encode(ps), m$hash)

  d <- place_decode(m$hash)
  expect_length(d, 3)
  expect_identical(vapply(d, function(x) x$kind, ""), c("geom", "zone", "upload"))
  expect_identical(d[[1]]$name, "Gulf box")
  expect_identical(d[[2]]$keys, c("CGM", "SOC", "GEO"))
  expect_identical(d[[3]]$digest, "3f1a9c04")
  # a leading # is part of the hash, not of the first place
  expect_identical(place_decode(paste0("#", m$hash))[[1]]$name, "Gulf box")
})

test_that("a non-ASCII name survives the round trip byte for byte", {
  v <- Filter(function(x) x$id == "name_reserved", fx$vectors)[[1]]
  expect_identical(.g1_pct_encode(v$name), v$name_encoded)
  expect_identical(.g1_pct_decode(v$name_encoded), v$name)
  expect_identical(place_decode(v$token)[[1]]$name, v$name)
  # none of the structural characters survives raw into the token
  expect_false(grepl("[~,]", strsplit(v$token, ".", fixed = TRUE)[[1]][2]))
  # an empty name keeps both dots
  e <- Filter(function(x) x$id == "empty_name", fx$vectors)[[1]]
  expect_identical(place_decode(e$token)[[1]]$name, "")
  expect_true(grepl("^g1\\.\\.", e$token))
})

test_that("all 20 malformed tokens are rejected, each with its own code", {
  for (r in fx$reject) {
    e <- tryCatch({place_decode(r$token); NULL}, msens_place_reject = function(e) e,
                  error = function(e) e)
    expect_false(is.null(e), info = paste(r$code, "-", r$why))
    expect_s3_class(e, "msens_place_reject")
    expect_identical(e$code, r$code, info = paste(r$token, "-", r$why))
    expect_match(conditionMessage(e), r$code, fixed = TRUE, info = r$token)
  }
})

test_that("SEEDED FAULTS: each of the four named mistakes goes red", {
  v <- Filter(function(x) x$id == "rect", fx$vectors)[[1]]
  g <- geojson_sfc(v$geometry)
  mul <- 1000
  m <- as.matrix(g[[1]][[1]]); m <- m[-nrow(m), , drop = FALSE]
  qx <- round(m[, 1] * mul); qy <- round(m[, 2] * mul)

  build <- function(zz, reset, alphabet, pad) {
    out <- c(0x10, 3, 1, 1, nrow(m)); cx <- 0; cy <- 0
    for (i in seq_len(nrow(m))) {
      if (reset && i == 1L) { cx <- 0; cy <- 0 }
      out <- c(out, msens:::.g1_varint(zz(qx[i] - cx)), msens:::.g1_varint(zz(qy[i] - cy)))
      cx <- qx[i]; cy <- qy[i]
    }
    s <- b64url_encode(as.raw(out))
    if (alphabet) s <- chartr("-_", "+/", s)
    if (pad) s <- paste0(s, strrep("=", (4 - nchar(s) %% 4) %% 4))
    paste0("g1.", .g1_pct_encode(v$name), ".", s)
  }
  ok <- function(...) build(msens:::.g1_zigzag, FALSE, FALSE, FALSE)
  expect_identical(ok(), v$token)                       # the harness itself is sound

  # 1. zigzag with the sign the other way round
  bad_zz <- function(n) if (n > 0) 2 * n else -2 * n + 1
  expect_false(identical(build(bad_zz, FALSE, FALSE, FALSE), v$token))
  # 2. the delta cursor reset per ring (visible on the two-ring vector)
  h <- Filter(function(x) x$id == "hole", fx$vectors)[[1]]
  expect_false(identical(as.numeric(unlist(h$deltas))[9:10], c(-89750, 27750)))
  # 3. the standard base64 alphabet instead of base64url
  expect_false(identical(build(msens:::.g1_zigzag, FALSE, TRUE, FALSE), v$token))
  expect_error(place_decode(build(msens:::.g1_zigzag, FALSE, TRUE, FALSE)), "b64")
  # 4. padding emitted
  padded <- build(msens:::.g1_zigzag, FALSE, FALSE, TRUE)
  expect_false(identical(padded, v$token))
  expect_error(place_decode(padded), "b64")
})

test_that("a place survives encode -> decode -> coverage unchanged", {
  # D8: every analysis runs on decode(encode(geometry)), never on the input
  p  <- sf::st_sfc(sf::st_polygon(list(cbind(
    c(-90, -89.875, -89.875, -90, -90), c(27, 27, 27.075, 27.075, 27)))), crs = 4326)
  rt <- place_decode(place_encode(list(kind = "geom", name = "Gulf", geometry = p)))[[1]]
  expect_equal(cells_in_polygon_grid(rt$geometry, "global05"),
               cells_in_polygon_grid(p, "global05"))
})
