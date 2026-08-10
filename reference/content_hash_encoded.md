# Fold the raster ENCODING into a payload hash

[`content_hashes()`](http://marinesensitivity.org/msens/reference/content_hashes.md)
identifies the PAYLOAD, which is what dedup needs. But the bytes at an
object key also depend on how the raster was written — datatype, NoData,
whether overviews were built. Changing any of those while the key stays
put means one URL serves two different files, and GDAL's `/vsicurl`
caches a file's header and byte ranges per URL: after such a swap,
low-zoom tiles 500 because the cached header describes bytes that are no
longer there, while high-zoom tiles keep working. (Observed exactly this
when dropping overviews: z5+ fine, z2–z4 HTTP 500, and a cache-busted
URL 200.)

## Usage

``` r
content_hash_encoded(content_hash, enc)
```

## Arguments

- content_hash:

  payload hash(es) from
  [`content_hashes()`](http://marinesensitivity.org/msens/reference/content_hashes.md)

- enc:

  short encoding tag, e.g. `"flt4s-nd9999-noovr"`

## Value

16-char hex hash(es) identifying payload + encoding

## Details

Folding the encoding in makes the key identify the BYTES, so any
encoding change publishes to a NEW url and no cache can be stale. Dedup
is unaffected: identical payloads written the same way still collapse to
one object.
