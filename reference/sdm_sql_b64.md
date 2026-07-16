# Encode SQL as urlsafe base64 for the TiTiler `sql=` query parameter

Matches `server/titiler/factory.py::_decode_sql` (`urlsafe_b64decode`,
padding optional). Use the result directly in `?sql=` on the /msens
endpoints.

## Usage

``` r
sdm_sql_b64(sql)
```

## Arguments

- sql:

  a single `SELECT cell_id, value ...` statement

## Value

urlsafe base64 string (padding stripped)
