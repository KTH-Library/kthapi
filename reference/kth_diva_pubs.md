# Retrieve DiVA publications for KTH from the KTH DiVA portal

This function sends a request to KTH's DiVA portal for a CSV data export
covering KTH publications between 2012 and 2019.

## Usage

``` r
kth_diva_pubs(
  orgid = "177",
  year_beg = "2012",
  year_end = "2019",
  use_cache = TRUE
)
```

## Arguments

- orgid:

  the DiVA organisation id, by default "177" for KTH

- year_beg:

  the beginning of the period, by default "2012"

- year_end:

  the end of the period, by default "2019"

- use_cache:

  logical flag to indicate if cached data should be used, default: TRUE

## Value

data frame with results
