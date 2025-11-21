# Retrieve DiVA authors for KTH from the KTH DiVA portal

This function returns parsed author information from DiVA data

## Usage

``` r
kth_diva_authors(
  orgid = "177",
  year_beg = "2012",
  year_end = "2019",
  use_cache = TRUE,
  refresh_cache = FALSE
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

- refresh_cache:

  logical flag to indicate if data cache should be refreshed, default:
  FALSE

## Value

data frame with results
