# Name aliases for DiVA authors in the KTH DiVA portal

This function returns data for authors that for the same author
identifier have multiple names registered in composite DiVA Name strings
(bibliographic names for a publication).

## Usage

``` r
kth_diva_aliases(authors = kth_diva_authors())
```

## Arguments

- authors:

  a tibble with authors data, default: kth_diva_authors()

## Value

data frame with results
