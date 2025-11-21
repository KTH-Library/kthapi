# Potential data quality issues for DiVA author data

This function summarizes some potential data quality issues for DiVA
author data, for example records with ORCIDs that relates to multiple
KTH author identifiers and vice versa. Some of these records may be
candidates for merging author data at the source.

## Usage

``` r
kth_diva_issues(authors = kth_diva_authors())
```

## Arguments

- authors:

  a tibble with authors data, default: kth_diva_authors()

## Value

a list with slots for data frames (details, overview, and associated
publications)
