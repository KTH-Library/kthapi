# Retrieve catalog data from the KTH Directory API given a slug for an organizational unit

Retrieve catalog data from the KTH Directory API given a slug for an
organizational unit

## Usage

``` r
kth_catalog(cfg = NULL, slug = NULL, lang = NULL)
```

## Arguments

- cfg:

  a configuration setting for the KTH APIs including base URL etc, by
  default from config()

- slug:

  the slug (a kind of human readable organizational identifer used at
  KTH) to enumerate

- lang:

  language, by default "en" is used, can also be set to "sv"

## Value

results records returned from the search

## Examples

``` r
if (FALSE) { # \dontrun{
kth_catalog("s")
} # }
```
