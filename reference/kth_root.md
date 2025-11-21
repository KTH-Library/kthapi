# Retrieve root data from KTH Directory API

See details at <https://api.kth.se/api/directory/swagger/>

## Usage

``` r
kth_root(config = NULL, path, lang)
```

## Arguments

- config:

  a configuration setting for the KTH APIs including base URL etc, by
  default from config()

- path:

  slug for directory, by default "root"

- lang:

  language to use, by default "en"

## Value

results records returned from the search

## Examples

``` r
if (FALSE) { # \dontrun{
kth_root()
} # }
```
