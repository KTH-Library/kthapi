# Retrieve organizational belonging for a given kthid or accountname at institutional level (assumed to be the second highest hierarchical level)

Retrieve organizational belonging for a given kthid or accountname at
institutional level (assumed to be the second highest hierarchical
level)

## Usage

``` r
kth_belonging_institutional(kthid, cfg = config())
```

## Arguments

- kthid:

  a string with the account name or KTH user id

- cfg:

  configuration setting for the KTH APIs including base URL etc, by
  default from config()

## Value

a tibble with the userid, the "slug" and the org unit description

## Examples

``` r
if (FALSE) { # \dontrun{
kth_belonging_institutional("u1z88syr")
kth_belonging_institutional("tjep")
} # }
```
