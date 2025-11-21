# Retrieve organizational unit for a given kthid or accountname

Retrieve organizational unit for a given kthid or accountname

## Usage

``` r
kth_profile_school_dep(kthid, cfg = config())
```

## Arguments

- kthid:

  a string with the account name or KTH user id

- cfg:

  configuration setting for the KTH APIs including base URL etc, by
  default from config()

## Examples

``` r
if (FALSE) { # \dontrun{
kth_profile_school_dep("u1z88syr")
kth_profile_legacy("tjep")
} # }
```
