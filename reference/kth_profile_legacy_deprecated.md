# Retrieve data for KTH Profiles

This API does supports the existing profile api, version 1.1 and has no
access key requirements.

## Usage

``` r
kth_profile_legacy_deprecated(userid = NULL, config = NULL)
```

## Arguments

- userid:

  a string with the account name or KTH user id

- config:

  a configuration setting for the KTH APIs including base URL etc, by
  default from config()

## Value

results records returned from the search

## Details

See details at
<https://api.kth.se/api/profile/swagger/?url=/api/profile/swagger.json#/v1.1/getPublicProfile_v11>

## Examples

``` r
if (FALSE) { # \dontrun{
kth_profile_legacy("agnel")
kth_profile_legacy("tjep")
tryCatch(kth_profile("hoyce"), error = function(e) e, warning("Does not exist?"))
} # }
```
