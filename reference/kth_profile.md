# Retrieve data for KTH Profiles

See details at <https://api.kth.se/api/profile/swagger/index.html>

## Usage

``` r
kth_profile(
  kthid = NULL,
  orcid = NULL,
  socialid = NULL,
  username = NULL,
  config = NULL
)
```

## Arguments

- kthid:

  the kthId for the user profile

- orcid:

  the ORC identifier for the user profile

- socialid:

  the social id for the user profile

- username:

  the accountname for the user profile

- config:

  a configuration setting for the KTH APIs including base URL etc, by
  default from config()

## Value

results records returned from the search

## Examples

``` r
if (FALSE) { # \dontrun{
kth_profile(username = "agnel")
kth_profile(username = "markussk")
tryCatch(kth_profile(username = "hoyce"), error = function(e) e, warning("Does not exist?"))
} # }
```
