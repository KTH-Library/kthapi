
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kthapi

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/KTH-Library/kthapi/workflows/R-CMD-check/badge.svg)](https://github.com/KTH-Library/kthapi/actions)
[![R-CMD-check](https://github.com/KTH-Library/kthapi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KTH-Library/kthapi/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of the `kthapi` R package is to provide an API wrapper for some
of the APIs used at KTH, The Royal Institute of Technology, which can be
used directly in R.

The KTH APIs are described here:

<https://www.kth.se/api/anvand-data-fran-kth-1.57059>

The KTH APIs provide information about employee profiles, published web
content, places, course schemas and program catalogues. This R package
interfaces with the API, making data available to use directly from R.

## Installation

You can install the development version of kthapi from GitHub with:

``` r
#install.packages("devtools)
devtools::install_github("KTH-Library/kthapi", dependencies = TRUE)
```

## Example usage

This API wrapper / client is pre-configured with a set of API endpoints:

``` r
library(kthapi)
library(knitr)
suppressPackageStartupMessages(library(dplyr))
```

This is a basic example which shows you how to make a lookup using the
“legacy” Profiles API, where we get contact information for a KTH
employee using an account name:

``` r

profile <- 
  kth_profile_legacy(userid = "tjep") |>
  getElement("content") 

# inspect this record
profile |> glimpse()
#> Rows: 1
#> Columns: 13
#> $ givenName          <chr> "Tobias"
#> $ familyName         <chr> "Jeppsson"
#> $ url                <chr> "https://www.kth.se/profile/tjep"
#> $ email              <chr> "tjep@kth.se"
#> $ image              <chr> "https://www.kth.se/files/avatar/tjep"
#> $ telephone          <chr> "087907106"
#> $ jobTitle           <chr> "Bibliometrisk analytiker"
#> $ `jobTitle-en`      <chr> "Librarian"
#> $ workLocation       <chr> "Osquars Backe 31"
#> $ worksFor.url       <chr> "https://www.kth.se/directory/t/tr/trac"
#> $ worksFor.name      <chr> "Publiceringens infrastruktur och media"
#> $ `worksFor.name-en` <chr> "Publication Infrastructure & Media"
#> $ path               <chr> "t/tr/trac"

# pivot into long format and display as a table
profile %>% t() %>% as.data.frame() %>%  
  cbind(rownames(.)) %>% setNames(nm = c("value", "key")) %>% as_tibble() %>%
  select(key, value) %>%
  kable()
```

| key              | value                                    |
|:-----------------|:-----------------------------------------|
| givenName        | Tobias                                   |
| familyName       | Jeppsson                                 |
| url              | <https://www.kth.se/profile/tjep>        |
| email            | <tjep@kth.se>                            |
| image            | <https://www.kth.se/files/avatar/tjep>   |
| telephone        | 087907106                                |
| jobTitle         | Bibliometrisk analytiker                 |
| jobTitle-en      | Librarian                                |
| workLocation     | Osquars Backe 31                         |
| worksFor.url     | <https://www.kth.se/directory/t/tr/trac> |
| worksFor.name    | Publiceringens infrastruktur och media   |
| worksFor.name-en | Publication Infrastructure & Media       |
| path             | t/tr/trac                                |

``` r


# NB: some valid account names do not return data
tryCatch(kth_profile_legacy("hoyce"), error = function(e) e)
#> <simpleError: No (or disabled) username>
```

## More examples

This is a basic example which shows how to make a lookup using the
authenticated Profiles API:

``` r

profile <- 
  kth_profile(username = "tjep") |>
  _$content

# organizational belonging
profile$worksFor$items |> 
  tibble::as_tibble() |> 
  select(key, name, nameEn) |> 
  mutate(slug = kthapi:::path_worksFor(key)) |> 
  knitr::kable()
```

| key | name | nameEn | slug |
|:---|:---|:---|:---|
| app.katalog3.T | Verksamhetsstöd | University Administration | t |
| app.katalog3.T.TR | KTH Biblioteket | KTH Library | t/tr |
| app.katalog3.T.TR.TRAC | Publiceringens infrastruktur och media | Publication Infrastructure & Media | t/tr/trac |

``` r

# displayname used in ABM app
kth_displayname("tjep", type = "username")
#> [1] "Tobias Jeppsson (tjep)"

# NB: this (authenticated API call) does not throw an error for non-employees
kth_displayname("markussk", type = "username")
#> [1] "Markus Skyttner (markussk)"
```

For more usage examples, please see the package vignettes.
