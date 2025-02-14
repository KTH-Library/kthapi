
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

This is a basic example which shows you how to make a lookup using the v
1.1 “legacy” Profiles API, where we get contact information for a KTH
employee using an account name:

``` r
profile <- 
  kth_profile_legacy("tjep") %>% 
  .$content 

# inspect this record
profile %>% glimpse()
#> Rows: 1
#> Columns: 12
#> $ givenName          <chr> "Tobias"
#> $ familyName         <chr> "Jeppsson"
#> $ url                <chr> "https://www.kth.se/profile/tjep"
#> $ email              <chr> "tjep@kth.se"
#> $ image              <chr> "https://www.kth.se/files/avatar/tjep"
#> $ telephone          <chr> "087907106"
#> $ jobTitle           <chr> "Bibliometrisk analytiker"
#> $ `jobTitle-en`      <chr> "Librarian"
#> $ workLocation       <chr> "OSQUARS BACKE 31"
#> $ worksFor.url       <chr> "https://www.kth.se/directory/t/tr/trac"
#> $ worksFor.name      <chr> "PUBLICERINGENS INFRASTRUKTUR"
#> $ `worksFor.name-en` <chr> "PUBLICATION INFRASTRUCTURE AND MEDIA"

# pivot into long format and display as a table
profile %>% t() %>% as.data.frame() %>%  
  cbind(rownames(.)) %>% setNames(nm = c("value", "key")) %>% as_tibble() %>%
  select(key, value) %>%
  kable()
```

| key              | value                                    |
| :--------------- | :--------------------------------------- |
| givenName        | Tobias                                   |
| familyName       | Jeppsson                                 |
| url              | <https://www.kth.se/profile/tjep>        |
| email            | <tjep@kth.se>                            |
| image            | <https://www.kth.se/files/avatar/tjep>   |
| telephone        | 087907106                                |
| jobTitle         | Bibliometrisk analytiker                 |
| jobTitle-en      | Librarian                                |
| workLocation     | OSQUARS BACKE 31                         |
| worksFor.url     | <https://www.kth.se/directory/t/tr/trac> |
| worksFor.name    | PUBLICERINGENS INFRASTRUKTUR             |
| worksFor.name-en | PUBLICATION INFRASTRUCTURE AND MEDIA     |

``` r


# NB: some valid account names do not return data
tryCatch(kth_profile_legacy("markussk"), error = function(e) e)
#> <KTH API call for markussk>
#> # A tibble: 0 × 10
#> # ℹ 10 variables: givenName <chr>, familyName <chr>, url <chr>, email <chr>,
#> #   image <chr>, telephone <chr>, jobTitle <chr>, jobTitle-en <chr>,
#> #   worksFor <list>, workLocation <chr>
```

## More examples

This is a basic example which shows how to make a lookup using the
authenticated Profiles API:

``` r

profile <- 
  kth_profile(username = "tjep") %>%
  .$content

# organizational belonging
profile$worksFor$items %>% 
  tibble::as_tibble() %>%
  select(path, name, nameEn) %>%
  knitr::kable()
```

| path      | name                         | nameEn                               |
| :-------- | :--------------------------- | :----------------------------------- |
| t/tr      | KTH BIBLIOTEKET              | KTH LIBRARY                          |
| t/tr/trac | PUBLICERINGENS INFRASTRUKTUR | PUBLICATION INFRASTRUCTURE AND MEDIA |

``` r

# corresponding "slugs"
profile$worksFor$items$path
#> [1] "t/tr"      "t/tr/trac"

# displayname used in ABM app
kth_displayname("tjep", type = "username")
#> [1] "Tobias Jeppsson (tjep)"

# NB: this (authenticated API call) does not throw an error for non-employees
kth_displayname("markussk", type = "username")
#> [1] "Markus Skyttner (markussk)"
```

For more usage examples, please see the package vignettes.
