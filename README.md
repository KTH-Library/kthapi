
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kthapi

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
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
  kth_profile_legacy("hoyce") %>% 
  .$content 

# inspect this record
profile %>% glimpse()
#> Rows: 1
#> Columns: 12
#> $ givenName          <chr> "Niklas"
#> $ familyName         <chr> "Olsson"
#> $ url                <chr> "https://www.kth.se/profile/hoyce"
#> $ email              <chr> "hoyce@kth.se"
#> $ image              <chr> "https://www.kth.se/files/avatar/hoyce"
#> $ telephone          <chr> "087909357"
#> $ jobTitle           <chr> "FÖRVALTNINGSLEDARE IT"
#> $ `jobTitle-en`      <chr> "IT Solution Manager"
#> $ workLocation       <chr> "DROTTNING KRISTINAS VÄG 48"
#> $ worksFor.url       <chr> "https://www.kth.se/directory/t/tj/tjd/tjda"
#> $ worksFor.name      <chr> "FÖRVALTNING"
#> $ `worksFor.name-en` <chr> "FÖRVALTNING"

# pivot into long format and display as a table
profile %>% t() %>% as.data.frame() %>%  
  cbind(rownames(.)) %>% setNames(nm = c("value", "key")) %>% as_tibble() %>%
  select(key, value) %>%
  kable()
```

| key              | value                                        |
| :--------------- | :------------------------------------------- |
| givenName        | Niklas                                       |
| familyName       | Olsson                                       |
| url              | <https://www.kth.se/profile/hoyce>           |
| email            | <hoyce@kth.se>                               |
| image            | <https://www.kth.se/files/avatar/hoyce>      |
| telephone        | 087909357                                    |
| jobTitle         | FÖRVALTNINGSLEDARE IT                        |
| jobTitle-en      | IT Solution Manager                          |
| workLocation     | DROTTNING KRISTINAS VÄG 48                   |
| worksFor.url     | <https://www.kth.se/directory/t/tj/tjd/tjda> |
| worksFor.name    | FÖRVALTNING                                  |
| worksFor.name-en | FÖRVALTNING                                  |

``` r


# NB: some valid account names do not return data
tryCatch(kth_profile_legacy("markussk"), error = function(e) e)
#> <simpleError: The API returned an error>
```

## More examples

This is a basic example which shows how to make a lookup using the
authenticated Profiles API:

``` r

profile <- 
  kth_profile(username = "hoyce") %>%
  .$content

# organizational belonging
profile$worksFor$items %>% 
  tibble::as_tibble() %>%
  select(path, name, nameEn) %>%
  knitr::kable()
```

| path          | name                    | nameEn |
| :------------ | :---------------------- | :----- |
| t/tj          | IT-AVDELNINGEN          | IT     |
| t/tj/tjd      | SYSTEMFÖRV & UTVECKLING |        |
| t/tj/tjd/tjda | FÖRVALTNING             |        |

``` r

# corresponding "slugs"
profile$worksFor$items$path
#> [1] "t/tj"          "t/tj/tjd"      "t/tj/tjd/tjda"

# displayname used in ABM app
kth_displayname("hoyce", type = "username")
#> [1] "Niklas Olsson (hoyce)"

# NB: this (authenticated API call) does not throw an error for non-employees
kth_displayname("markussk", type = "username")
#> [1] "Markus Skyttner (markussk)"
```

For more usage examples, please see the package vignettes.
