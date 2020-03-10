
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kthapi

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of the `kthapi` R package is to provide an API wrapper for APIs
used at KTH, The Royal Institute of Technology, which can be used
directly in R.

The KTH APIs provides information about employee profiles, published web
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

# display the default API config
config()
#> $url_schemas
#> [1] "https://www.kth.se/api/schema/v2"
#> 
#> $url_kopps
#> [1] "https://api.kth.se/api/kopps/v2"
#> 
#> $url_profiles
#> [1] "https://api.kth.se/api/profile/1.1"
#> 
#> $url_directory
#> [1] "https://api.kth.se/api/directory/"
#> 
#> $url_places
#> [1] "https://api.kth.se/api/places"
#> 
#> $url_publications
#> [1] "https://api.kth.se/api/publications"
#> 
#> $ua
#> <request>
#> Options:
#> * useragent: http://github.com/hadley/httr

# how to change the config
my_cfg <- config()
my_cfg$ua <- "My own user agent string"
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
#> Observations: 1
#> Variables: 12
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
