# KTH APIs configuration

This function provides a list with settings for the base url and user
agent used in API calls, and the relevant API keys required.

## Usage

``` r
config()
```

## Details

Please edit your ~./Renviron file and set values for environment
variables KTH_API_KEY_DIRECTORY, KTH_API_KEY_PROFILES and
KTH_API_KEY_PUBLICATIONS you can use file.edit("~/.Renviron");
readRenviron("~/.Renviron"))

## See also

[`user_agent`](https://httr.r-lib.org/reference/user_agent.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive()) {

# how to change the config
my_cfg <- config()
my_cfg$ua <- httr::user_agent("My own user agent string")
my_cfg$verbose <- TRUE

# then use this config when making calls
kth_profile_legacy("tjep", config = my_cfg)
 }
} # }
```
