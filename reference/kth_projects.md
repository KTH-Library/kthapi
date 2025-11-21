# Retrieve data for KTH Projects

See details at <https://api-r.referens.sys.kth.se/api/projects/swagger>

## Usage

``` r
kth_projects(
  path = "projects/",
  year_beg = NULL,
  year_end = NULL,
  kthUserName = NULL,
  orcid = NULL,
  tag = NULL,
  config = NULL
)
```

## Arguments

- path:

  string to indicate endpoint to use, for example "projects",
  "projects/public", "projects/hidden", "fundings/", Default:
  "projects/"

- year_beg:

  starting year, for example 2012

- year_end:

  ending year, for example 2019

- kthUserName:

  username of KTH profile, for example "stemme"

- orcid:

  Orcid of members to get, for example "0000-0003-2983-5573"

- tag:

  project tag

- config:

  a configuration setting for the KTH APIs including base URL etc, by
  default from config()

## Value

resulting records

## Examples

``` r
if (FALSE) { # \dontrun{
kth_projects()
kth_projects("projects/public")$content$projects %>% tibble::as_tibble()
kth_projects("admin/last-manual-import")
kth_projects("admin/weekly-import-information")
} # }
```
