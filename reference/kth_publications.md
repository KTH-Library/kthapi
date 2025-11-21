# Retrieve data for KTH Publications

See details at <https://api.kth.se/api/publications/swagger>

## Usage

``` r
kth_publications(
  path = c("filteredPublications", "userstatus", "stats", "activePublicUsers",
    "activeNotPublicUsers", "activeUsersWithPublications",
    "activeUsersWithoutPublications", "organisation", "organisations"),
  username = NULL,
  orgid = NULL,
  is_html = c("false", "true"),
  lang = c("sv", "en"),
  style = c("ieee", "apa"),
  divaUri = NULL,
  q = NULL,
  config = NULL
)
```

## Arguments

- path:

  one of a set of valid API calls

- username:

  the accountname parameter to use

- orgid:

  the organisation identifier to use

- is_html:

  string to indicate if HTML should be returned, default "false"

- lang:

  the language code (one of "en" or "sv")

- style:

  the style to use (when requesting organisation path)

- divaUri:

  the diva URI to use (when requesting organisation/diva path)

- q:

  regexp filter (when requesting organizations path)

- config:

  a configuration setting for the KTH APIs including base URL etc, by
  default from config()

## Value

results records returned from the search

## Examples

``` r
if (FALSE) { # \dontrun{
kth_publications(path = "userstatus", username = "tjep")

orgid <- tibble::as_tibble(kth_publications(path = "organisations")$content) |>
    dplyr::filter(nameLocalized == "Bibliotek") %>% dplyr::pull(id)

pubs <- kth_publications("organisation", orgid = orgid)$content$publications |>
    tibble::as_tibble()

kth_publications("organisation", divaUri = kth_diva_org_mods_uri(orgid))$content |>
 as.character() |> xml2::read_html() |> rvest::html_nodes("a") |> as.character()
} # }
```
