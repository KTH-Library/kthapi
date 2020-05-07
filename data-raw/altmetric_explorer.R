library(httr)
library(rvest)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(readr)
library(here)

parse_altmetric_explorer <- function(html_file, verbose = FALSE) {

  if (missing(html_file))
    html_file <- "https://www.altmetric.com/explorer/institution/departments"

  # try first to access the online version (loginwall: use a KTH IP address)
  ae <-
    read_html(html_file) %>%
    html_nodes(css = ".department-hierarchy-wrapper > ol") %>%
    html_nodes(css = ".department")

  # use local html file if online scraping blocked by loginwall
  if (is_empty(ae)) {
    if (verbose)
      message("parse_altmetric_explorer(): using local file")
    # this html file comes from a manual download
    html_file <- file.path(here(), "data-raw", "Altmetric Explorer.html")
    ae <-
      read_html(html_file) %>%
      html_nodes(css = ".department-hierarchy-wrapper > ol") %>%
      html_nodes(css = ".department")
  }

  else {
    if (verbose)
      message("parse_altmetric_explorer(): using online data")
  }

  # fcns to scrape department data

  x_level <- function(x)
    x %>% html_attr("class")

  x_id_desc <- function(x)
    x %>% html_nodes(".name") %>%
    html_text() %>%
    # use two capture groups to extract id and desc
    str_match_all("\\[(\\d+)\\]\\s+(.*?)$") %>%
    map_df(as_tibble) %>%
    select(id = 2, desc = 3) %>%
    mutate(id = as.integer(id))

  x_count <- function(x)
    x %>% html_nodes(css = ".count") %>%
    html_node("strong") %>%
    html_text() %>%
    parse_number()

  # Note: the webpage seems to have changed on Altmetric's website,
  # this function has been adapted to work with both the old and the new format
  x_href <- function(x)
    ae %>% html_nodes(css = "a") %>%
    html_attr("href") %>%
    sub("outputs","highlights",.) %>%
    gsub("%3(a|A)",":",.) %>%
    sub("https://www.altmetric.com","",.) %>%
    paste0("https://www.altmetric.com",.)

  # parse individual components and merge into a data frame
  t1 <- bind_cols(
    tibble(
      level = ae %>% x_level,
      count = ae %>% x_count,
      href = ae %>% x_href
    ),
    ae %>% x_id_desc
  )

  # return results
  t1 %>%
  mutate(aid = str_match(href, "department_id=(.*?):department:(.*?)$")[,3]) %>%
  select(diva_id = id, altmetric_id = aid, desc, level, count, href)

}

parse_highlights <- function(html_file, verbose = FALSE) {

  if (missing(html_file))
    html_file <- "https://www.altmetric.com/explorer/highlights"

  # try first to access the online version (loginwall: use a KTH IP address)
  aeh <-
    read_html(html_file) %>%
    html_nodes(".stats")

  # use local html file if online scraping blocked by loginwall
  if (is_empty(aeh)) {
    if (verbose)
      message("parse_highlights(): using local file")
    html_file <- file.path(here(), "data-raw", "Altmetric Explorer Highlights.html")
    aeh <-
      read_html(html_file) %>%
      html_nodes(".stats")
  }

  else {
    if (verbose)
      message("parse_highlights(): using online data")
  }

  # fcns to scrape department data
  indicators <- c("mentions", "mentioned_outputs", "total_outputs")

  x_stat_title <- function(x, indicator)
    x %>% html_nodes(sprintf(".stat.%s .content .title", indicator)) %>%
    html_text()

  x_stat_count <- function(x, indicator)
    x %>% html_nodes(sprintf(".stat.%s .content .count", indicator)) %>%
    html_text() %>%
    readr::parse_number()


  df_indicator <- function(x)
    tibble(
      indicator = aeh %>% x_stat_title(x),
      value = aeh %>% x_stat_count(x)
    )

  indicators %>% map_df(df_indicator)

}


altmetrics_get_attention <- function(json_file, verbose = FALSE) {

  if (missing(json_file))
    json_file <- paste0("https://www.altmetric.com/explorer/api/research_outputs/attention?digest=",
                        openssl::sha1("", Sys.getenv("ALTMETRICS_EXPLORERAPI_SECRET")),
                        "&key=",
                        Sys.getenv("ALTMETRICS_EXPLORERAPI_KEY")
                        )

  local_json_file <- file.path(here(), "data-raw", "altmetric_attention.json")

  data <- tryCatch(
                    {
                      res <- rjson::fromJSON(file=json_file)
                      if (verbose)
                        message("altmetrics_get_attention(): using online data")
                      res
                    },
                    error=function(e){},
                    warning=function(w){
                    if (verbose)
                      message("altmetrics_get_attention(): using local file")
                    rjson::fromJSON(file=local_json_file)
                    }
  )

  data$meta$response$`total-results`

}


altmetric_explorer <- parse_altmetric_explorer()
usethis::use_data(altmetric_explorer, overwrite = TRUE)

altmetric_explorer_attention <- altmetrics_get_attention()
usethis::use_data(altmetric_explorer_attention, overwrite = TRUE)

## Doesn't work online, only with the local file
altmetric_explorer_highlights <- parse_highlights(verbose = TRUE)
usethis::use_data(altmetric_explorer_highlights, overwrite = TRUE)
