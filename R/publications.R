#'
kth_publication_mods_uri <- function(pid) {
  sprintf("http://kth.diva-portal.org/smash/references?referenceFormat=MODS&pids=[%s]",
    paste0(collapse=pid))
}

#http://kth.diva-portal.org/smash/record.jsf?searchType=RESEARCH&language=en&query=&af=%5B%22personOrgId%3A5993%22%5D&aq=%5B%5B%5D%5D&aq2=%5B%5B%5D%5D&aqe=%5B%5D&noOfRows=50&sortOrder=author_sort_asc&sortOrder2=title_sort_asc&onlyFullText=false&sf=all#

#' Retrieve data for KTH Publications
#'
#' See details at <https://api.kth.se/api/publications/swagger>
#' @param path one of a set of valid API calls
#' @param username the accountname parameter to use
#' @param orgid the organisation identifier to use
#' @param is_html string to indicate if HTML should be returned, default "false"
#' @param style the style to use (when requesting organisation path)
#' @param divaUri the diva URI to use (when requesting organisation/diva path)
#' @param q regexp filter (when requesting organizations path)
#' @param lang the language code (one of "en" or "sv")
#' @param config a configuration setting for the KTH APIs including base URL etc, by default from config()
#'
#' @importFrom attempt stop_if_all stop_if_not
#' @importFrom jsonlite fromJSON flatten
#' @importFrom httr GET http_type status_code
#' @importFrom tibble as_tibble
#' @importFrom progress progress_bar
#' @import tibble dplyr
#' @export
#'
#' @return results records returned from the search
#' @examples
#' \dontrun{
#' kth_publications(path = "userstatus", username = "tjep")
#'
#' orgid <- tibble::as_tibble(kth_publications(path = "organisations")$content) %>%
#'     dplyr::filter(nameLocalized == "Library") %>% dplyr::pull(id)
#'
#' pubs <- kth_publications("organisation", orgid = orgid)$content$publications %>%
#'     tibble::as_tibble()
#'
#' uri <- pubs$identifierUri[1]
#' }
kth_publications <- function(
  path = c("filteredPublications", "userstatus", "stats", "activePublicUsers",
           "activeNotPublicUsers", "activeUsersWithPublications", "activeUsersWithoutPublications",
           "organisation", "organisations"),
  username = NULL, orgid = NULL,
  is_html = c("false", "true"), lang = c("sv", "en"), style = c("ieee", "apa"),
  divaUri = NULL, q = NULL,
  config = NULL)
{

  # filteredPublications, userstatus (username)
  # c("stats", "activePublicUsers", "activeNotPublicUsers", "activeUsersWithPublications", "activeUsersWithoutPublications")
  # organisation/{id}(id, html, l, style)
  # organisation/diva(divaUri)
  # organisations(q, l)

  check_internet()
  stop_if_all(args, is.null, "You need to specify at least one argument")

  if (any(missing(config), is.null(config)))
    config <- config()

  is_valid_arg <- function(x) !is.null(x) && nchar(x) > 0

  params <- NULL
  if (path %in% c("filteredPublications", "userstatus")) {
    stop_if_not(username, is_valid_arg, msg = "Please provide a valid username")
    path <- sprintf("%s/%s", path, username)
  } else if (path %in% c("stats", "activePublicUsers", "activeNotPublicUsers",
     "activeUsersWithPublications", "activeUsersWithoutPublications")) {
    # do nothing
  } else if (path == "organisation") {
    if (is_valid_arg(orgid)) {
      path <- sprintf("%s/%s", path, orgid)
      params <- list(
        html = match.arg(is_html),
        l = match.arg(lang),
        style = match.arg(style)
      )
    } else if (is_valid_arg(divaUri)) {
      path <- sprintf("%s/diva", path)
      stop_if_not(divaUri, is_valid_arg, msg = "Please provide a valid Diva URI")
      params <- list(divaUri = divaUri)
    }
  } else if (path == "organisations") {
    if (is.null(q)) q <- ".*"
    params <- list(q = q, l = match.arg(lang))
  }

  url <- sprintf("%s/%s", config$url_publications, path)
  if (config$verbose) message("Sending GET to url: ", url)

  if (is.null(params)) {
    resp <- GET(url,
      add_headers(api_key = config$api_key_publications), config$ua)
  } else {
    resp <- GET(url, query = params,
      add_headers(api_key = config$api_key_publications), config$ua)
  }

  check_status(resp)

  if (path == "organisation/diva") {
    parsed <- httr::content(resp)
  } else {
    if (http_type(resp) != "application/json") {
      print(content(resp))
      stop("API did not return json", call. = FALSE)
    }
    parsed <- fromJSON(rawToChar(resp$content)) #, flatten = TRUE)
  }

  if (status_code(resp) != 200) {
    stop(
      sprintf(
        "API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  # parse nested json into tabular format
  content <-
    parsed

  structure(
    list(
      content = content,
      query = path
    ),
    class = c("kthapipublications")
  )
}

#' @export
print.kthapipublications <- function(x, ...) {
  stopifnot(inherits(x, 'kthapipublications'))
  cat(sprintf("<KTH API call for %s>\n",x$query))
  print(x$content)
  invisible(x)
}
