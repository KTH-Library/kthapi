#' Retrieve data for KTH Publications
#'
#' See details at <https://api.kth.se/api/publications/swagger/?url=/api/publications/swagger.json#/>
#' @param path one of a set of valid API calls
#' @param username the accountname parameter to use
#' @param orgid the organisation identifier to use
#' @param is_html boolean to indicate if HTML should be returned
#' @param style the style to use (when requesting organisation path)
#' @param divaUri the diva URI to use (when requesting organisation/diva path)
#' @param q regexp filter (when requesting organizations path)
#' @param l the language code (one of "en" or "sv")
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
#' kth_publications(username = "hoyce")
#' }
kth_publications <- function(
  path = c("filteredPublications", "userstatus", "stats", "activePublicUsers",
           "activeNotPublicUsers", "activeUsersWithPublications", "activeUsersWithoutPublications",
           "organisation", "organisations"),
  username = NULL, orgid = NULL, is_html = NULL, l = NULL, style = NULL, divaUri = NULL, q = NULL,
  config = NULL)
{

  # filtererdPublications, userstatus (username)
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
    if (is_valid_arg(id)) {
      path <- sprintf("%s/%s", path, id)
      message("Should add params html, l, style if present here ...")
    } else if (is_valid_arg(divaUri)) {
      path <- sprintf("%s/diva")
      stop_if_not(divaUri, is_valid_arg, msg = "Please provide a valid Diva URI")
      params <- list(divaUri = divaUri)
    }
  } else if (path == "organisations") {
    if (is.null(q)) q <- ".*"
    if (is.null(l)) l <- "en"
    params <- list(q = q, l = l)
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
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- fromJSON(rawToChar(resp$content)) #, flatten = TRUE)

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
    class = "kthapipublications"
  )
}

#' @export
print.kthapipublications <- function(x, ...) {
  stopifnot(inherits(x, 'kthapipublications'))
  cat(sprintf("<KTH API call for %s>\n",x$query))
  print(x$content)
  invisible(x)
}
