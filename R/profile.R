#' KTH APIs configuration
#'
#' This function provides a list with settings for the base url and user agent
#' used in API calls.
#'
#'
#' @export
config <- function() {
  list(
    url_schemas = "https://www.kth.se/api/schema/v2",
    url_kopps = "https://api.kth.se/api/kopps/v2",
    url_profiles = "https://api.kth.se/api/profile/1.1",
    url_directory = "https://api.kth.se/api/directory/",
    url_places = "https://api.kth.se/api/places",
    url_publications = "https://api.kth.se/api/publications",
    ua = httr::user_agent("http://github.com/hadley/httr")
  )
}

#' Retrieve data for KTH Profiles
#'
#' This API does supports the existing profile api, version 1.1 and has no access key requirements.
#'
#' See details at <https://api.kth.se/api/profile/swagger/?url=/api/profile/swagger.json#/v1.1/getPublicProfile_v11>
#'
#' @param userid a string with the account name or KTH user id
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
#' kth_profile_legacy("hoyce")
#' kth_profile_legacy("agnel")
#' # kth_profile_legacy("markussk")  # error
#' }
kth_profile_legacy <- function(
  userid = NULL,
  config = NULL)
{

  check_internet()
  stop_if_all(args, is.null, "You need to specify at least one argument")
  stop_if_not(is.character(userid) && nchar(userid) > 0,
              msg = "Please provide a query, see http://librishelp.libris.kb.se/help/search_language_swe.jsp")

  if (any(missing(config), is.null(config)))
    config <- config()

  resp <- GET(sprintf("%s/%s", config$url_profiles, userid), config$ua)
  check_status(resp)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- fromJSON(rawToChar(resp$content), flatten = TRUE)

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
    parsed %>%
    tibble::as_tibble() %>%
    jsonlite::flatten() %>%
    tibble::as_tibble()

  structure(
    list(
      content = content,
      query = userid
    ),
    class = "kthapi"
  )
}


#' @export
print.kthapi <- function(x, ...) {
  stopifnot(inherits(x, 'kthapi'))
  cat(sprintf("<KTH API call for %s>\n",x$query))
  print(tibble::as_tibble(x$content))
  invisible(x)
}
