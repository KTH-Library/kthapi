#' Retrieve data from KTH Directory API
#'
#' See details at <https://api.kth.se/api/directory/swagger/>
#'
#' @param config a configuration setting for the KTH APIs including base URL etc, by default from config()
#' @param path slug for directory, by default "root"
#'
#' @importFrom attempt stop_if_all stop_if_not
#' @importFrom jsonlite fromJSON flatten
#' @importFrom httr GET http_type status_code add_headers
#' @importFrom tibble as_tibble
#' @importFrom progress progress_bar
#' @import tibble dplyr
#' @export
#'
#' @return results records returned from the search
#' @examples
#' \dontrun{
#' kth_directory()
#' }
kth_directory <- function(config = NULL, path = "root")
{

  check_internet()
  stop_if_all(args, is.null, "You need to specify at least one argument")

#  stop_if_not(is.character(userid) && nchar(userid) > 0,
#              msg = "Please provide a userid (accountname or kthid)")

  if (any(missing(config), is.null(config)))
    config <- config()

  url <- sprintf("%s/%s", config$url_directory, path)

  message("Getting url: ", url)
  resp <- GET(url, config$ua, add_headers(api_key = config$api_key))


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
      content = content
    ),
    class = "kthapi"
  )
}



