#' Retrieve data for KTH Projects
#'
#' See details at <https://api-r.referens.sys.kth.se/api/projects/swagger/?url=/api/projects/swagger.json>
#' @param path string to indicate endpoint to use, for example
#' "projects", "projects/public", "projects/hidden", "fundings/", Default: "projects/"
#' @param year_beg starting year, for example 2012
#' @param year_end ending year, for example 2019
#' @param kthUserName username of KTH profile, for example "stemme"
#' @param orcid Orcid of members to get, for example "0000-0003-2983-5573"
#' @param tag project tag
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
#' @return resulting records
#' @examples
#' \dontrun{
#' kth_projects()
#' }
kth_projects <- function(path = "projects/",
    year_beg = NULL, year_end = NULL,
    kthUserName = NULL, orcid = NULL, tag = NULL,
    config = NULL) {
  #"https://api-r.referens.sys.kth.se/api/projects/v1/projects"

  check_internet()

  if (any(missing(config), is.null(config)))
    config <- config()

  is_valid_arg <- function(x) !is.null(x) && nchar(x) > 0

  params <- NULL
  if (path %in% c("projects/public", "projects/hidden", "projects/",
                  "project/tags/", "fundings/")) {
  } else if (path %in% c("projects/date/")) {
    stop_if_not(year_beg, is_valid_arg, msg = "Please provide a valid beginning year")
    stop_if_not(year_end, is_valid_arg, msg = "Please provide a valid ending year")
    params <- list(start = year_beg, end = year_end)
  } else if (path %in% c("projects/kthUserName/", "projects/username/")) {
    stop_if_not(kthUserName, is_valid_arg, msg = "Please provide a valid kthUserName")
    path <- paste0("projects/username/", kthUserName)
  } else if (path %in% c("projects/orcid/")) {
   stop_if_not(orcid, is_valid_arg, msg = "Please provide a valid orcid")
    path <- paste0(path, orcid)
  } else if (path %in% c("projects/tag/")) {
    stop_if_not(tag, is_valid_arg, msg = "Please provide a valid tag")
    path <- paste0(path, tag)
  } else {
    warning("... Unsure about endpoint.")
  }

  url <- sprintf("%s/%s", config$url_projects, path)

  if (config$verbose)
    message("Sending GET to url: ", url)

  if (is.null(params)) {
    resp <- GET(url,
                add_headers(api_key = config$api_key_projects), config$ua)
  } else {
    resp <- GET(url, query = params,
                add_headers(api_key = config$api_key_projects), config$ua)
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
    class = "kthapiprojects"
  )

}

#' @export
print.kthapiprojects <- function(x, ...) {
  stopifnot(inherits(x, 'kthapiprojects'))
  cat(sprintf("<KTH API call for %s>\n",x$query))
  print(x$content)
  invisible(x)
}
