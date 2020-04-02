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
              msg = "Please provide a userid (accountname or kthid)")

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


#' Retrieve display name given kthid or accountname
#'
#' @param kthid a string with the account name or KTH user id
#' @param cfg configuration setting for the KTH APIs including base URL etc, by default from config()
#' @export
kth_displayname <- function(kthid, cfg = config()) {
  profile <- kth_profile_legacy(kthid, cfg)$content
  accountname <- gsub("(.*?)@.*?$", "\\1", profile$email)
  sprintf("%s %s (%s)",
    profile$givenName, profile$familyName, accountname)
}

#' Retrieve organizational unit for a given kthid or accountname
#'
#' @param kthid a string with the account name or KTH user id
#' @param cfg configuration setting for the KTH APIs including base URL etc, by default from config()
#' @export
#' @examples
#' \dontrun{
#' kth_profile_school_dep("u1z88syr")
#' kth_profile_legacy("hoyce")
#' }
kth_profile_school_dep <- function(kthid, cfg = config()) {
  cfg <- config()
  profile <- kth_profile_legacy(kthid, cfg)$content
  units <- gsub("https://www\\.kth\\.se/directory/(.*?)$", "\\1", profile$`worksFor.url`)
  strsplit(units, "/") %>% unlist()
}

#' Retrieve organizational belonging for a given kthid or accountname at
#' institutional level (assumed to be the second highest hierarchical level)
#'
#' @param kthid a string with the account name or KTH user id
#' @param cfg configuration setting for the KTH APIs including base URL etc, by default from config()
#' @import dplyr
#' @return a tibble with the userid, the "slug" and the org unit description
#' @export
#' @examples
#' \dontrun{
#' kth_belonging_institutional("u1z88syr")
#' kth_belonging_institutional("hoyce")
#' }
kth_belonging_institutional <- function(kthid, cfg = config()) {

  unit_codes <- kth_profile_school_dep(kthid)

  # use the first two levels of the hierarchy
  # for "organizational belonging at level 2 ie institutional level"
  slug <- paste0(collapse = "/", unit_codes[1:2])

  # look up the english lang description for this "slug"
  unit2 <-
    kth_school_dep() %>%
    rename(desc = `description.en`) %>%
    inner_join(tibble(slug = slug, by = c("slug"))) %>%
    select(slug, desc)

  tibble(kthid = kthid) %>% bind_cols(unit2)
}

#' @export
print.kthapi <- function(x, ...) {
  stopifnot(inherits(x, 'kthapi'))
  cat(sprintf("<KTH API call for %s>\n",x$query))
  print(tibble::as_tibble(x$content))
  invisible(x)
}
