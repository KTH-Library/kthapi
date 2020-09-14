#' @importFrom attempt stop_if_not
#' @importFrom curl has_internet
check_internet <- function(){
  stop_if_not(.x = has_internet(), msg = "Please check your internet connection")
}

#' @importFrom httr status_code
check_status <- function(res){
  stop_if_not(
    .x = status_code(res),
    .p = ~ .x == 200,
    msg = "The API returned an error")
}

#' KTH APIs configuration
#'
#' This function provides a list with settings for the base url and user agent
#' used in API calls, and the relevant API keys required.
#'
#' Please edit your ~./Renviron file and set values for environment variables
#' KTH_API_KEY_DIRECTORY, KTH_API_KEY_PROFILES and KTH_API_KEY_PUBLICATIONS
#' you can use file.edit("~/.Renviron"); readRenviron("~/.Renviron"))
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'
#' # how to change the config
#' my_cfg <- config()
#' my_cfg$ua <- httr::user_agent("My own user agent string")
#' my_cfg$verbose <- TRUE
#'
#' # then use this config when making calls
#' kth_profile_legacy("hoyce", config = my_cfg)
#'  }
#' }
#' @seealso
#'  \code{\link[httr]{user_agent}}
#' @rdname config
#' @export
#' @importFrom httr user_agent
config <- function() {
  key_directory <- Sys.getenv("KTH_API_KEY_DIRECTORY")
  key_profiles <- Sys.getenv("KTH_API_KEY_PROFILES")
  key_publications <- Sys.getenv("KTH_API_KEY_PUBLICATIONS")
  if (key_directory == "")
    message("Please set the KTH_API_KEY_DIRECTORY envvar for accessing authed endpoints")
  if (key_profiles == "")
    message("Please set the KTH_API_KEY_PROFILES envvar for accessing authed endpoints")
  if (key_publications == "")
    message("Please set the KTH_API_KEY_PUBLICATIONS envvar for accessing authed endpoints")
  list(
    url_schemas = "https://www.kth.se/api/schema/v2",
    url_kopps = "https://api.kth.se/api/kopps/v2",
    url_profiles_legacy = "https://api.kth.se/api/profile/1.1",
    url_profiles = "https://api.kth.se/api/profile/v1",
    url_directory = "https://api.kth.se/api/directory/v1",
    url_places = "https://api.kth.se/api/places",
    url_publications = "https://api.kth.se/api/publications/v1",
    ua = httr::user_agent("http://github.com/hadley/httr"),
    verbose = FALSE,
    api_key_profiles = key_profiles,
    api_key_directory = key_directory,
    api_key_publications = key_publications
  )
}

#' Status message related to db availability
#' @return list with two slots for status message and status
#' @export
status_kthapi <- function() {

  r_environ_path <- normalizePath("~/.Renviron", mustWork = FALSE)
  envvars <- c("KTH_API_KEY_DIRECTORY", "KTH_API_KEY_PROFILES", "KTH_API_KEY_PUBLICATIONS")

  if (any(Sys.getenv(envvars) == "")) {
    msg <- paste("Please use an .Renviron at", r_environ_path,
                 "with all of these envvars set", paste(envvars))
    return (list (msg = msg, status = FALSE))
  }

  # KTH Profiles API legacy check
  unit_code <- kth_profile_school_dep("hoyce")
  is_valid_1 <- length(unit_code) > 1
  if (!is_valid_1)
    warning("KTH Profiles Legacy API check failed - connectivity issue?")

  # KTH Profiles API check
  profile <- kth_profile(username = "hoyce")
  is_valid_2 <- profile$content$emailAddress == "hoyce@kth.se"
  if (!is_valid_2)
    warning("KTH Profiles API check failed - connectivity issue?")

  # KTH Directory API check
  api_slugs <- kth_school_dep()$slug
  valid_slugs <- unlist(strsplit("acjmst", ""))
  is_valid_3 <- all(valid_slugs %in% api_slugs)
  if (!is_valid_3)
    warning("KTH Directory API check failed - connectivity issue?")

  if (all(is_valid_1, is_valid_2, is_valid_3))
    return (list (msg = "OK", status = TRUE))

  list(msg = "Issue with KTH APIs", status = FALSE)
}
