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
#' @export
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
    api_key_profiles = key_profiles,
    api_key_directory = key_directory,
    api_key_publications = key_publications
  )
}
