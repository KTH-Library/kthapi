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
#' used in API calls.
#'
#'
#' @export
config <- function() {
  key <- Sys.getenv("KTH_API_KEY")
  if (key == "") message("Please set the KTH_API_KEY envvar for accessing authed endpoints")
  list(
    url_schemas = "https://www.kth.se/api/schema/v2",
    url_kopps = "https://api.kth.se/api/kopps/v2",
    url_profiles = "https://api.kth.se/api/profile/1.1",
    url_directory = "https://api.kth.se/api/directory/v1",
    url_places = "https://api.kth.se/api/places",
    url_publications = "https://api.kth.se/api/publications",
    ua = httr::user_agent("http://github.com/hadley/httr"),
    api_key = key
  )
}
