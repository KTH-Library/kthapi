#' @keywords internal
"_PACKAGE"

## usethis namespace: start

#' KTH APIs
#'
#' KTH offers a number of APIs that provides information about employee
#' profiles, published web content, places, course schemas and program
#' catalogues. This R package interfaces with the API, making data
#' available to use from R.
#'
#' An introduction to the KTH APIs in Swedish is available here: <https://www.kth.se/api/anvand-data-fran-kth-1.57059>
#'
#' An overview of the KTH APIs can be found here: <https://www.kth.se/api>
#'
#' Individual APIs are documented here:
#'
#' - Course schema API: <https://www.kth.se/api/schema/v2/apidoc.html>
#' - Program catalogue API: <https://api.kth.se/api/kopps/v2/>
#' - Profiles API (personal contact info): <https://api.kth.se/api/profile/swagger/>
#' - Directory API : <https://api.kth.se/api/directory/swagger/>
#' - Places API: <https://api.kth.se/api/places/swagger/>
#' - Publications API: <https://api.kth.se/api/publications/swagger/>
#' - Projects API: <https://api-r.referens.sys.kth.se/api/projects/swagger>
#'
#' 

## usethis namespace: end
NULL

#' @name globals
#' @title globals
utils::globalVariables(c(".", "description.en", "Name", "PID", "extorg",
"is_external", "is_remappable_extorg", "kthid", "n2", "n_commas",
"n_pid", "name", "orcid", "orcid.x", "orcid.y", "orgids", "pids", "pubs"))

#print_global_vars <- function(undef_global_vars)
#  cat(paste(collapse = ", ", sprintf("\"%s\"", unlist(strsplit(undef_global_vars, " ")))))
