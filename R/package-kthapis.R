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
#' @name kthapis
#' @docType package
#' @keywords package
#' @aliases package-kthapis
#'
# needed for use of . in magrittr pipelines
utils::globalVariables(c(".", "description.en"))
NULL

#' Public data with KTH Departments at Altmetric
#'
#' Departments at KTH are listed with their DiVA Unit identifiers and English names.
#'
#' @source \url{https://www.altmetric.com/explorer/institution/departments}
"altmetric_explorer"

#' Public data with KTH Altmetric Highlights summary indicators
#'
#' @source \url{https://www.altmetric.com/explorer/highlights}
"altmetric_explorer_highlights"

#' Public data with KTH Altmetric Outputs with Attention
#'
#' Data fetched from Altmetric Explorer API
#'
#' @source \url{https://www.altmetric.com/explorer/api/research_outputs/attention}
"altmetric_explorer_attention"

#' Public data for KTH organizational units used in the Annual Bibliometric Monitoring.
#'
#' A table combining data from the `bibliomatrix` package with the KTH Directory API and
#' the Altmetric Exlorer dataset.
#' Departments at KTH used in the ABM application are listed with their
#' DiVA Unit identifiers, kthid identifiers and Altmetric identifiers.
"abm_units"
