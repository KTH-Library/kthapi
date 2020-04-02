#' KTH APIs
#'
#' KTH offers a number of APIs that provides information about employee
#' profiles, published web content, places, course schemas and program
#' catalogues. This R package interfaces with the API, making data
#' available to use from R.
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
#' A table from Altmetric at https://www.altmetric.com/explorer/institution/departments.
#' Departments at KTH are listed with their DiVA Unit identifiers and English names.
"altmetric_explorer"
