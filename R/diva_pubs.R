#' Retrieve DiVA publications for KTH from the KTH DiVA portal
#'
#' This function sends a request to KTH's DiVA portal for a CSV data export
#' covering KTH publications between 2012 and 2019.
#'
#' @param orgid the DiVA organisation id, by default "177" for KTH
#' @param year_beg the beginning of the period, by default "2012"
#' @param year_end the end of the period, by default "2019"
#' @return data frame with results
#' @export
#' @importFrom jsonlite toJSON
#' @importFrom httr parse_url build_url
#' @importFrom curl curl_download
#' @import readr
kth_diva_pubs <- function(orgid = "177", year_beg = "2012", year_end = "2019") {

  message("Please be patient, the export takes a few minutes to complete...")

  pubtypes <- function() c(
    "bookReview", "review", "article",
    "artisticOutput", "book", "chapter",
    "manuscript", "collection", "other",
    "conferencePaper", "patent", "conferenceProceedings",
    "report", "dataset"
  )

  queryparam_aq2 <- function(.pubtypes = pubtypes()) {

    list(list(
      list(dateIssued = I(list(from = year_beg, to = year_end))),
      list(organisationId = orgid, `organisationId-Xtra` = TRUE),
      list(publicationTypeCode = .pubtypes))
    ) %>% jsonlite::toJSON(auto_unbox = TRUE)

  }

  smash_url <- httr::parse_url("https://kth.diva-portal.org/smash/export.jsf")

  # add any params? "language=en&searchType=RESEARCH&query=&af=[]&onlyFullText=false&sf=all"
  smash_url$query <- list(
    format = "csvall2", addFilename = "true",
    aq = I("[[]]"), aqe = I("[]"), aq2 = I(queryparam_aq2()),
    onlyFullText = "false", noOfRows = as.character(5e6L),
    sortOrder = "title_sort_asc", sortOrder2 = "title_sort_asc"
  )

  smash_file <- tempfile() #"~/repos/semanticscholar/data-raw/kth.csv"
  on.exit(unlink(smash_file))

  url <- httr::build_url(smash_url)
  cu <- curl::curl_download(url, destfile = smash_file)
  #download.file(url_smash, destfile = smash_file)
  #res <- curl_fetch_memory(build_url(smash_url))

  ct <- readr::cols(
    .default = col_character(),
    PID = col_double(),
    Year = col_double(),
    PMID = col_double(),
    Opponents = col_logical(),
    Supervisors = col_logical(),
    Examiners = col_logical(),
    ThesisLevel = col_logical(),
    Credits = col_logical(),
    Programme = col_logical(),
    Subject = col_logical(),
    Uppsok = col_logical(),
    DefencePlace = col_logical(),
    DefenceLanguage = col_logical(),
    DefenceDate = col_logical(),
    CreatedDate = col_date(format = ""),
    PublicationDate = col_date(format = ""),
    LastUpdated = col_date(format = ""),
    NumberOfAuthors = col_double(),
    ExternalCooperation = col_logical(),
    FridaLevel = col_double(),
    Term = col_logical(),
    Reviewed = col_logical(),
    FreeFulltext = col_logical(),
    SustainableDevelopment = col_logical()
  )

  readr::read_csv(cu, col_types = ct)

}

