#' Classify a publication record by Swedish standard subject categories
#'
#' Umeå University provides a web [api](https://klassificera.ub.umu.se/) described
#'    [here](https://klassificera.ub.umu.se/api2.html), for classifying
#'    english and swedish language records from DiVA or Swepub based on the MODS
#'    format (v 3.2 or later) according to the [Swedish standard](https://www.scb.se/dokumentation/klassifikationer-och-standarder/standard-for-svensk-indelning-av-forskningsamnen/).
#'    Classification can also be made based on a [Web of Science record](https://klassificera.ub.umu.se/uploadFile.txt)
#'    Uploaded record batches may not exceed 200 MB, using batches of 10 MB are
#'    recommended (around 2-3000 records per chunk)
#'
#' The classification is based on information in there record from the title
#'    and abstract (required) but also on keywords, ISSN/journal, ISBN-prefix/
#'    publisher and affiliations. Training data comes from SwePub (July 2020).
#'    It is based on the following papers:
#'    - Dual Coordinate Descent Methods for Logistic Regression and Maximum
#'    Entropy Models. ([doi:10.1007/s10994-010-5221-8](https://dx.doi.org/10.1007/s10994-010-5221-8))
#'    - Entropy-Based Term Weighting Schemes for Text Categorization in VSM. ([doi:10.1109/ICTAI.2015.57](https://dx.doi.org/10.1109/ICTAI.2015.57))
#' @details The classification is made at the research topic level (5 digits)
#'    for English language records and at research subject group level
#'    3 digits for Swedish language records.
#' @param record string the publication identifier string in DiVA or an export file
#'    from Web of Science in the "Plain text/Full record" format
#' @param type string, type of identifier, one of "mods" or "wos", default: "mods"
#' @param threshold a value in between 0.1 and 0.51, which governs to which
#'   which extent a record is classified with more than one subject area
#' @param email email adress for the user
#' @return a tibble with suggested classifications
#' @examples \dontrun{
#'
#' # classify using a DiVA record identifier
#' classify_umu_ub("diva2:515038", threshold = 0.3, email = "john.doe@hotmail.com")
#'
#' # classify using a WoS Record in Flatfile/Plain text format
#' wos_record <- readr::read_lines("https://klassificera.ub.umu.se/uploadFile.txt")
#' classify_umu_ub(wos_record, type = "wos")
#' }
#' @importFrom dplyr between bind_cols
#' @importFrom xml2 write_xml xml_find_first xml_attr xml_contents xml_find_all
#' @importFrom httr upload_file POST accept
#' @importFrom readr write_lines read_tsv
#' @importFrom stringi stri_unescape_unicode
#' @export
classify_umu_ub <- function(record, type = c("mods", "wos"),
  threshold = 0.2, email = "foo.bar@null.se") {

  stopifnot(
    dplyr::between(threshold, 0.1, 0.51),
    grepl("^\\S{1,}@\\S{2,}\\.\\S{2,}$", email)
  )

  diva_mods_url <- function(diva_pid)
    sprintf(paste0("https://kth.diva-portal.org/smash/references",
       "?referenceFormat=MODS&pids=[%s]&fileName=export.xml"), diva_pid)

  filedata_mods <- function(pid) {
    mods <- GET(diva_mods_url(pid))
    mods_file <- tempfile()
    xml2::write_xml(content(mods), mods_file)
    filedata <- httr::upload_file(mods_file, type = "text/xml")
  }

  filedata_wos <- function(wosid) {
    wos_file <- tempfile()
    #on.exit(unlink(wos_file))
    readr::write_lines(wosid, wos_file)
    filedata <- httr::upload_file(wos_file, type = "text/plain")
    return(filedata)
  }

  if (missing(type)) type <- "mods"

  filedata <- switch(type,
    "mods" = filedata_mods(record),
    "wos" = filedata_wos(record)
  )

  #on.exit(unlink(filedata))

  res <-
    httr::POST(
      url = sprintf("https://klassificera.ub.umu.se/api/v1/%s", type),
      encode = "multipart", accept("*/*"),
      query = list(
        key = I(email),
        multilabelthreshold = threshold
      ),
      body = list(data = filedata)
      #    add_headers(`Accept-Encoding` = "gzip"),
      #    add_headers("Content-Type" = "multipart/related"),
      #    verbose()
    )

  if (type == "mods")
    return(readr::read_tsv(content(res, as = "text")))

  # we likely have an xml response, parse it into a table
  topic <- stringi::stri_unescape_unicode("/poster/post/forsknings\\u00e4mne")
  x1 <- content(res)
  x2 <- x1 %>% xml_find_all(topic)

  dplyr::bind_cols(
    UT = x2 %>% xml2::xml_find_first("..") %>% xml2::xml_attr("UT") %>% as.character(),
    desc = x2 %>% xml2::xml_contents() %>% as.character(),
    code = x2 %>% xml2::xml_attr("kod") %>% as.character(),
    prob = x2 %>% xml2::xml_attr("sannolikhet") %>% as.character()
  )

}

#' Classify into subject categories using SwePub Web API
#'
#' Use the title, abstract and keywords to get a suggestion for subject
#'    categories to use from the Swedish standard set of 3 or 5 digit
#'    subject categories.
#' @details Calls the API at https://swepub-qa.kb.se/api/v1/classify
#' @param title the title of the paper
#' @param abstract the abstract
#' @param keywords keywords used
#' @param level either 3 or 5, default: 3
#' @examples \dontrun{
#' classify_swepub(
#'    title = "Magnetic resonance",
#'    abstract = "This paper deals with magnetic resonance",
#'    keywords = "magnetic radiotherapy nuclear",
#'    level = 5
#' )
#' }
#' @importFrom httr with_config config POST accept_json add_headers content_type_json content
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom dplyr tibble
#' @export
classify_swepub <- function(title, abstract, keywords, level = 3) {

  stopifnot(level == 3 || level == 5)
  stopifnot(any(!is.na(c(title, abstract, keywords))))
  stopifnot(any(nzchar(c(title, abstract, keywords))))

  json <-
    list(
      level = as.character(level),
      abstract = abstract,
      title = title,
      keywords = keywords
    )

  res <- # something is wacky with that SSL cert
    httr::with_config(httr::config(ssl_verifypeer = 0L),
    httr::POST(
      url = "https://swepub-qa.kb.se/api/v1/classify",
      body = jsonlite::toJSON(json, auto_unbox = TRUE),
      httr::accept_json(),
      httr::add_headers("Content-Type" = "application/json"),
      httr::content_type_json()
    ))

  out <-
    httr::content(res, type = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)

  tt <- function(x, y)
    sapply(x, function(y) paste0(y, collapse = " > "))

  if (out$status == "no match")
    return(dplyr::tibble())

  dplyr::tibble(
    eng_code = out$suggestions$`eng.code`,
    eng_label = out$suggestions$`eng.prefLabel`,
    eng_topics = tt(out$suggestions$eng._topic_tree),
    swe_code = out$suggestions$swe.code,
    swe_label = out$suggestions$swe.prefLabel,
    swe_topics = tt(out$suggestions$swe._topic_tree)
  )
}

#' DiVA publication records from KTH with subject classification issues
#' @details Data comes from [](https://bibliometri.swepub.kb.se/process)
#' @param institution the institution string, Default: 'kth'
#' @param year_beg filter from year, Default: 2012
#' @param year_end filter to year, Default: 2020
#' @param page_size for paged responses, the page length, Default: 20
#' @return a tibble with results
#' @examples
#' \dontrun{
#' if(interactive()){
#'  issues_swepub()
#'  }
#' }
#' @export
#' @importFrom httr with_config config GET accept
#' @importFrom readr read_csv
issues_swepub <- function(institution = "kth", year_beg = 2012L,
  year_end = 2020L, page_size = 20) {

  res <- httr::with_config(httr::config(ssl_verifypeer = 0L),
    httr::GET(sprintf(paste0("https://bibliometri.swepub.kb.se/api/v1/",
      "process/%s/export?from=%s&to=%s",
      "&audit_flags=UKA_comprehensive_check_invalid",
      "&limit=%s"),
      institution, year_beg, year_end, page_size),
      httr::accept("text/csv")
    )
  )
  readr::read_csv(content(res, as = "text", encoding = "UTF-8"), skip = 1)
}


