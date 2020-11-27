library(plumber)
library(dplyr)
library(purrr)
library(kthapi)
library(htmlwidgets)
library(readr)

#* @apiTitle Data from KTH DiVA portal
#* @apiDescription DiVA data from the KTH DiVA portal.
#* Lightweight programmatic access to paper and author data.
#* @apiContact list(name = "API Support", url = "https://KTH-library.github.io", email = "biblioteket@kth.se")
#* @apiLicense list(name = "Apache 2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
#* @apiTag Publication Functionality related to retrieving publication data
#* @apiTag Author Functionality related to retrieving author data
#* @apiTag Issues Potential data quality issues
#* @apiVersion 0.1

pubs <- kth_diva_pubs()

#* Data for a publication given a publication identifier (DiVA PID or paper identifer)
#* @get /v1/paper/<PaperId>
#* @response 400 Invalid input.
#* @param PaperId:string A publication identifier; examples for various identifiers: DiVA: PID:1242492, DOI: DOI:10.1109/ECOC.2017.8345960, PubMed ID: PMID:19872477, UT/ISI: ISI:000208837900085
#* @tag Publication
function(PaperId) {

  id <- trimws(URLdecode(PaperId))
  cat("Got PaperId:", PaperId, "\n")

  re <- function(prefix, x)
    sapply(regmatches(x, regexec(sprintf("%s:(.*)", prefix), x)), "[", 2)

  pid <- re("PID", id)
  pmid <- re("PMID", id)
  doi <- re("DOI", id)
  isi <- re("ISI", id)

  res <- data.frame()

  cat("Identifiers: ", pid, pmid, doi, isi)

  # use fts here! with column:-syntax
  if (!is.na(pid) && nchar(pid) > 0)
    res <- pubs %>% filter(PID == pid) %>% collect()

  if (!is.na(doi) && nchar(doi) > 0)
    res <- pubs %>% filter(DOI == doi) %>% collect()

  if (!is.na(pmid) && nchar(pmid) > 0)
    res <- pubs %>% filter(PMID == pmid) %>% collect()

  if (!is.na(isi) && nchar(isi) > 0)
    res <- kth_diva_pubs() %>% filter(ISI == isi) %>% collect()

  return(res)
}

#* Data for an author, given an author identifier (KTH identifier or ORCID)
#* @get /v1/author/<AuthorId>
#* @response 400 Invalid input.
#* @param AuthorId:string An author identifier; examples for various identifiers (kthid, ORCID): u1mzh00z, 0000-0001-5302-1698
#* @tag Author
function(AuthorId) {

  get_identifier <- function(x, re)
    sapply(regmatches(x, gregexpr(re, x, perl = TRUE)), "[", 1)

  re_orcid <- sprintf("(%s)", paste0(collapse = "-",
    c(rep("\\d{4}", 3), "\\d{3}[1-9Xx]")))

  re_kthid <- "(u1.{6})"

  .kthid <- get_identifier(AuthorId, re_kthid)
  .orcid <- get_identifier(AuthorId, re_orcid)

  res <- data.frame()

  if (!is.na(.kthid) && nchar(.kthid) > 0)
    res <- kth_diva_authors() %>% filter(kthid == .kthid) %>% collect()

  if (!is.na(.orcid) && nchar(.orcid) > 0)
    res <- kth_diva_authors() %>% filter(orcid == .orcid) %>% collect()

  return(res)
}

aliases <- kth_diva_aliases()

#* DiVA author aliases
#* @get /v1/aliases
#* @response 400 Invalid input.
#* @tag Author
#* @serializer csv
function() {
  aliases
}

issues <- kth_diva_issues()

#* Full set of potential issues due to multiple author identifiers etc
#* @get /v1/issues
#* @response 400 Invalid input.
#* @tag Issues
function() {
  issues
}

#* Overview of multiplettes (ORCIDs with multiple associated kthids)
#* @get /v1/issues/overview
#* @response 400 Invalid input.
#* @tag Issues
#* @serializer csv
function() {
  issues$overview
}

#* Publication identifiers associated with author identifier multiplettes,
#* showing potential candidates for merging
#* @get /v1/issues/details
#* @response 400 Invalid input.
#* @tag Issues
#* @serializer csv
function() {
  issues$details
}

#* Publications where one kth identifier is associated with multiple ORCIDs
#* @get /v1/issues/publications
#* @response 400 Invalid input.
#* @tag Issues
#* @serializer csv
function() {
  issues$pubs
}
