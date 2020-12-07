#' Retrieve DiVA publications for KTH from the KTH DiVA portal
#'
#' This function sends a request to KTH's DiVA portal for a CSV data export
#' covering KTH publications between 2012 and 2019.
#'
#' @param orgid the DiVA organisation id, by default "177" for KTH
#' @param year_beg the beginning of the period, by default "2012"
#' @param year_end the end of the period, by default "2019"
#' @param use_cache logical flag to indicate if cached data should be used, default: TRUE
#' @return data frame with results
#' @export
#' @importFrom jsonlite toJSON
#' @importFrom httr parse_url build_url
#' @importFrom curl curl_download
#' @importFrom rappdirs app_dir
#' @import readr
kth_diva_pubs <- function(orgid = "177", year_beg = "2012", year_end = "2019", use_cache = TRUE) {

  diva_tmp <- function(file) file.path(rappdirs::app_dir("kthapi")$config(), file)
  tmp <- diva_tmp("kth_diva_pubs.rds")
  if (!dir.exists(dirname(tmp))) dir.create(dirname(tmp), recursive = TRUE)
  if (file.exists(tmp)) return(readr::read_rds(tmp))

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

  pubs <- readr::read_csv(cu, col_types = ct)

  if (use_cache) readr::write_rds(pubs, tmp)

  return(pubs)

}

#' Parse a DiVA namestring with bibliographic data
#'
#' This function parses a string and returns the components (such as
#' name, kthid and ORCID, org affiliation)
#'
#' @param diva_name the string with author names and identifiers
#' @return data frame with results
#' @export
#' @importFrom stringi stri_detect_regex stri_replace_all stri_match_all
#' @importFrom dplyr tibble
parse_diva_name <- function(diva_name) {

  #diva_name <- "Jia, Shi;Pang, Xiaodan [u1e3qilb] (KTH [177], Skolan för informations- och kommunikationsteknik (ICT) [5994]) (RISE Acreo AB, NETLAB, SE-16425 Kista, Sweden);Ozolins, Oskars;Yu, Xianbin;Hu, Hao;Yu, Jinlong;Guan, Pengyu;Da Ros, Francesco;Popov, Sergei [u1fs8je2] (KTH [177], Skolan för informations- och kommunikationsteknik (ICT) [5994]);Jacobsen, Gunnar;Galili, Michael;Morioka, Toshio;Zibar, Darko;Oxenlowe, Leif K."
  has_etal <- any(stringi::stri_detect_regex(diva_name, ";et al\\.?,?"))
  if (has_etal) diva_name <- stringi::stri_replace_all(diva_name,
    replacement = "", regex = ";et al\\.?,?")

  dns <- strsplit(diva_name, ";")
  x <- unlist(dns)

  # http://www.drregex.com/2017/11/match-nested-brackets-with-regex-new.html
  # use clipr::read_clip() with below if not R v 4, else ...
  # r"(?=\()(?:(?=(?(1).*?(?=\1)).*?\((.*))(?=(?(2).*?(?=\2)).*?\)(.*)).)+?(?>.*?(?=\1))[^(]*?(?=\2$)"
  # r"(?=\()(?:(?=.*?\((?!.*?\1)(.*\)(?!.*\2).*))(?=.*?\)(?!.*?\2)(.*)).)+?.*?(?=\1)[^(]*(?=\2$)"
  # r"\((?:[^()]+|(?R))*+\)"

  re_parens <-
    paste0("(?=\\()(?:(?=.*?\\((?!.*?\\1)(.*\\)(?!.*\\2).*))",
           "(?=.*?\\)(?!.*?\\2)(.*)).)+?.*?(?=\\1)[^(]*(?=\\2$)")

  re_kthid <- "\\[(u1.{6})\\]"
  re_orcid <- sprintf("\\[(%s)\\]", paste0(collapse = "-", c(rep("\\d{4}", 3), "\\d{3}[1-9Xx]")))

  parse_re <- function(x, re, mode = c("regexec", "gregexpr")) {
    fn <- switch(match.arg(mode), regexec = regexec(re, x), gregexpr = gregexpr(re, x))
    res <- lapply(regmatches(x, fn), "[", -1)
    sapply(res, function(s) if (length(s) == 0) NA_character_ else paste(s, collapse = " "))
  }

  parse_re2 <- function(x, re) {
    res <- lapply(x, function(y) stringi::stri_match_all_regex(y, re)[[1]][ ,2])
    vapply(res, function(s) ifelse(length(s) > 1, paste(s, collapse = " "), s), character(1))
  }

  res <- tibble(
    Name = x,
    name = trimws(parse_re(x, "([^(\\[|\\(]*)")),
    kthid = parse_re2(x, re_kthid),
    orcid = parse_re(x, re_orcid),
    orgids = parse_re2(x, "\\[(\\d+)\\]"),
    extorg = parse_re2(parse_re2(x, "(\\([^(\\[|\\(]*\\))$"), "\\((.*?)\\)"),
    has_etal = has_etal
  )

}
#' Parse all DiVA namestrings in DiVA publications
#'
#' This function parses all DiVA namestrings in a set of DiVA publications.
#'
#' @param pubs a data frame with diva publications, by default kth_diva_pubs()
#' @return data frame with results
#' @export
#' @importFrom progress progress_bar
#' @importFrom purrr map2_df
#' @import dplyr
parse_diva_names <- function(pubs = kth_diva_pubs()) {

  items <- pubs %>% filter(!is.na(PID) & !is.na(Name)) %>% select(PID, Name)

  pb <- progress::progress_bar$new(
    format = "parsing items [:bar] :percent eta: :eta",
    total = nrow(items))

  parse_names <- function(x, y) {
    pb$tick()
    bind_cols(tibble(PID = x), parse_diva_name(y))
  }

  res <- purrr::map2_df(items$PID, items$Name, parse_names)

  # remap some values due to dirty input
  res %>%
    mutate(is_external = is.na(kthid) & is.na(orcid) & is.na(orgids)) %>%
    mutate(is_unmatched_extorg = is_external & (Name != name) & is.na(extorg)) %>%
    mutate(n_commas = nchar(gsub("[^,]", "", name))) %>%
    mutate(is_remappable_extorg = n_commas > 2 & is.na(extorg)) %>%
    mutate(extorg = ifelse(is_remappable_extorg, name, extorg)) %>%
    mutate(name = ifelse(is_remappable_extorg, NA_character_, name)) %>%
    mutate(extorg = gsub("\\.\\)$", "", extorg))

  # pubs may have a kthid, orcid, name or extorg
  # we group on combinations of those and assign unique PIDs
  pids <-
    res %>% inner_join(pubs, by = "PID") %>%
    group_by(kthid, orcid, name, extorg) %>%
    mutate(pids = paste0(collapse = " ", unique(PID))) %>%
    mutate(n_pid = lengths(strsplit(pids, " "))) %>%
    select(kthid, orcid, name, extorg, pids, orgids, n_pid) %>% distinct() %>%
    arrange(desc(n_pid))

  res %>% left_join(pids)
}

#' Retrieve DiVA authors for KTH from the KTH DiVA portal
#'
#' This function returns parsed author information from DiVA data
#'
#' @param orgid the DiVA organisation id, by default "177" for KTH
#' @param year_beg the beginning of the period, by default "2012"
#' @param year_end the end of the period, by default "2019"
#' @param use_cache logical flag to indicate if cached data should be used,
#' default: TRUE
#' @param refresh_cache logical flag to indicate if data cache should be
#' refreshed, default: FALSE
#' @return data frame with results
#' @export
#' @importFrom readr write_rds read_rds
kth_diva_authors <- function(orgid = "177", year_beg = "2012", year_end = "2019",
                             use_cache = TRUE, refresh_cache = FALSE) {

  diva_tmp <- function(file) file.path(rappdirs::app_dir("kthapi")$config(), file)
  tmp <- diva_tmp("kth_diva_authors.rds")
  if (!dir.exists(dirname(tmp))) dir.create(dirname(tmp), recursive = TRUE)
  if (file.exists(tmp) && !refresh_cache) return(readr::read_rds(tmp))

  .pubs <- kth_diva_pubs(orgid, year_beg, year_end, use_cache)
  parsed_diva_names <- parse_diva_names(pubs = .pubs)

  if (use_cache) readr::write_rds(parsed_diva_names, tmp)

  return(pubs)
}

#' Name aliases for DiVA authors in the KTH DiVA portal
#'
#' This function returns data for authors that for the same author identifier
#' have multiple names registered in composite DiVA Name strings
#' (bibliographic names for a publication).
#'
#' @param authors a tibble with authors data, default: kth_diva_authors()
#' @return data frame with results
#' @export
#' @import dplyr
kth_diva_aliases <- function(authors = kth_diva_authors()) {

  namez <- authors

  # aliases (kthids with more than one name variation)
  aliases_kthid <-
    namez %>% filter(!is.na(kthid) & !is.na(name)) %>% group_by(kthid)  %>%
    mutate(n2 = n_distinct(name)) %>% filter(n2 > 1) %>% arrange(desc(n2)) %>%
    distinct(kthid, name, n2) %>% ungroup() %>%
    arrange(desc(n2), kthid, name) %>%
    select(kthid, name, n2)

  # aliases (orcids with more than one name variation)
  aliases_orcid <-
    namez %>% filter(!is.na(orcid) & !is.na(name)) %>% group_by(orcid)  %>%
    mutate(n2 = n_distinct(name)) %>% filter(n2 > 1) %>% arrange(desc(n2)) %>%
    distinct(orcid, name, n2) %>% ungroup() %>%
    arrange(desc(n2), orcid, name) %>%
    select(orcid, name, n2)

  aliases_kthid %>%
    left_join(aliases_orcid) %>%
    select(kthid, orcid, n = n2, name) %>%
    collect()

}

#' Potential data quality issues for DiVA author data
#'
#' This function summarizes some potential data quality issues for DiVA
#' author data, for example records with ORCIDs that relates to multiple
#' KTH author identifiers and vice versa. Some of these records may be
#' candidates for merging author data at the source.
#'
#' @param authors a tibble with authors data, default: kth_diva_authors()
#' @return a list with slots for data frames (details, overview, and associated
#' publications)
#' @export
#' @import dplyr
kth_diva_issues <- function(authors = kth_diva_authors()) {

  namez <- authors

  # orcids which relates to more than one kthid
  # these could be data quality issues?
  details <-
    namez %>% filter(!is.na(orcid) & !is.na(kthid)) %>% group_by(orcid) %>%
    mutate(n1 = n_distinct(name), n2 = n_distinct(kthid)) %>% filter(n2 > 1) %>%
    distinct(orcid, kthid, n2, name, Name) %>% ungroup() %>%
    arrange(orcid, kthid, desc(n2), name) %>%
    select(orcid, kthid, n = n2) %>% inner_join(namez) %>%
    distinct(orcid, kthid, pids) %>%
    collect()

  overview <-
    details %>%
    group_by(orcid) %>%
    count(kthid) %>%
    arrange(orcid, desc(n)) %>%
    collect()

  # publications for kthids that have several different orcids
  pubs <-
    namez %>% left_join(namez %>%
      filter(!is.na(orcid) & !is.na(kthid)) %>%
      distinct(orcid, kthid), by = "kthid") %>%
      filter(orcid.x != orcid.y) %>%
      select(kthid, orcid.x, orcid.y, everything()
    ) %>%
  arrange(desc(kthid)) %>%
    collect()

  list(overview = overview, details = details, pubs = pubs)
}


