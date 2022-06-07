ldap_config <- function() {

  user <- Sys.getenv("LDAP_USER")
  pass <- Sys.getenv("LDAP_PASS")
  host <- Sys.getenv("LDAP_HOST")
  base <- Sys.getenv("LDAP_BASE")

  if (any(c(user, pass, host, base) == ""))
    stop("please set all of LDAP_USER, LDAP_PASS, LDAP_HOST and LDAP_BASE in your .Renviron")

  list(
    ldap_host = sprintf("ldaps://%s", host),
    ldap_base = sprintf("%s", base),
    ldap_user = sprintf("%s@ug.kth.se", user),
    ldap_pass = sprintf("%s", pass)
  )

}

ldap_cmd_search <- function(cfg = ldap_config(), ldap_query, ldap_attributes) {

  # TODO: URLencode parameters! see https://docs.oracle.com/cd/E19396-01/817-7616/ldurl.html

  a <- ""
  if (!(missing(ldap_attributes)))
    a <- stringr::str_c("\"" , ldap_attributes, "\"", collapse = " ")

  ldapsearch <- "ldapsearch"

  if (Sys.info()["sysname"] == "Windows"){
    ldapsearch <- file.path("C:", "OpenLDAP", "bin", "ldapsearch.exe")
  }

  if (!nzchar(Sys.which(ldapsearch)))
    stop("Cannot find ldapsearch utility on system, pls install it (try sudo apt install ldap-utils)")

  sprintf(
    "%s -LLL -E pr=2147483647/noprompt -H \"%s\" -x -D \"%s\" -w \"%s\" -b \"%s\" \"%s\" %s",
    ldapsearch, cfg$ldap_host, cfg$ldap_user, cfg$ldap_pass, cfg$ldap_base, ldap_query, a
  )
}

# ldap_search("(ugOrcid=*)", c("ugOrcid", "ugKthid"), ldap_config = ldap_config())

ldap_cmd_whoami <- function(cfg = ldap_config()) {

  sprintf(
    "ldapwhoami -H \"%s\" -x -D \"%s\" -w \"%s\"",
    cfg$ldap_host, cfg$ldap_user, cfg$ldap_pass)

}

which_rle <- function(bits) {

  stopifnot(all(is.logical(bits)), is.vector(bits), length(bits) >= 1)

  l <- j <- i <- values <- NULL

  bits %>%
    rle() %>%
    unclass() %>%
    tibble::as_tibble() %>%
    dplyr::rename(l = lengths) %>%
    dplyr::mutate(j = cumsum(l)) %>%
    dplyr::mutate(i = j - l + 1L) %>%
    filter(values == TRUE) %>%
    select(i, j)

}

#' @importFrom base64enc base64decode
parse_ldif <- function(text, ldap_attributes = NULL, dn = NULL) {

  res <- text

  # fix linebreaks in ldif response
  is_ml <- grepl("^\\s{1}.+", res, perl = TRUE)

  seg <- which_rle(is_ml)

  join_lines <- function(lines, i, j)
    paste0(lines[(i - 1)], trimws(lines[i:j]))

  # replace first line of segment with joined lines from full segment
  t1 <-
    purrr::map2_chr(seg$i, seg$j, function(x, y)
      paste0(collapse = "\n", join_lines(res, x, y))
    )

  res[(seg$i - 1)] <- t1

  # comment multiline segments (but not first line)
  purrr::walk2(seg$i, seg$j, function(x, y)
    res[x:y] <- "#"
  )

  # discard commented lines
  res <- grep("^#", res, value = TRUE, invert = TRUE)

  # decode base64-encoded blobs in ldif text
  res <-
    textclean::fgsub(
      x = res,
      pattern = "::\\s+(.*?)$",
      fun = function(x) paste0(": ", rawToChar(base64enc::base64decode(x)))
    )

  # convert to dataframe
  ldif <- res
  re <- "^(.*?):+\\s{1}(.*?)$"
  out <- grep(re, ldif, value = TRUE, perl = TRUE)
  key <- stringr::str_match(out, re)[ ,2]
  value <- stringr::str_match(out, re)[ ,3]

  out <-
    tibble::tibble(key, value) %>%
    tidyr::pivot_wider(names_from = key, values_fn = list)

  if (is.null(ldap_attributes))
    ldap_attributes <- setdiff(names(out), "dn")

  out %>%
    tidyr::unnest(cols = c(dn, ldap_attributes))

}

#' Search Active Directory at KTH
#'
#' This function uses ldapsearch to query the KTH Active Directory. It requires
#' environment variables to be set in .Renviron, specifically
#' LDAP_USER, LDAP_PASS, LDAP_HOST and LDAP_BASE for the service
#' account used for the queries.
#'
#' @details
#'
#' The ldapsearch command makes use of -E pr=2147483647/noprompt to avoid paging
#' and returns results in LDIF format which is parsed into a tibble.
#'
#' It is possible to disable ldaps TLS require certificate check, by setting
#' the environment variable LDAPTLS_REQCERT to "never" (can be done in .Renviron)
#'
#' @param ldap_query the LDAP query to issue, such as 'ugKthid=*'
#' @param ldap_attributes set of attributes to return, by default NULL
#' but can be a character vector of attributes, such as c('ugKthid', 'ugOrcid')
#' @param cfg the connection credentials, by default given from ldap_config()
#' @return tibble with results
#' @examples
#' \dontrun{
#' if(interactive()){
#'  ldap_search("(&(ugOrcid=*)(ugKthid=*))", c("ugKthid", "ugOrcid"))
#'  }
#' }
#' @export
ldap_search <- function(
  ldap_query,
  ldap_attributes = NULL,
  cfg = ldap_config()
) {

  cmd <- ldap_cmd_search(
    cfg = cfg,
    ldap_query = ldap_query,
    ldap_attributes = ldap_attributes
    )

  if (Sys.getenv("LDAPTLS_REQCERT") == "") {
    message("Note: it is possible to disable ldaps TLS require certificate check, by ...")
    message('Sys.setenv("LDAPTLS_REQCERT"="never")')
    message("(or by making this environment variable setting persistent using .Renviron)")
  }

  message("Running LDAP query for ", ldap_query, " w attribs: \n",
          paste0(collapse = " ", ldap_attributes))
  res <- system(cmd, intern = TRUE)

  if (!nzchar(res) && attr(res, "status") == 254)
    stop("Cannot contact LDAP server")

  message("Parsing LDIF respone w ", length(res), " lines of data.")
  parse_ldif(res, ldap_attributes)
}

#' Run ldap search for kthid/orcid pairs
#'
#' A LDAP search resulting in a list with three tibbles;
#' for persons with orcid data, persons with unit affiliation and
#' persons with more than one unit
#' @return list with slots for tibbles
#' @examples
#' \dontrun{
#' if(interactive()){
#'  ug_orcid_kthid_unit()
#'  }
#' }
#' @export
#' @importFrom stringr str_count str_extract
#' @importFrom tidyr separate_rows
ug_orcid_kthid_unit <- function() {

  ugKthid <- ugOrcid <- extensionAttribute15 <- is_multi <- dn <- NULL

  a <-
    ldap_search(
      ldap_query = "(&(ugOrcid=*)(ugKthid=*))",
      ldap_attributes = c(
        "displayName", "ugUsername",
        "ugKthid", "ugOrcid",
        "ugPrimaryAffiliation"
      ))

  orcid_kthid_pairs <-
    a %>%
    select(ugKthid, ugOrcid) %>%
    distinct()

  b <-
    ldap_search(
      ldap_query = "(&(ugKthid=*)(extensionAttribute15=*))",
      ldap_attributes = c(
        "ugKthid", "extensionAttribute15", "ugPrimaryAffiliation"
      )
    ) %>%
    mutate(units = gsub("pa.anstallda.", "", extensionAttribute15, fixed = TRUE)) %>%
    mutate(is_multi = stringr::str_count(units, ",")) %>%
    mutate(unit = stringr::str_extract(units, "([:alnum:]{3,})"))

  # people with multiple affiliated units

  many_orgs <-
    b %>%
    mutate(units = gsub("pa.anstallda.", "", extensionAttribute15, fixed = TRUE)) %>%
    mutate(is_multi = stringr::str_count(units, ",")) %>%
    filter(is_multi > 0) %>%
    select(ugKthid, units) %>%
    tidyr::separate_rows(units, sep = ",") %>%
    mutate(u1 = stringr::str_extract(units, "([:alnum:]{3,})"))

  # counts by unit
  # b %>%
  #   group_by(unit) %>% tally() %>% arrange(desc(n)) %>% View()

  fulljoin <-
    b %>% full_join(a %>% select(-dn), by = "ugKthid")

  list(
    kthid_with_unit = fulljoin,
    orcid_kthid_pairs = orcid_kthid_pairs,
    kthid_many_orgs = many_orgs
  )

}

ldap_whoami <- function(ldap_config) {

  cmd <- ldap_cmd_whoami(ldap_config)
  system(cmd, intern = TRUE)
}

ad_search_kthid <- function(kthid) {

  query <- sprintf("(&(ugKthid=%s)(ugUsername=*))", kthid)

  ldap_search(query, cfg = ldap_config(), ldap_attributes = c(
    "ugKthid", "ugUsername"
  ))

}

#ad_search_kthid("u10*")

ad_search_accountname <- function(accountname) {

  query <- sprintf("(&(sAMAccountName=%s)(ugKthid=*))", accountname)

  ldap_search(query, cfg = ldap_config(), ldap_attributes = c(
    "ugKthid", "sAMAccountName"
  ))

}

# ad_search_accountname("marku*")

ad_lookup_dn <- function(kthid) {

  query <- sprintf("(&(ugKthid=%s)(ugUsername=*))", kthid)

  ldap_search(query, cfg = ldap_config(), ldap_attributes = c(
    "ugKthid", "ugUsername", "displayName"
  ))

}

# ad_lookup_dn("u1z88syr")
