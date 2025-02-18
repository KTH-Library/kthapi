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
    "%s -o ldif-wrap=no -LLL -E pr=2147483647/noprompt -H \"%s\" -x -D \"%s\" -w \"%s\" -b \"%s\" \"%s\" %s",
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

  data <- group <- n_dn <- NULL

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
  res <- grep("^\\s*#", res, value = TRUE, invert = TRUE)
  res <- res[nchar(res) > 0]

  remove_nuls <- function(msg) {
    #msg <- "iWNfusvBrUS/No9qgnroWQ=="
    msg |> base64enc::base64decode() |> as.character() |> 
      grep(pattern = "[^00]", value = TRUE) |> 
      purrr::map_chr(\(x) rawToChar(as.raw(strtoi(x, 16L)))) |> 
      paste(collapse = "") |>
      stringi::stri_enc_toutf8(is_unknown_8bit = TRUE) |> 
      #stringi::stri_escape_unicode()
      gsub(pattern = "[[:print:]]+", replacement = "")
  }

  # decode base64-encoded blobs in ldif text
  res <-
    textclean::fgsub(
      x = res,
      pattern = "::\\s+(.*?)$",
      fun = function(x) paste0(": ", remove_nuls(x))
    ) |> suppressWarnings()
  
  # convert to dataframe
  ldif <- res
  re <- "^(.*?):+\\s{1}(.*?)$"
  out <- grep(re, ldif, value = TRUE, perl = TRUE)
  key <- stringr::str_match(out, re)[ ,2]
  value <- stringr::str_match(out, re)[ ,3]
  fails <- c(which(is.na(key)), which(is.na(value))) |> unique()
  if (length(fails) > 0) {
    key <- key[-fails]
    value <- value[-fails]
  }

  a <- 
    tibble::tibble(key, value) |> 
    mutate(n_dn = as.integer(key == "dn")) |> 
    mutate(group = cumsum(n_dn)) |>
    group_by(group, key) |> 
    summarize(value = paste0(collapse = "|", value), .groups = "keep") |> 
    ungroup() |> 
    nest_by(group, key) |> 
    ungroup() |> 
    tidyr::unnest_wider(data) |> #tidyr::unnest_wider(value, names_sep = "_") |> 
    tidyr::pivot_wider(names_from = key, values_from = value) |> 
    ungroup()

  out <- 
    a |> select(-any_of(c("group")))
  # out <- 
  #   purrr::map2(a$key, a$data, function(k, v) tibble(v[1] |> unlist()) |> setNames(nm = k[1])) |> 
  #   bind_cols()

#   out <-
#     tibble::tibble(key, value) |> #nest_by(key) |> tidyr::unnest_wider(data) |> unnest(value)
# #    unnest(value)
# #    tidyr::pivot_longer() |> 
#     group_by(key) |> 
#     summarize(value = paste0(collapse = "|", value)) |> 
#     ungroup() |> 
#    tidyr::pivot_wider(names_from = key)

  #message("Current value for ldap attribs: ", ldap_attributes)

  if (is.null(ldap_attributes)) {
    ldap_attributes <- 
      setdiff(names(out), "dn")
  } else if (length(ldap_attributes) == 1 && ldap_attributes == "*") {
    ldap_attributes <- 
      names(out)
  }
  # if (length(ldap_attributes) == 1 && ldap_attributes == "*") {
  #   message("Current value for ldap attribs: ", ldap_attributes)
  #   ldap_attributes <- 
  #     ldif |> grep(pattern = "^([^#].*?)(:+\\s+.*?)$", value = TRUE) |> 
  #       gsub(pattern = "^(.*?)(:+\\s+.*?)$", replacement = "\\1") |> 
  #       unique()
  # }

  #message("Final value for ldap attribs: ", ldap_attributes)

  out

  # out |> 
  #   tidyr::unnest(cols = any_of(ldap_attributes))
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

  if (!is.null(attr(res, "status")) && attr(res, "status") == 254)
    stop("Cannot contact LDAP server")

  message("Parsing LDIF respone w ", length(res), " lines of data.")
  parse_ldif(res, ldap_attributes = ldap_attributes)
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

  ugKthid <- ugOrcid <- extensionAttribute15 <- is_multi <- dn <-
    username <- unit <- dn2 <- un2 <- category <- displayName <- 
      kthOrcidClaimed <- kth_orcid_claimed <- ugOrcidClaimed <- ug_orcid_claimed <-
    NULL

  a <-
    ldap_search(
      ldap_query = "(&(ugOrcid=*)(ugKthid=*))",
      ldap_attributes = c(
        "displayName", "ugUsername",
        "ugKthid", "ugOrcid",
        "ugPrimaryAffiliation", 
        "kthOrcidClaimed", "ugOrcidClaimed"
      )) |>
    select(dn, displayName, kthid = ugKthid,
           username = "ugUsername", category = "ugPrimaryAffiliation",
           orcid = "ugOrcid",  
           ug_orcid_claimed = ugOrcidClaimed, kth_orcid_claimed = kthOrcidClaimed)

  orcid_kthid_pairs <-
    a |>
    select(kthid, orcid) |>
    distinct()

  b <-
    ldap_search(
      ldap_query = "(&(ugKthid=*)(extensionAttribute15=*))",
      ldap_attributes = c(
        "ugKthid", "extensionAttribute15", "ugPrimaryAffiliation"
      )
    ) |>
    mutate(units = gsub("pa.anstallda.", "", extensionAttribute15, fixed = TRUE)) |>
    mutate(is_multi = stringr::str_count(units, ",")) |> 
    mutate(unit = stringr::str_extract(units, "([:alnum:]{3,})")) |>
    mutate(displayName = gsub("CN=(.*?)\\s[(](.*?)[)].*$", "\\1", dn)) |>
    mutate(username = gsub("CN=(.*?)\\s[(](.*?)[)].*$", "\\2", dn)) |>
    select(dn, kthid = "ugKthid", username, category = "ugPrimaryAffiliation",
           unit, units, is_multi)

  # people with multiple affiliated units

  many_orgs <-
    b |>
    filter(is_multi > 0) |> 
    select(kthid, units) |>
    tidyr::separate_rows(units, sep = ",") |>
    mutate(unit = stringr::str_extract(units, "([:alnum:]{3,})"))

  # counts by unit
  # b %>%
  #   group_by(unit) %>% tally() %>% arrange(desc(n)) %>% View()

  fulljoin <-
    b |> full_join(a |> select(-dn), by = c("kthid", "category", "username")) |>
    mutate(dn2 = gsub("CN=(.*?)\\s[(](.*?)[)].*$", "\\1", dn)) |>
    mutate(un2 = gsub("CN=(.*?)\\s[(](.*?)[)].*$", "\\2", dn)) |>
    mutate(displayName = ifelse(is.na(displayName), dn2, displayName)) |>
    mutate(username = ifelse(is.na(username), un2, username)) |>
    select(kthid, username, displayName, category, unit, units, is_multi, orcid, 
      ug_orcid_claimed, kth_orcid_claimed) |>
    arrange(orcid)

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

ad_search_kthid_wildcard <- function(kthid) {

  query <- sprintf("(&(ugKthid=%s)(ugUsername=*))", kthid)

  ldap_search(query, cfg = ldap_config(), ldap_attributes = c("*")
  ) |> 
  tidyr::pivot_longer(cols = everything())

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
