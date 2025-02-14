test_that("ldap search works - ug / orcid / kthid / unit", {
  skip_on_ci()

  search <- ug_orcid_kthid_unit()

  empties <- 
    search$kthid_with_unit |> 
    filter(displayName == "")

  is_valid <- length(empties$kthid) > 1000
  expect_true(is_valid)

})

test_that("search for specific and wildcard attributes works", {
  skip_on_ci()

  my_cfg <- ldap_config()
  my_ldap_query <- "(&(ugKthid=u1z88syr)(ugUsername=*)(ugOrcid=*))"
  
  my_ldap_attributes <- c("ugKthid", "ugOrcid", "kthOrcid")
  s1 <- ldap_search(my_ldap_query, my_ldap_attributes, my_cfg)
  
  my_ldap_attributes <- c("*")
  s2 <- ldap_search(my_ldap_query, my_ldap_attributes, my_cfg)
  
  is_valid <- 
    ncol(s1) == 4 && nrow(s1) == 1 && 
    ncol(s2) > 100 && nrow(s2) == 1
  
  expect_true(is_valid)

})


test_that("ldap search works - 'star'-search for all attribs - several kthids", {
  skip_on_ci()

  search <- ug_orcid_kthid_unit()

  anons <- 
    search$kthid_with_unit |> 
    filter(displayName == "")

  ad_search_kthid_star <- function(kthid) {
    query <- sprintf("(&(ugKthid=%s))", kthid)
    ldap_search(query, cfg = ldap_config(), ldap_attributes = "*") |> 
      suppressMessages()
  }

  search_fn <- purrr::possibly(ad_search_kthid_star)

  res <- 
    anons$kthid |> head(10) |> 
    purrr::map_dfr(search_fn, .progress = TRUE)

  lookup <- 
    anons |> head(10) |> inner_join(res, by = "kthid") |> 
    select(contains("cid"), contains("hid"))

  is_valid <- length(lookup$kthid) == 10
  expect_true(is_valid)

})