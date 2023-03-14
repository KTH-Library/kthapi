# Set to FALSE to test with testthat, set to TRUE before pushing to Github
skip_api_tests <- TRUE

test_that("query for root slugs works", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  slugs <- kth_root(path = "root")$content$slug
  expect_gt(length(slugs), 5)
})

test_that("query for root works", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  lookup <- kth_root(path = "root")$content

  # t1 <- bind_cols(with(lookup, list(
  #   slug, parent_id, imported, kthid, location,
  #   fax, phone, website, version, `description.en`
  # )))

  expect_equal(nrow(lookup), 6)

})

test_that("query for KTH schools and departments catalog works", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  api_slugs <- kth_school_dep()$slug
  valid_slugs <- unlist(strsplit("acjmst", ""))
  is_valid <- all(valid_slugs %in% api_slugs)
  expect_true(is_valid)
})

test_that("query for catalog from slug 'a/af' works", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  af <- kth_catalog(slug = "a/af", lang = "en")
  n_users <- nrow(af$users)
  n_catalogs <- nrow(af$catalogs)
  is_valid_parent <- af$parent$slug == "a"

  is_valid <- n_users > 100 && n_catalogs > 5 && is_valid_parent
  expect_true(is_valid)
})


test_that("current bundled data 'abm_units' matches w Directory API results for schools and institutions", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  # get current data on schools and institutions from the Directory API
  kds <- kth_school_dep()

  # find mismatch through an antijoin
  non_matches <-
    abm_units[-c(1),] %>%
    anti_join(kth_school_dep(), by = "slug")

  is_mismatched <- (nrow(non_matches) > 0)

  if (is_mismatched) {
    message("Found inconsistencies between current abm_units and the Directory API:")
    print(non_matches)
  }

  expect_false(is_mismatched)

  # TODO: make this a better test
})

#a$info %>% purrr::map_df(function(x) as.data.frame(x) %>% flatten %>% as_tibble)
#a$parents %>% purrr::map_df(function(x) as.data.frame(x) %>% flatten %>% as_tibble)
#a$catalogs %>% purrr::map_df(function(x) as.data.frame(x) %>% flatten %>% as_tibble) #as_tibble(.name_repair = "universal") %>% flatten)
#a$users %>% purrr::map_df(function(x) as.data.frame(x) %>% flatten %>% as_tibble)

# UG: seedar alla andra system med Profiles-data.
#
# LDAP kthPAGroupMembership - kan plocka ut det sista steget, men vi har inte de trevliga namnen
#
# I Mac-fallet importeras namnen...
#
# De två första av fyra är skola och institution, sedan kommer avdelning
