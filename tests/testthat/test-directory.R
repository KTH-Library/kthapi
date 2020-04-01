test_that("query for slugs works", {
  slugs <- kth_directory(path = "root")$content$slug
  expect_gt(length(slugs), 5)
})

test_that("query for root lookup works", {
  lookup <- kth_directory(path = "root")$content

  t1 <- bind_cols(with(lookup, list(
    slug, parent_id, imported, kthid, location,
    fax, phone, website, version, `description.en`
  )))

  expect_gt(nrow(t1), 7)

})

test_that("query for KTH schools and departments catalog works", {
    api_slugs <- kth_school_dep()$slug
    valid_slugs <- unlist(strsplit("acjmst", ""))
    is_valid <- all(valid_slugs %in% api_slugs)
    expect_true(is_valid)
})

test_that("query for catalog from slug works", {

    c <-
    GET(sprintf("%s/catalog/%s/*", config()$url_directory, "a"),
        query = list(slug = "a", l = "sv"),
        add_headers(api_key = config()$api_key))

  cc <- httr::content(c)
  cc$catalogs

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

