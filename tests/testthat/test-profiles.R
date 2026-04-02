# Set to FALSE to test with testthat, set to TRUE before pushing to Github
skip_api_tests <- TRUE

test_that("Legacy Profiles API returns data", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  profile <- kth_profile_legacy("tjep")
  api_email <- profile$content$email

  expect_equal(api_email, "tjep@kth.se")
})


test_that("Legacy Profiles API returns LDAP equiv of 'displayName'", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  displayName <- kth_displayname("u1z88syr")
  expect_equal(displayName, "Agne Larsson (agnel)")

})

test_that("Legacy Profiles API returns organizational belonging for kthid", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  unit_code <- kth_profile_school_dep("u1z88syr")
  is_valid <- "tr" == unit_code[2]
  expect_true(is_valid)

})

test_that("Legacy Profiles API returns organizational belonging for accountid", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  unit_code <- kth_profile_school_dep("tjep")
  is_valid <- "tr" == unit_code[2]
  expect_true(is_valid)

})

test_that("Legacy Profiles API returns institutional belonging for accountid and kthid", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  t1 <- kth_belonging_institutional("tjep")
  t2 <- kth_belonging_institutional("u1z88syr")
  t3 <- try(kth_belonging_institutional("hoyce"), silent = TRUE)

  is_valid <- (t1$desc == "KTH Library" && t2$slug == "t/tr" && inherits(t3, "try-error"))

  expect_true(is_valid)

})

test_that("Profiles API returns organizational belonging for a specific kthid", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  p1 <- kth_profile(kthid = "u1z88syr")$content$worksFor$items

  path_worksFor <- function(x) {
    x |> 
    gsub(pattern = "app\\.katalog3\\.", replacement = "") |> 
    tolower() |> 
    gsub(pattern = "\\.", replacement = "/")
  }

  # $path is gone now (?)
  #is_valid <- rev(p1$path)[1] == "t/tr/trac"
  is_valid <- 
    rev(p1$key)[1] |> path_worksFor() == "t/tr/trac"

  expect_true(is_valid)
})

# test_that("Profiles API returns kthid given accountname", {
#
#   skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")
#
#   p1 <- kth_profile(username = "markussk")$content$username
#   is_valid <- p1 == "markussk"
#   expect_true(is_valid)
# })
