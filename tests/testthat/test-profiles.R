# Set to FALSE to test with testthat, set to TRUE before pushing to Github
skip_api_tests <- TRUE

test_that("Legacy Profiles API returns data", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  api_email <- kth_profile_legacy("hoyce")$content$email
  expect_equal(api_email, "hoyce@kth.se")
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

  unit_code <- kth_profile_school_dep("hoyce")
  is_valid <- "tj" == unit_code[2]
  expect_true(is_valid)

})

test_that("Legacy Profiles API returns institutional belonging for accountid and kthid", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  t1 <- kth_belonging_institutional("hoyce")
  t2 <- kth_belonging_institutional("u1z88syr")
  t3 <- tryCatch(kth_belonging_institutional("markussk"), error = function(e) e)

  is_valid <- (t1$desc == "IT" && t2$slug == "t/tr" && t3$message == "The API returned an error: Not found")

  expect_true(is_valid)

})

test_that("Profiles API returns organizational belonging for a specific kthid", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  p1 <- kth_profile(kthid = "u1rh0c9q")$content$worksFor$items
  is_valid <- rev(p1$path)[1] == "t/tj/tjd/tjda"
  expect_true(is_valid)
})

test_that("Profiles API returns kthid given accountname", {

  skip_if(skip_api_tests, "skipping tests that need authentication in case we're in the cloud")

  p1 <- kth_profile(username = "markussk")$content$kthId
  is_valid <- p1 == "u1o2ujjd"
  expect_true(is_valid)
})
