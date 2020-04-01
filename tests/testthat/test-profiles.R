test_that("Legacy Profiles API returns data", {
  api_email <- kth_profile_legacy("hoyce")$content$email
  expect_equal(api_email, "hoyce@kth.se")
})


test_that("Legacy Profiles API returns LDAP equiv of 'displayName'", {

  displayName <- kth_displayname("u1z88syr")
  expect_equal(displayName, "Agne Larsson (agnel)")

})

test_that("Legacy Profiles API returns organizational belonging for kthid", {

  unit_code <- kth_profile_school_dep("u1z88syr")
  is_valid <- "tr" == unit_code[2]
  expect_true(is_valid)

})

test_that("Legacy Profiles API returns organizational belonging for accountid", {

  unit_code <- kth_profile_school_dep("hoyce")
  is_valid <- "tj" == unit_code[2]
  expect_true(is_valid)

})
