test_that("Profiles API returns data", {
  api_email <- kth_profile_legacy("hoyce")$content$email
  expect_equal(api_email, "hoyce@kth.se")
})


test_that("Profiles API returns LDAP equiv of 'displayName'", {

  displayName <- kth_displayname("u1z88syr")
  expect_equal(displayName, "Agne Larsson (agnel)")

})

