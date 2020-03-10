test_that("Profiles API returns data", {
  api_email <- kth_profile_legacy("hoyce")$content$email
  expect_equal(api_email, "hoyce@kth.se")
})
