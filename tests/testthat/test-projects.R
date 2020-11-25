test_that("Projects API returns data given accountname", {
  p1 <- kth_projects(path = "projects/kthUserName/", kthUserName = "stemme")
  is_valid <- nrow(p1$content$people) > 1
  expect_true(is_valid)
})

test_that("Projects API returns data for all public projects", {
  p1 <- kth_projects()
  is_valid <- p1$content$pagination$lastPage > 200
  expect_true(is_valid)
})

#test_that("Projects API returns data when given orcid", {
#  p1 <- kth_projects("projects/orcid/", orcid = "0000-0003-2983-5573")
#  is_valid <- length(p1$content$`_id`) >= 4
#  expect_true(is_valid)
#})

test_that("Projects API returns data when given tag", {
  p1 <- kth_projects("projects/tag/10209")
  is_valid <- nrow(p1$content$projects) >= 2
  expect_true(is_valid)
})
