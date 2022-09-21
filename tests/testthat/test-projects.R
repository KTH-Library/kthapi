
# test_that("Projects API returns data given accountname", {
#   skip_on_ci()
#   p1 <- kth_projects(path = "projects/username/", kthUserName = "stemme")
#   is_valid <- nrow(p1$content$people) > 1
#   expect_true(is_valid)
# })

# test_that("Projects API returns data for all public projects", {
#   skip_on_ci()
#   p1 <- kth_projects("projects/public")
#   is_valid <- p1$content$pagination$lastPage > 50
#   expect_true(is_valid)
# })
#
# test_that("Projects API returns data when given orcid", {
#   skip_on_ci()
#   p1 <- kth_projects("projects/orcid/", orcid = "0000-0003-2983-5573")
#   is_valid <- length(p1$content$`_id`) >= 4
#   expect_true(is_valid)
# })
#
# test_that("Projects API returns data when given tag", {
#   skip_on_ci()
#   p1 <- kth_projects("projects/tag/", tag = "10209")
#   is_valid <- nrow(p1$content$projects) >= 2
#   expect_true(is_valid)
# })
#
# test_that("Projects API returns people duplicates", {
#   skip_on_ci()
#   p1 <- kth_projects_people_duplicates()
#   is_valid <- nrow(p1) >= 2
#   expect_true(is_valid)
# })
