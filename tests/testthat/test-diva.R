test_that("Fetching data from KTH DiVA portal works", {
  skip_if(TRUE == TRUE, "skipped due to long running operation")
  pubs <- kth_diva_pubs()
  n1 <- pubs %>% filter(PID == "931688")
  is_valid <- n1$PID == 931688
  expect_true(is_valid)
})
