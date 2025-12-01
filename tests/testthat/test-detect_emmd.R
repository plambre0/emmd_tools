library(testthat)

test_that("detect_emm runs on simulated gaussian data", {
  set.seed(1)
  dat <- simulate_emm(n = 100, p = 20, k = 2, family = "gaussian")
  out <- detect_emm(dat, outcome = "Y", exposure = "A", family = "gaussian")
  expect_s3_class(out, "emmd_result")
  expect_true(is.list(out$estimates))
})

test_that("detect_emm runs on simulated binomial data", {
  set.seed(2)
  dat <- simulate_emm(n = 200, p = 30, k = 3, family = "binomial")
  out <- detect_emm(dat, outcome = "Y", exposure = "A", family = "binomial")
  expect_s3_class(out, "emmd_result")
  expect_true(is.list(out$estimates))
})

test_that("penalized_select respects lambda choice", {
  set.seed(3)
  dat <- simulate_emm(n = 150, p = 25, k = 2, family = "gaussian")

  # Use the new argument name: lambda_type
  out_min <- detect_emm(dat, outcome = "Y", exposure = "A",
                        family = "gaussian", lambda_type = "lambda.min")
  out_1se <- detect_emm(dat, outcome = "Y", exposure = "A",
                        family = "gaussian", lambda_type = "lambda.1se")

  expect_s3_class(out_min, "emmd_result")
  expect_s3_class(out_1se, "emmd_result")
})
