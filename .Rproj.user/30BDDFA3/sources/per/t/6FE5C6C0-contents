library(testthat)
library(emmd)

set.seed(123)

test_that("detect_emm runs on Gaussian and binomial outcomes", {
  n <- 200
  p <- 20
  k <- 3

  # Simulate Gaussian data
  X <- matrix(rnorm(n * p), nrow = n)
  colnames(X) <- paste0("V", 1:p)
  A <- rbinom(n, 1, 0.5)
  true_mods <- c("V3", "V7", "V12")
  beta0 <- 0.2
  beta_A <- 0.5
  beta_int <- rep(0, p); names(beta_int) <- colnames(X)
  beta_int[true_mods] <- c(1.2, 0.8, -1)
  linpred <- beta0 + beta_A * A + rowSums(sweep(X, 2, beta_int, "*") * A)
  Y <- linpred + rnorm(n)
  df <- data.frame(Y = Y, A = A, X)
  attr(df, "true_modifiers") <- true_mods

  # Gaussian: lambda.min
  res_min <- detect_emm(df, outcome = "Y", exposure = "A", lambda_type = "lambda.min")
  expect_s3_class(res_min, "emmd_result")
  expect_true(length(res_min$selected) >= length(true_mods))

  # Gaussian: lambda.1se
  res_1se <- detect_emm(df, outcome = "Y", exposure = "A", lambda_type = "lambda.1se")
  expect_s3_class(res_1se, "emmd_result")
  expect_true(all(true_mods %in% res_1se$selected)) # True mods always included
  expect_true(length(res_1se$selected) <= length(res_min$selected)) # More conservative

  # Check estimates dimensions
  for (mod in res_1se$selected) {
    est <- res_1se$estimates[[mod]]
    expect_true(all(c("Estimate", "Std. Error") %in% colnames(est)))
  }
})

test_that("detect_emm works for binomial outcome", {
  n <- 200
  p <- 15
  k <- 2

  X <- matrix(rnorm(n * p), nrow = n)
  colnames(X) <- paste0("V", 1:p)
  A <- rbinom(n, 1, 0.5)
  true_mods <- c("V2", "V5")
  beta0 <- -0.2
  beta_A <- 0.7
  beta_int <- rep(0, p); names(beta_int) <- colnames(X)
  beta_int[true_mods] <- c(1, -1.5)
  linpred <- beta0 + beta_A * A + rowSums(sweep(X, 2, beta_int, "*") * A)
  prob <- 1 / (1 + exp(-linpred))
  Y <- rbinom(n, 1, prob)
  df <- data.frame(Y = Y, A = A, X)
  attr(df, "true_modifiers") <- true_mods

  res_bin <- detect_emm(df, outcome = "Y", exposure = "A", lambda_type = "lambda.1se", family = "binomial")
  expect_s3_class(res_bin, "emmd_result")
  expect_true(all(true_mods %in% res_bin$selected))
})

test_that("plot_emm runs without errors", {
  n <- 100; p <- 10; k <- 2
  dat <- simulate_emm(n, p, k, family = "gaussian")
  res <- detect_emm(dat, outcome = "Y", exposure = "A", lambda_type = "lambda.1se")
  expect_error(plot_emm(res), NA) # should not error
})

test_that("lambda.1se controls false positives", {
  set.seed(456)
  n <- 200
  p <- 20
  k <- 3

  # Generate covariates and exposure
  X <- matrix(rnorm(n * p), nrow = n)
  colnames(X) <- paste0("V", 1:p)
  A <- rbinom(n, 1, 0.5)

  # True effect modifiers
  true_mods <- c("V4", "V10")
  beta0 <- 0
  beta_A <- 0.5
  beta_int <- rep(0, p); names(beta_int) <- colnames(X)
  beta_int[true_mods] <- c(1.2, -1.0)

  # Outcome: Gaussian
  linpred <- beta0 + beta_A * A + rowSums(sweep(X, 2, beta_int, "*") * A)
  Y <- linpred + rnorm(n)
  df <- data.frame(Y = Y, A = A, X)
  attr(df, "true_modifiers") <- true_mods

  # Run detect_emm with lambda.1se
  res <- detect_emm(df, outcome = "Y", exposure = "A", lambda_type = "lambda.1se")

  # All true modifiers must be detected
  expect_true(all(true_mods %in% res$selected))

  # Count false positives
  false_positives <- setdiff(res$selected, true_mods)
  cat("False positives detected:", length(false_positives), "\n")

  # Expect very few (ideally 0–1) false positives under lambda.1se
  expect_lte(length(false_positives), 1)
})
