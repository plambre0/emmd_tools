#' @param n Number of observations.
#' @param p Number of covariates.
#' @param k Number of true effect modifiers.
#' @param family `"gaussian"` or `"binomial"`.
#'
#' @return A data frame containing Y, A, and covariates,
#'         with the true modifiers stored as an attribute.
#' @export
simulate_emm <- function(n = 200, p = 50, k = 3, family = "gaussian") {
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("V", seq_len(p))
  A <- rbinom(n, 1, 0.5)
  beta0 <- 0.2
  beta_A <- 0.5
  int_beta <- rep(0, p)
  true_idx <- sample.int(p, k)
  int_beta[true_idx] <- runif(k, 0.5, 1.2)

  linpred <- beta0 + beta_A * A + as.vector((A * X) %*% int_beta)

  if (family == "gaussian") {
    Y <- linpred + rnorm(n, sd = 1)
  } else {
    prob <- 1 / (1 + exp(-linpred))
    Y <- rbinom(n, 1, prob)
  }
  df <- data.frame(Y = Y, A = A)
  df <- cbind(df, as.data.frame(X))
  attr(df, "true_modifiers") <- colnames(X)[true_idx]
  df
}
