#' @param n Number of observations.
#' @param K Number of folds.
#' @param seed Optional seed for reproducibility.
#'
#' @return Integer vector of fold IDs.
#' @keywords internal
make_folds <- function(n, K = 5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  sample(rep(seq_len(K), length.out = n))
}

#' @param data Data frame.
#' @param outcome Outcome column name.
#' @param exposure Exposure column name.
#' @param covars Optional covariate names. If NULL, uses everything except
#'        outcome and exposure.
#' @param K Number of CV folds.
#' @param seed Seed for fold generation.
#' @param family `"gaussian"` or `"binomial"`.
#' @param s `"lambda.min"` or `"lambda.1se"`.
#' @param maxit Max iterations passed to glmnet.
#' @param thresh Convergence threshold.
#' @param standardize.response Logical, passed to glmnet.
#'
#' @return Output from `penalized_select()`.
#' @export
cv_select <- function(data, outcome, exposure, covars = NULL, K = 5, seed = 1,
                      family = c("gaussian", "binomial"),
                      s = c("lambda.min", "lambda.1se"),
                      maxit = if (match.arg(family) == "binomial") 1000000 else 100000,
                      thresh = 1e-07,
                      standardize.response = FALSE) {
  family <- match.arg(family)
  s <- match.arg(s)
  if (is.null(covars)) covars <- setdiff(names(data), c(outcome, exposure))
  n <- nrow(data)
  foldid <- make_folds(n = n, K = K, seed = seed)
  penalized_select(
    X = data[covars],
    A = data[[exposure]],
    Y = data[[outcome]],
    family = family,
    foldid = foldid,
    s = s,
    maxit = maxit,
    thresh = thresh,
    standardize.response = standardize.response
  )
}
