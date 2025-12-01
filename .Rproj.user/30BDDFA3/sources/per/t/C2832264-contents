#' @param x A vector, matrix, or data frame.
#'
#' @return A numeric matrix with centered and scaled columns.
#' @keywords internal
safe_scale <- function(x) {
  if (is.data.frame(x)) {
    mat <- as.matrix(x)
  } else if (is.matrix(x)) {
    mat <- x
  } else {
    mat <- matrix(as.numeric(x), ncol = 1)
    cn <- tryCatch(deparse(substitute(x)), error = function(e) NULL)
    if (!is.null(cn)) colnames(mat) <- cn
  }
  means <- colMeans(mat, na.rm = TRUE)
  sds <- apply(mat, 2, sd, na.rm = TRUE)
  sds[is.na(sds) | sds == 0] <- 1
  centered <- sweep(mat, 2, means, "-")
  scaled <- sweep(centered, 2, sds, "/")
  colnames(scaled) <- colnames(mat)
  scaled
}

#' @param A Numeric exposure vector.
#' @param X Covariate matrix or data frame.
#' @param prefix Prefix for generated interaction variable names.
#'
#' @return A matrix whose columns are A multiplied by each column of X.
#' @keywords internal
int_matrix <- function(A, X, prefix = "int_") {
  X <- as.matrix(X)
  if (length(A) != nrow(X)) stop("length(A) must equal nrow(X)")
  A <- as.numeric(A)
  mat <- sweep(X, 1, A, FUN = "*")
  colnames(mat) <- paste0(prefix, colnames(X))
  mat
}

#' @param X Covariate matrix or data frame.
#' @param A Exposure vector.
#' @param Y Outcome vector.
#' @param family Model family: `"gaussian"` or `"binomial"`.
#' @param foldid Optional fold IDs for cross-validation.
#' @param lambda_type Whether to use `"lambda.min"` or `"lambda.1se"`.
#' @param maxit Maximum glmnet iterations.
#' @param thresh Convergence threshold.
#' @param standardize.response Whether glmnet standardizes the response.
#'
#' @return A list with selected modifiers, the cv.glmnet object, and
#'         the coefficients at the chosen lambda.
#' @export
penalized_select <- function(X, A, Y, family = "gaussian", foldid = NULL,
                             lambda_type = c("lambda.min", "lambda.1se"),
                             maxit = if (family == "binomial") 1000000 else 100000,
                             thresh = 1e-07,
                             standardize.response = FALSE) {

  lambda_type <- match.arg(lambda_type)
  intX <- int_matrix(A = A, X = X, prefix = "int_")
  intX <- safe_scale(intX)

  # Fit cross-validated glmnet
  cvfit <- glmnet::cv.glmnet(
    x = intX,
    y = Y,
    family = family,
    foldid = foldid,
    maxit = maxit,
    thresh = thresh,
    standardize.response = standardize.response
  )

  # Select coefficients according to lambda_type
  lambda <- if (lambda_type == "lambda.min") cvfit$lambda.min else cvfit$lambda.1se
  coefs <- as.matrix(stats::coef(cvfit, s = lambda))

  # Identify nonzero coefficients
  nz <- rownames(coefs)[which(coefs != 0)]
  nz <- nz[!nz %in% "(Intercept)"]
  selected <- sub("^int_", "", nz)

  list(selected = unique(selected), cvfit = cvfit, coef = coefs)
}
