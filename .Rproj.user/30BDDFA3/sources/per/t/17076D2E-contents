#' @param data Data frame.
#' @param outcome Outcome variable name.
#' @param exposure Exposure variable name.
#' @param covars Covariate names (optional).
#'
#' @keywords internal
check_inputs <- function(data, outcome, exposure, covars) {
  stopifnot(is.data.frame(data))
  if (!outcome %in% names(data)) stop("outcome not found in data")
  if (!exposure %in% names(data)) stop("exposure not found in data")
  if (!is.null(covars)) {
    missing <- setdiff(covars, names(data))
    if (length(missing) > 0) stop("covariates not found: ", paste(missing, collapse = ", "))
  }
}

#' @param data Data frame.
#' @param outcome Outcome variable.
#' @param exposure Exposure variable.
#' @param modifier Modifier variable.
#' @param family `"gaussian"` or `"binomial"`.
#'
#' @return A list containing the model fit and the interaction coefficient row.
#' @keywords internal
reestimate_for_modifier <- function(data, outcome, exposure, modifier, family) {
  fml <- as.formula(paste0(outcome, " ~ ", exposure, " + ", modifier, " + ", exposure, ":", modifier))
  glm_family <- if (family == "gaussian") stats::gaussian() else stats::binomial()
  fit <- stats::glm(fml, data = data, family = glm_family)
  summ <- summary(fit)$coefficients
  int_name <- paste0(exposure, ":", modifier)
  if (!int_name %in% rownames(summ)) int_name <- paste0(modifier, ":", exposure)
  if (!int_name %in% rownames(summ)) stop("Interaction term not found in summary for modifier: ", modifier)
  est <- summ[int_name, , drop = FALSE]
  list(fit = fit, interaction = est)
}

#' @param data Data frame.
#' @param outcome Name of the outcome variable.
#' @param exposure Name of the exposure variable.
#' @param covars Optional covariate names.
#' @param family Model family (`"gaussian"` or `"binomial"`).
#' @param selector Function used for the selection step.
#' @param alpha Significance level (used only for printing).
#' @param lambda_type `"lambda.min"` or `"lambda.1se"`.
#' @param maxit Max glmnet iterations.
#' @param thresh Convergence tolerance.
#' @param standardize.response Logical, passed to glmnet.
#'
#' @return An object of class `emmd_result`.
#' @export
detect_emm <- function(data, outcome, exposure, covars = NULL,
                       family = c("gaussian", "binomial"),
                       selector = penalized_select,
                       alpha = 0.05,
                       lambda_type = c("lambda.min", "lambda.1se"),
                       maxit = if (match.arg(family) == "binomial") 1e6 else 1e5,
                       thresh = 1e-07,
                       standardize.response = FALSE) {

  family <- match.arg(family)
  lambda_type <- match.arg(lambda_type)
  check_inputs(data, outcome, exposure, covars)
  if (is.null(covars)) covars <- setdiff(names(data), c(outcome, exposure))

  Y <- data[[outcome]]
  A <- data[[exposure]]
  X <- data[covars]

  # For binomial, default to lambda.1se if lambda.min is requested
  if (family == "binomial" && lambda_type == "lambda.min") lambda_type <- "lambda.1se"

  sel_res <- selector(
    X = X,
    A = A,
    Y = Y,
    family = family,
    lambda_type = lambda_type,
    maxit = maxit,
    thresh = thresh,
    standardize.response = standardize.response
  )

  selected <- sel_res$selected
  estimates <- list()
  raw_pvals <- numeric()

  if (length(selected) > 0) {
    for (v in selected) {
      re <- tryCatch(
        reestimate_for_modifier(
          data = data,
          outcome = outcome,
          exposure = exposure,
          modifier = v,
          family = family
        ),
        error = function(e) e
      )
      if (inherits(re, "error")) next
      estimates[[v]] <- re$interaction
      pcol <- if (family == "gaussian") "Pr(>|t|)" else "Pr(>|z|)"
      raw_pvals[v] <- if (pcol %in% colnames(re$interaction)) {
        as.numeric(re$interaction[, pcol])
      } else {
        NA_real_
      }
    }
  }

  p_adj <- if (length(raw_pvals) > 0) stats::p.adjust(raw_pvals, method = "BH") else numeric()

  out <- list(
    selected   = selected,
    estimates  = estimates,
    raw_pvals  = raw_pvals,
    p_adjusted = p_adj,
    selector   = sel_res,
    call       = match.call(),
    alpha      = alpha,
    family     = family
  )
  class(out) <- "emmd_result"
  out
}

#' @param x An `emmd_result` object.
#' @param ... Ignored.
#'
#' @export
print.emmd_result <- function(x, ...) {
  cat("emmd result\n")
  cat("Selected modifiers (count =", length(x$selected), "):\n")
  if (length(x$selected) == 0) {
    cat("  (none)\n")
  } else {
    for (v in x$selected) {
      p <- if (!is.null(x$p_adjusted[v])) {
        formatC(x$p_adjusted[v], digits = 3, format = "f")
      } else {
        "NA"
      }
      est_row <- x$estimates[[v]]
      est <- if (!is.null(est_row)) {
        formatC(est_row[1, "Estimate"], digits = 3, format = "f")
      } else {
        "NA"
      }
      cat(sprintf("  %s — est = %s, adj.p = %s\n", v, est, p))
    }
  }
}
