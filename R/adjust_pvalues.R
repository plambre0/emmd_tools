#' @param pvals Numeric vector of p-values.
#' @param method Either `"BH"` (default) or `"bonferroni"`.
#'
#' @return A vector of adjusted p-values.
#' @export
adjust_pvalues <- function(pvals, method = c("BH", "bonferroni")) {
  method <- match.arg(method)
  if (length(pvals) == 0) return(numeric())
  stats::p.adjust(pvals, method = method)
}
