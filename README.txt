This is, emmd, an R package I designed for the detection and estimation of effects modifiers for high-dimensional data where the number of variables is relatively large compared to sample size. This was designed with epidemiological and biological data in mind. 
emmd identifies and estimates effect modifiers in high-dimensional data by first using LASSO-penalized regression to detect interactions between an exposure and covariates, then re-fitting generalized linear models on the selected modifiers to produce interpretable effect estimates, standard errors, and multiple-testing-adjusted p-values, with visualization of interaction effects.

Sources Used for Methodology
Tibshirani, R. (1996). Regression shrinkage and selection via the LASSO. Journal of the Royal Statistical Society: Series B, 58(1), 267–288.
Friedman, J., Hastie, T., & Tibshirani, R. (2010). Regularization Paths for Generalized Linear Models via Coordinate Descent. Journal of Statistical Software, 33(1), 1–22.
Hastie, T., Tibshirani, R., & Friedman, J. (2009). The Elements of Statistical Learning. Springer.
Benjamini, Y., & Hochberg, Y. (1995). Controlling the False Discovery Rate: A Practical and Powerful Approach to Multiple Testing. Journal of the Royal Statistical Society, Series B, 57(1), 289–300.
Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6(2), 65–70.
McCullagh, P., & Nelder, J. A. (1989). Generalized Linear Models. 2nd Edition. Chapman & Hall.

Libraries Used
Friedman, J., Hastie, T., & Tibshirani, R. (2022). glmnet: Lasso and Elastic-Net Regularized Generalized Linear Models. R package version 4.1-7.
Wickham, H. et al. (2023). testthat: Unit Testing for R. R package version 3.2.5.
Wickham, H., & Hester, J. (2023). devtools: Tools to Make Developing R Packages Easier. R package version 3.6.3.
