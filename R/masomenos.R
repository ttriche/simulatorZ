masomenos <- function(X, y, normalize=FALSE, digits=NULL, ...) {  
  X <- data.frame(X, row.names=NULL)
  if (!is(y, 'Surv')) y <- Surv(y[, 1], y[, 2])
  alpha <- rowCoxTests(t(X), y, ...) ## design?
  getCoefBetas(alpha, normalize=normalize, digits=digits)
}
