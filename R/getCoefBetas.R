getCoefBetas <- function(alpha, normalize=FALSE, digits=NULL) {
  if (normalize == TRUE) alpha$effectSize <- alpha$coef / alpha$se.coef
  else alpha$effectSize <- alpha$coef
  if (!is.null(digits)) alpha$effectSize <- round(alpha$effectSize, digits)
  sign(alpha$effectSize) / nrow(alpha)
}
