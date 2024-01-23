#' Ventilation induced Pulse Pressure Variation point estimate
#'
#' @param model_gam a mgcv::gam model build with the VPPV_gam function.
#'
#' @return value expressed in percent
#' @export
#' @importFrom mgcv PredictMat
#' @importFrom stats coef
#'
#' @examples
#' b <- afib
#' colnames(b) <- c('TIME', 'rr0','rr1','pp')
#' ## assuming a MVR of 12 cycles per minute
#' dat <- VPPV_frame(b, TIME,rr0,rr1,pp, 12)
#' model_gam <- VPPV_gam(dat)
#' VPPV_value(model_gam)

VPPV_value <- function( model_gam){
  smooth <- model_gam$smooth[[3]]
  x = seq(0,1,length=100)
  range = data.frame(x=x, by=NA); names(range) = c(smooth$term, smooth$by)

  mat = PredictMat(smooth, range)
  par = smooth$first.para:smooth$last.para
  y = mat %*% model_gam$coefficients[par]

  VPPV = 100*diff(range(y))/coef(model_gam)[[1]]
  VPPV
}
