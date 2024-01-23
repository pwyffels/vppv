#' Making a GAM model
#'
#' @param Dat Dat is a VPPV data frame that contains the following columns : time,RR0, RR1, timing and PP
#'
#' @return mgcv model
#' @export
#' @importFrom stats resid
#' @importFrom mgcv gam
#' @examples
#' ## using the right formal for the data
#' b <- afib
#' colnames(b) <- c('TIME', 'rr0','rr1','pp')
#' ## assuming a MVR of 12 cycles per minute
#' dat <- VPPV_frame(b, TIME,rr0,rr1,pp, 12)
#' model_gam <- VPPV_gam(dat)
#' summary(model_gam)
VPPV_gam <- function(Dat) {
  #data<- Dat
  #data<-data[-(1:2),]
  Dat$RR0cc<-resid(mgcv::gam(RR0~s(timing),data=Dat))
  Dat$RR1cc<-resid(mgcv::gam(RR1~s(timing),data=Dat))

  mod<-mgcv::gam(PP~s(RR0cc,bs='cs')+s(RR1cc,bs='cs')+s(timing, bs='cc',k=4)+s(time,k=5),data=Dat, method='REML',select=T)
  mod
}
