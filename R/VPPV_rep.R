#' Replicates of VPPV_value using a Metropolis Hastings sample
#'
#' @param model_gam A mgcv::gam model resulting from the VPPV_gam function
#'
#' @return a data frame with 1000 values of VPPV based on resampling the coeficients of the model a 1000 times
#' @export
#' @importFrom mgcv gam.mh PredictMat
#'
#' @examples
#' dat = VPPV_frame(afib, time, RR0,RR1, PP,12)
#' model_gam <- VPPV_gam(dat)
#' VPPV_value(model_gam)
#' dist_gam <- VPPV_rep(model_gam)
#' quantile(dist_gam, c(0.027,0.5,0.975),na.rm=TRUE)
#' plot(density(dist_gam$VPPV))
VPPV_rep <- function(model_gam){
  br <- gam.mh(model_gam, thin=2, ns=2000, rw.scale=0.25)$bs
  smooth <- model_gam$smooth[[3]]
  x = seq(0,1,length=100)
  range = data.frame(x=x, by=NA); names(range) = c(smooth$term, smooth$by)

  mat = PredictMat(smooth, range)
  par = smooth$first.para:smooth$last.para

  VPPVrep = data.frame(VPPV=rep(0,1000))
  for (i in (1:1000)){
    #y = mat %*% br[i,par]
    #VPPV = 100*diff(range(y)/br[i,1])
    VPPVrep[i,1]= 100*diff(range(mat %*% br[i,par]))/br[[i,1]]
    #VPPVrep[[i]]=  VPPV
  }
  VPPVrep
}
