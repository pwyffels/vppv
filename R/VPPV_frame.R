#' Making a VPPV data frame for further analysis
#'
#' @param data data.frame or tibble containing the raw data
#' @param time column name of the variable that contains the timestamps of the ECG-Rwave of each individual beat (in seconds)
#' @param RR0 column name of the variable that contains the length of the RR0 intervals (in seconds)
#' @param RR1 column name of the variable that contains the length of the RR1 intervals (in seconds)
#' @param PP  column name of the variable that contains the value of the Pulse Pressure of each individual beat in mmHg
#' @param MVR value of the mechanical ventilation rate expressed in cycles per minute
#'
#' @return dataset with correct column names and calculated relative timing of each individual beat for further analysis
#'     with the functions of the vppv-package.
#' @export
#' @importFrom dplyr rename mutate
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#'
#' @examples
#' b <- afib
#' colnames(b) <- c('TIME', 'rr0','rr1','pp')
#' ## assuming a MVR of 12 cycles per minute
#' VPPV_frame(b, TIME,rr0,rr1,pp, 12)

VPPV_frame <- function(data,time, RR0, RR1, PP, MVR){
  data %>% rename(time = !!enquo(time),
                  RR0 = !!enquo(RR0),
                  RR1 = !!enquo(RR1),
                  PP = !!enquo(PP)) %>%
    mutate(timing = (time%%(60/MVR))/(60/MVR))
}
