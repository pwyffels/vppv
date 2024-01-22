#' Example dataset of a patient in Atrial Fibrillation
#'
#' A dataset of a patient in atrial fibrillation of 3 minutes containing beat-to-beat measurements.
#'
#' @format A dataframe of 177 rows and 4 variables:
#' \describe{
#'   \item{time}{timestamp of ECG Rwave of each individual beat, in sec}
#'   \item{RR0}{length of the RR interval preceding each individual beat, in sec}
#'   \item{RR1}{length of the RR interval pre-preceding each individual beat, in sec}
#'   \item{PP}{Pulse Pressure (systolic-Diastolic bloodpressure) for each individual beat, in mmHg}
#'   }
#'@source dataset extracted from COMPASSretro database
"afib"

