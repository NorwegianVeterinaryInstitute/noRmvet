#' Calculate Confidence Intervals
#'
#' This function is used to calculate confidence intervals.
#'
#' @param x The name of the column with the number of positive/resistant samples
#' @param n The name of the column with the total number of samples
#'
#' @author Håkon Kaspersen \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#'
#' @importFrom stats binom.test
#' @importFrom stats setNames
#'
get_binCI <- function(x, n){
  if (n > 0){
    as.numeric(
      setNames(
        binom.test(x, n)$conf.int * 100,
        c("lwr", "upr")
      )
    )
  } else {
    NA
  }
}
