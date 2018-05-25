#' Check Value Function
#' 
#' function that checks the input values if they are <1 and not NA and makes it positive
#' 
#' @param any numeric value
#' 
#' @return updated numeric value
#' 
#' @author mitja seibold \email{mitja.seibold@@student.uva.nl}
#' 
#' @examples 
#' foo <- -4
#' checkValue(foo)
#' 
#' @export
checkValue <- function(val) {
  # checks if input value is >0
  ifelse(val <= 1, val <- 1, val)
  # ckecks if input is not NA
  ifelse(is.na(val), val <- 1, val)
  return(val)
}