#' Best Treatment Function
#' 
#' function that gets a 3D matrix as an input and returns a 2D matrix with the 3 best treatments for each simulations
#' 
#' @param mtrx 3D matrix (treatment x patients x simulations)
#' 
#' @return 2D matrix with the 3 best treatments over patients and simulations
#' 
#' @author mitja seibold \email{mitja.seibold@@student.uva.nl}
#' @seealso \code{\link{polyasUrnFuncSimple}}
#' @seealso \code{\link{polyasUrnFuncAdvanced}}
#' 
#' @examples 
#' foo <- array(1:125, dim = c(5,5,5))
#' bst <- bestTrt(foo)
#' 
#' @export
bestTrt <- function(mtrx) {
  # create an empty 2D matrix treatment x simulations
  mat <- 
    matrix(NA, ifelse(length(mtrx[, 1, 1]) < 3, length(mtrx[, 1, 1]), 3), length(mtrx[1, 1, ]))

  # for loop that includes the best 3 treatments for each simulation into the matrix
  for (i in 1:length(mat[1, ])) {
    for (j in 1:length(mat[, 1])) {
      mat[j, i] <- order(rowSums(mtrx[, , i]), decreasing = T)[j]
    }
  }
  # returns matrix
  return(mat)
}