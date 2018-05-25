#' Best Treatment Function
#' 
#' function that gets a 3D matrix as an input and returns a vector with the 3 best treatments over patients and simulations
#' 
#' @param 3D matrix (treatment x patients x simulations)
#' 
#' @return vector with the 3 best treatments over patients and simulations
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
  # create an empty vector 
  vec <- NA
  # for loop that includes the best 3 treatments for each simulation into the matrix
  for (i in 1:length(mat[1, ])) {
    for (j in 1:length(mat[, 1])) {
      mat[j, i] <- order(rowSums(mtrx[, , i]), decreasing = T)[j]
    }
  }
  # for loop that uses the 2D matrix to get the best three treatments over all simulations
  for (m in 1:length(mat[, 1])) {
    vec[m] <- as.numeric(names(sort(table(mat[m, ]), decreasing = TRUE)[1]))
  }
  
  return(vec)
}