#' Polya's Urn Simple Function
#' 
#' function that represents the simple polya's urn simulation setup and creates a 3D matrix (treatment x patients x simulations) to be used for the \code{\link{plotRatio}}
#' 
#' @param Number of patients
#' @param Number of simulations
#' @param Number of treatments (default = 2) 
#'  
#' @return 3D matrix (treatment x patients x simulations) with the ratio of each treatment compared to all treatments for each patient and each simulation
#' 
#' @author mitja seibold \email{mitja.seibold@@student.uva.nl}
#' @seealso \code{\link{polyasUrnFuncAdvanced}}
#' 
#' @examples 
#' p <- 100
#' s <- 2
#' t <- 3
#' plotRatio(polyasUrnFuncSimple(p,s,t))
#' 
#' @export
polyasUrnFuncSimple <- function(pat,
                                sim,
                                trt = 2,
                                trtRat = 100) {
  ratioMat <-
    array(NA, dim = c(trt, pat, sim))           # creates empty 3D matrix 
  
  for (m in 1:sim) {
    # for loop for the # simulations
    
    urn <-
      c(1:trt)                                  # a vector representing the urn
    
    for (n in 1:trt) {
      ratioMat[n, 1, m] <-
        (length(which(urn == n))) / length(urn) # calculating the ratio of each treatment 
    }
    
    for (k in 2:pat) {
      # for loop for the # patients
      getElm <-
        sample(urn, 1)                          # take random element from urn
      if (sample(1:100, 1) <= trtRat) {
        urn <-
          c(urn, getElm)                        # update urn (add drawn element + 1 [Polya's urn])
      }
      for (p in 1:trt) {
        ratio2 <- (length(which(urn == p))) / length(urn) # get new ratio
        ratioMat[p, k, m] <- ratio2            # update ratio
      }
      
    }
  }
  return(ratioMat)
}
