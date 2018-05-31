#' Polya's Urn Advanced Function
#' 
#' function that represents the advanced polya's urn simulation setup and creates a 3D matrix (treatment x patients x simulations) to be used for the \code{\link{plotRatio}}
#' Besides the same input as in \code{\link{polyasUrnFuncSimple}} additionally you have the ability to
#' change the treatment success rate for treatment 1 and 2, the number of starting balls (for treatment 1 and 2),
#' the number of return balls (for treatment 1 and 2) and the relapse rate of treatment 1 and 2.
#' 
#' @param pat Number of patients
#' @param sim Number of simulations
#' @param trt Number of treatments (default = 2)
#' @param trtRat1 Success rate of 1. treatment
#' @param trtRat2 Success rate of 2. treatment
#' @param nbrBalls1 Beginn rate of balls for 1. treatment
#' @param nbrBalls2 Beginn rate of balls for 2. treatment
#' @param nbrRetur1 Return rate of balls for 1. treatment
#' @param nbrRetur2 Return rate of balls for 2. treatment
#' @param nbrRelap1 Relapse rate of balls for 1. treatment
#' @param nbrRelap2 Relapse rate of balls for 2. treatment
#'  
#' @return 3D matrix (treatment x patients x simulations) with the ratio of each treatment compared to all treatments for each patient and each simulation
#' 
#' @author mitja seibold \email{mitja.seibold@@student.uva.nl}
#' @seealso \code{\link{polyasUrnFuncSimple}}
#' 
#' @examples 
#' p <- 100
#' s <- 2
#' t <- 3
#' plotRatio(polyasUrnFuncAvanced(p,s,t))
#'
#' p <- 100
#' s <- 2
#' t <- 3
#' tRat1 <- 50, tRat2 <- 75
#' nBalls1 <- 2, nBalls2 <- 3
#' nRet1 <- 4, nRet2 <- 2
#' nRel1 <- 3
#' nRel2 <- 2 
#' plotRatio(polyasUrnFuncAdvanced(p,s,t,tRat1,tRat2,nBall1,nBalls2,nRet1,nRet2,nRel1,nRel2))
#' 
#' @export
polyasUrnFuncAdv <- function(pat,
                             sim,
                             trt = 2,
                             trtRat1 = 100,
                             trtRat2 = 100,
                             nbrBalls1 = 1,
                             nbrBalls2 = 1,
                             nbrRetur1 = 1,
                             nbrRetur2 = 1,
                             nbrRelap1 = 0,
                             nbrRelap2 = 0) {
  ratioMat <-
    array(NA, dim = c(trt, pat, sim))                       # creates empty 3D matrix for ration
  for (m in 1:sim) {
    # for loop for the # simulations
    
    urn <-
      c(1:trt, rep(1, nbrBalls1 - 1), rep(2, nbrBalls2 - 1))# a vector representing the urn
    
    for (n in 1:trt) {
      ratioMat[n, 1, m] <-
        (length(which(urn == n))) / length(urn)             # calculating the ratio of each treatment
    }
    
    for (k in 2:pat) {
      # for loop for the # patients
      getElm <-
        sample(urn, 1)                                      # take random element from urn
      if (getElm == 1) {
        # checks if drawn Element is equal to 1
        s1 <- sample(1:100, 1)
        # gets a smaple out of 100
        if (s1 <= trtRat1) {
          # checks the success rate of treatment 1
          urn <-
            c(urn, rep(getElm, nbrRetur1))                  # update urn (add number of return balls for treatment 1)
        } else {
          urn <- urn
        }
        
      } else if (getElm == 2) {
        # check if drawn element is equalt to 2
        s2 <- sample(1:100, 1)
        # gets a sample out of 100
        if (s2 <= trtRat2) {
          # checks success rate of treatment 2
          urn <- c(urn, rep(getElm, nbrRetur2))            # update urn (add number of return balls for treatment 2)
        } else {
          urn <- urn
        }
      } else {
        urn <- c(urn, getElm)
      }
      
      # Relapse Random
      # only works after 10 patients
      if (k >= 10) {
        dice <- sample(1:10, 1) #
        if (dice == 10) {
          urn1 <-
            urn[urn %in% 1]                   # create single urns for treatment 1
          urn2 <-
            urn[urn %in% 2]                   # create single urns for treatment 2
          urn3 <-
            urn[!urn %in% c(1, 2)]            # create single urn without treatment 1&2
          
          if (nbrRelap1 > 0 & length(urn1)>nbrRelap1) {
            urn1 <- head(urn1,-(nbrRelap1))   # update urn (minus the relapse score rate for treatment 1)
          }
          if (nbrRelap2 > 0 & length(urn2)>nbrRelap2) {
            urn2 <- head(urn2, -(nbrRelap2))  # update urn (minus the relapse score rate for treatment 2)
          }
          urn <- c(urn1, urn2, urn3)
        }
      }
      
      for (p in 1:trt) {
        ratio2 <- (length(which(urn == p))) / length(urn) # create new ratio
        ratioMat[p, k, m] <- ratio2           # update ratio
      }
      
    }
  }
  return(ratioMat)
}
