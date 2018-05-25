#' Polya's Urn Plot Function
#' 
#' function that gets a 3D matrix as an input and returns a plot of max 3 treatments over patients and simulations
#' 
#' @param 3D matrix (treatment x patients x simulations)
#' @param Number of treatments that should be plotted (default = 2)
#' @param Treatment index that should be represented by: Red line, Blue line, Black line (default = 1,2,3)
#' 
#' @return Plot with max three treatments (y axis = ration of the treatment)  over patients (x axis) and simulations
#' @return Red line, Blue line, Black line represents each represent a treatment
#' 
#' @author mitja seibold \email{mitja.seibold@@student.uva.nl}
#' @seealso \code{\link{polyasUrnFuncSimple}}
#' @seealso \code{\link{polyasUrnFuncAdvanced}}
#' 
#' @examples 
#' foo <- array(1:125, dim = c(5,5,5))
#' plotRatio(foo)
#' 
#' foo <- array(1:125, dim = c(5,5,5))
#' prinNbr <- 3
#' pRed <- 5
#' pBlue <- 4
#' pBlack <- 1
#' plotRatio(foo,prinNbr,pRed, pBlue, pBlack)
#' 
#' @export
plotRatio <-
  function(mtrx,
           plotNbr = 2,
           plotRed = 1,
           plotBlue = 2,
           plotBlack = 3) {
    
   # create empty plot
     plot(
      1:length(mtrx[1, , 1]),
      mtrx[1, NA, 1],
      ylim = c(0, 1),
      xlim = c(1, length(mtrx[1, , 1])),
      col = "red",
      type = "l",
      bty = "n",
      xlab = "Patients tested",
      ylab = "Probability of Treatment(s)"
      # legend(length(mtrx[1,,1])-1.5,1,
      #         c("Treatm. 1", "Treatm. 2", "Treatm. 3"),
      #         lty = c(1,1,1),
      #         col = c("red", "blue", "black"))
    )
    # create individual lines
    for (i in 1:length(mtrx[1, 1,])) {
      # plot first treatment in Red
      lines(mtrx[plotRed, , i], type = "l", col = "red") # create plot - line-graph
      # plot second treatment only if there is a second treatment
      if (plotNbr > 1 & length(mtrx[, 1, 1]) > 1) {
        # plot second treatment in Blue
        lines(mtrx[plotBlue, , i], type = "l", col = "blue") # create plot - line-graph
      }
      # plot third treatment only if there is a second treatment
      if (plotNbr > 2 & length(mtrx[, 1, 1]) > 2) {
        # plot third treatment in Black
        lines(mtrx[plotBlack, , i], type = "l", col = "black") # create plot - line-graph
      }
    }
    
  }
