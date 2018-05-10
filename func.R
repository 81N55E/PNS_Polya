##################################
####### fuctions #################

# author: Mitja Seibold
# mail: 81N55E@gmailcom
# created: 9th May 2018

# TODO: more treatments - DONE
# TODO: different colors - 
# TODO: plot more treatments
# TODO: create advanced options

# fuct that represents polya's urn simulation setup 
# input #simulations #treatments #patients
rfunc<-function(sim, trt, pat){
  simPlot <- matrix(NA,sim,pat)           # creates empty matrix for plot
  plot(1:pat,simPlot[1,],                 # creates empty plot
       ylim = c(0,1),
       type = "l",
       bty = "n",
       xlab = "Patients tested",
       ylab = "Probability of Treatment 1")
  
  for(m in 1:sim){                       # for loop for the # simulations
    urn <- 1:trt                         # a vector representing the urn
    ratio <- (length(which(urn==1)))/length(urn) # calculating the ration of treatment one (for plotting)
    for (k in 1:pat) {                   # for loop for the # patients
      getElm <- sample(urn,1)            # take random element from urn
      urn <- c(urn,getElm)               # update urn (add drawn element + 1 [Polya's urn])
      ratio2 <- (length(which(urn==1)))/length(urn) # create new ration
      ratio <- c(ratio,ratio2)           # update ratio
    }
    lines(ratio,type = "l", col = "red") # create plot - line-graph
  }
}

# checker fuction that checks whether input value is >0 & not NA
checkValue <- function(val){
  ifelse(val<=1,val <- 1,val)
  ifelse(is.na(val),val <- 1,val)
  return(val)
}