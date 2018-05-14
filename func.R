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

# fuct that represents the simple polya's urn simulation setup 
# (2 treatments with just treatment plotted)
# input: #simulations #patients (treatments is 2)
# output: plot of the ratio of first treatment over patients

rfuncSimple<-function(sim, pat){
  simPlot <- matrix(NA,sim,pat)           # creates empty matrix for plot
  plot(1:pat,simPlot[1,],                 # creates empty plot
       ylim = c(0,1),
       type = "l",
       bty = "n",
       xlab = "Patients tested",
       ylab = "Probability of Treatment 1")
  
  for(m in 1:sim){                       # for loop for the # simulations
    urn <- 1:2                           # a vector representing the urn
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



# fuct that represents the intermediate polya's urn simulation setup 
# (free choice of tretments and choice of how many treatments plotted (max 3 - best 3))
# input: #simulations #treatments #patients #nbr of treatments plotted
# output: Plot of ratio of treatments (with up to three best treatments)

rfuncInter<-function(sim, trt, pat, plotNbr){
  simPlot <- matrix(NA,sim,pat)           # creates empty matrix for plot
  plot(1:pat,simPlot[1,],                 # creates empty plot
       ylim = c(0,1),
       type = "l",
       bty = "n",
       xlab = "Patients tested",
       ylab = "Probability of Treatment(s)")
  
  for(m in 1:sim){                       # for loop for the # simulations
    
    urn <- 1:trt                         # a vector representing the urn
    ratioMat <- matrix(NA,trt,pat)       # create ratio Matrix
    
    for(n in 1:trt){
      ratioMat[n,1] <- (length(which(urn==n)))/length(urn) # calculating the ration of treatment one (for plotting)
    }
    
    for(k in 2:pat) {                   # for loop for the # patients
      getElm <- sample(urn,1)            # take random element from urn
      urn <- c(urn,getElm)              # update urn (add drawn element + 1 [Polya's urn])
      
      for(p in 1:trt){
        ratio2 <- (length(which(urn==p)))/length(urn) # create new ratio
        ratioMat[p,k] <- ratio2           # update ratio
      }
     
    }
    
    # plot best treatment
    stTrt <- order(colSums(ratioMat),decreasing=T)[1]
    lines(ratioMat[stTrt,],type = "l", col = "red") # create plot - line-graph
    if(plotNbr>1&trt>1){
      # plot second best treatment
      sdTrt <- order(colSums(ratioMat),decreasing=T)[2]
      lines(ratioMat[sdTrt,],type = "l", col = "blue") # create plot - line-graph
    }
    if(plotNbr>2&trt>2){
      # plot third best treatment
      rdTrt <- order(colSums(ratioMat),decreasing=T)[3]
      lines(ratioMat[rdTrt,],type = "l", col = "black") # create plot - line-graph
    }
  }
}

# checker fuction that checks whether input value is >0 & not NA
checkValue <- function(val){
  ifelse(val<=1,val <- 1,val)
  ifelse(is.na(val),val <- 1,val)
  return(val)
}