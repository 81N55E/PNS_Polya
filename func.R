##################################
####### fuctions #################

# author: Mitja Seibold
# mail: 81N55E@gmail.com
# created: 9th May 2018

# TODO: more treatments 
# TODO: different colors
# TODO: plot more treatments 
# TODO: create advanced options

# fuct that represents polya's urn simulation setup 

# fuct that represents the simple polya's urn simulation setup 
# (2 treatments with just treatment plotted)
# input: #simulations #patients (treatments is 2)
# output: matrix of the ratio of first treatment over patients

rfuncSimple<-function(sim, pat){
  simPlot <- matrix(NA,sim,pat)           # creates empty matrix with ration in it
  
  for(m in 1:sim){                       # for loop for the # simulations
    urn <- 1:2                           # a vector representing the urn
    simPlot[m,1] <- (length(which(urn==1)))/length(urn) # calculating the ration of treatment one (for plotting)
    for (k in 2:pat) {                   # for loop for the # patients
      getElm <- sample(urn,1)            # take random element from urn
      urn <- c(urn,getElm)               # update urn (add drawn element + 1 [Polya's urn])
      ratio2 <- (length(which(urn==1)))/length(urn) # create new ration
      simPlot[m,k] <- ratio2           # update ratio
    }
  }
  return(simPlot)
}

# simple plotting function
# input: matrix
# output: plot with rows as different lines
plotRatio <- function(mtrx,difRat){
  plot(1:length(mtrx[1,]),mtrx[1,],                 # creates empty plot
       ylim = c(0,1),
       col = "red",
       type = "l",
       bty = "n",
       xlab = "Patients tested",
       ylab = "Probability of Treatment 1")
  if(length(mtrx[,1])>1){
    for(i in 2:length(mtrx[,1])){
      lines(mtrx[i,],type = "l", col = "red")
    }
  }
}



# fuct that represents the intermediate polya's urn simulation setup 
# (free choice of treatments and choice of how many treatments plotted (max 3 - best 3))
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
    stTrt <- order(rowMeans(ratioMat),decreasing=T)[1]
    lines(ratioMat[stTrt,],type = "l", col = "red") # create plot - line-graph
    if(plotNbr>1&trt>1){
      # plot second best treatment
      sdTrt <- order(rowMeans(ratioMat),decreasing=T)[2]
      lines(ratioMat[sdTrt,],type = "l", col = "blue") # create plot - line-graph
    }
    if(plotNbr>2&trt>2){
      # plot third best treatment
      rdTrt <- order(rowMeans(ratioMat),decreasing=T)[3]
      lines(ratioMat[rdTrt,],type = "l", col = "black") # create plot - line-graph
    }
  }
}


# fuct that represents the advanced polya's urn simulation setup 
# (choice between 2/3 treatments, choice of beginning rate of treatments,
#  choice of nbr of returns and choice of relapse rate for treatments at random)
# input: #simulations #treatments #patients #beginning rate for treatment 1-3 #return rate for treatment 1-3 #relapse rate for treatment 1-3
# output: Plot of ratio of treatments 
funcAdvan <- function(sim, trt, pat, nbrBalls1, nbrBalls2, nbrRetur1, nbrRetur2, nbrRelap1, nbrRelap2){
  simPlot <- matrix(NA,sim,pat)                                        # creates empty matrix for plot
  plot(1:pat,simPlot[1,],                                              # creates empty plot
       ylim = c(0,1),
       type = "l",
       bty = "n",
       xlab = "Patients tested",
       ylab = "Probability of Treatment(s)")
  
  for(m in 1:sim){                                                     # for loop for the # simulations
    
    urn <- c(rep(1,nbrBalls1), rep(2,nbrBalls2))                       # a vector representing the urn
    ratioMat <- matrix(NA,trt,pat)                                     # create ratio Matrix
    
    for(n in 1:trt){
      ratioMat[n,1] <- (length(which(urn==n)))/length(urn)             # calculating the ration of treatment one (for plotting)
    }
    
    for(k in 2:pat) {                                                  # for loop for the # patients
      getElm <- sample(urn,1)                                          # take random element from urn
      if(getElm == 1){
        urn <- c(urn,rep(getElm,nbrRetur1))                            # update urn (add drawn element + 1 [Polya's urn])
      }else{
        urn <- c(urn,rep(getElm,nbrRetur2))
      }
      
      # Relapse Random 
      # only works after 10 patients
      if (k >= 10){
        dice <- sample(1:10,1) #
        if(dice == 10){
          urn1 <- urn[! urn %in% 2]                   # create single urns for each treatment
          urn2 <- urn[! urn %in% 1]                   # create single urns for each treatment
          
          if(nbrRelap1>0){
            urn1 <- head(urn1,-(nbrRelap1))
          }
          if(nbrRelap2>0){
            urn2 <- head(urn2, -(nbrRelap2))
          }
          urn <- c(urn1,urn2)
        }
      }
      
      for(p in 1:trt){
        ratio2 <- (length(which(urn==p)))/length(urn) # create new ratio
        ratioMat[p,k] <- ratio2           # update ratio
      }
      
    }
    
    # plot treatments
    lines(ratioMat[1,],type = "l", col = "red") # create plot - line-graph
    #if(plotNbr>1&trt>1){
      # plot second treatment
      lines(ratioMat[2,],type = "l", col = "blue") # create plot - line-graph
   # }
   # if(plotNbr>2&trt>2){
      # plot third best treatment
    #  rdTrt <- order(rowMeans(ratioMat),decreasing=T)[3]
    #  lines(ratioMat[rdTrt,],type = "l", col = "black") # create plot - line-graph
    #}
  }
}



# checker fuction that checks whether input value is >0 & not NA
checkValue <- function(val){
  ifelse(val<=1,val <- 1,val)
  ifelse(is.na(val),val <- 1,val)
  return(val)
}