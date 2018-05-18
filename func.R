##################################
####### fuctions #################

# author: Mitja Seibold
# mail: 81N55E@gmail.com
# created: 9th May 2018

# fuct that represents the advanced polya's urn simulation setup 
# (choice between 2/3 treatments, choice of beginning rate of treatments,
#  choice of nbr of returns and choice of relapse rate for treatments at random)
# input: #simulations #treatments #patients #beginning rate for treatment 1-3 #return rate for treatment 1-3 #relapse rate for treatment 1-3
# output: Plot of ratio of treatments 
polyasUrnFunc <- function(trt = 2, pat, sim, 
                          nbrBalls1 = 1, nbrBalls2 = 1, 
                          nbrRetur1 = 1, nbrRetur2 = 1, 
                          nbrRelap1 = 0, nbrRelap2 = 0){
  
  ratioMat <- array(NA,dim = c(trt,pat,sim))                           # creates empty matrix for plot
  for(m in 1:sim){                                                     # for loop for the # simulations
    
    urn <- c(1:trt,rep(1,nbrBalls1-1), rep(2,nbrBalls2-1))                       # a vector representing the urn
    
    for(n in 1:trt){
      ratioMat[n,1,m] <- (length(which(urn==n)))/length(urn)             # calculating the ration of treatment one (for plotting)
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
        ratioMat[p,k,m] <- ratio2           # update ratio
      }
      
    }
  }
  return(ratioMat)
}




#################################################################
################ Plotting function ##############################
#################################################################

# simple plotting function
# input: matrix
# output: plot with rows as different lines
plotRatio <- function(mtrx, ver, plotNbr = 2){
  
  plot(1:length(mtrx[1,,1]),mtrx[1,NA,1],                 # creates empty plot
       ylim = c(0,1),
       col = "red",
       type = "l",
       bty = "n",
       xlab = "Patients tested",
       ylab = "Probability of Treatment 1"
  )
  
  switch(ver, 
         simple = {
             for(i in 1:length(mtrx[1,1,])){
               lines(mtrx[1,,i],type = "l", col = "red")
             }
         },
         
         intermediate = {
             for(i in 1:length(mtrx[1,1,])){
               # plot best treatment
               stTrt <- order(rowMeans(mtrx[,,i]),decreasing=T)[1]
               lines(mtrx[stTrt,,i],type = "l", col = "red") # create plot - line-graph
               if(plotNbr>1&length(mtrx[,1,1])>1){
                 # plot second best treatment
                 sdTrt <- order(rowMeans(mtrx[,,i]),decreasing=T)[2]
                 lines(mtrx[sdTrt,,i],type = "l", col = "blue") # create plot - line-graph
               }
               if(plotNbr>2&length(mtrx[,1,1])>2){
                 # plot third best treatment
                 rdTrt <- order(rowMeans(mtrx[,,i]),decreasing=T)[3]
                 lines(mtrx[rdTrt,,i],type = "l", col = "black") # create plot - line-graph
               }
             }
         },
         
         advanced = {
           for(i in 1:length(mtrx[1,1,])){
             lines(mtrx[1,,i],type = "l", col = "red")
             lines(mtrx[2,,i],type = "l", col = "blue")
           }
         }
  )
}

######### additional functions ####################

# function that returns best treatment after all simulations
bestTrt <- function(mtrx){
  vec <- length(mtrx[1,1,])
  for(i in 1: length(mtrx[1,1,])){
    vec[i] <- order(rowSums(mtrx[,,1]),decreasing=T)[1]
  }
  return(as.numeric(names(sort(table(vec),decreasing=TRUE)[1])))
}


# checker fuction that checks whether input value is >0 & not NA
checkValue <- function(val){
  ifelse(val<=1,val <- 1,val)
  ifelse(is.na(val),val <- 1,val)
  return(val)
}