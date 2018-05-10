shinyServer(
  function(input, output, session){
    output$myplot <- renderPlot({
      
      ##################################
      ####### fuctions #################
      
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
      
      ####################################
      ###### get variables ###############
      
      verType <- input$Version
      
      nbrTre <- as.numeric(input$`Number of Treatments`)
      nbrTre <- checkValue(nbrTre)
      nbrPat <- as.numeric(input$`Number of Patients`)
      nbrPat <- checkValue(nbrPat)
      nbrSim <- as.numeric(input$`Number of Simulations`)
      nbrSim <- checkValue(nbrSim)
      
      # advanced variables
      adv1 <- input$special1
      ifelse(adv1 == "Yes", adv1<-T, adv1<-F)
      adv2 <- input$special2
      ifelse(adv2 == "Yes", adv2<-T, adv2<-F)
      
      
     
      ######################################
      ###### pipeline #####################
      
      if(verType == "simple"){                    # simulates urn in simple mode (only two treatments)
        rfunc(nbrSim,2,nbrPat)
      }else if(verType == "intermediate"){        # simulates urn in intermediate (>2 treatments available)
        rfunc(nbrSim,nbrTre,nbrPat)
      }else{                                      # simulates urn in advanced mode
        if(adv1&adv2 == T){
          rfunc(nbrSim,nbrTre,nbrPat)
        }else if(adv1==T&adv2==F){
          rfunc(nbrSim,nbrTre,nbrPat)
        }else if(adv1==F&adv2==T){
          rfunc(nbrSim,nbrTre,nbrPat)
        }else {
          rfunc(nbrSim,nbrTre,nbrPat)
        }
      }
    
  }
  )
  }
)
