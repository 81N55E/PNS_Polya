shinyServer(
  function(input, output, session){
    output$myplot <- renderPlot({
      
      source("func.R") # calls function file
      
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
