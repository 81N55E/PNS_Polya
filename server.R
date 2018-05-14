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
      nbrPlt <- as.numeric(input$Plot3BestTrts)
      
      # advanced variables
      adv1 <- input$special1
      ifelse(adv1 == "Yes", adv1<-T, adv1<-F)
      adv2 <- input$special2
      ifelse(adv2 == "Yes", adv2<-T, adv2<-F)
      
      
     
      ######################################
      ###### pipeline #####################
      
      if(verType == "simple"){                    # simulates urn in simple mode (only two treatments)
        rfuncSimple(nbrSim,nbrPat)
      }else if(verType == "intermediate"){        # simulates urn in intermediate (>2 treatments available)
        rfuncInter(nbrSim,nbrTre,nbrPat,nbrPlt)
      }else{                                      # simulates urn in advanced mode
        if(adv1&adv2 == T){
          #rfunc(nbrSim,nbrTre,nbrPat)
          print("Has still to be build")
        }else if(adv1==T&adv2==F){
          #rfunc(nbrSim,nbrTre,nbrPat)
          print("Has still to be build")
        }else if(adv1==F&adv2==T){
          #rfunc(nbrSim,nbrTre,nbrPat)
          print("Has still to be build")
        }else {
          #rfunc(nbrSim,nbrTre,nbrPat)
          print("Has still to be build")
        }
      }
    
  }
  )
  }
)
