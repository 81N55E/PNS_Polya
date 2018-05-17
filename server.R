shinyServer(
  function(input, output, session){
    
    output$rate <- renderValueBox({
      valueBox(
        value = input$`Number of Treatments`,
        subtitle = "Number of Treatments",
        icon = icon("download")
      )
    })
    
    output$users <- renderValueBox({
      valueBox(
        value = input$`Number of Patients`,
        subtitle = "Number of Patients",
        icon = icon("users")
      )
    })
    
    output$count <- renderValueBox({
      valueBox(
        value = input$`Number of Simulations`,
        subtitle = "Number of Simulations",
        icon = icon("area-chart"),
        color = "yellow"
      )
    })
    
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
      isBalls   <- input$BegBalls
      nbrBalls1 <- as.numeric(input$begNbrOfBalls1)
      nbrBalls1 <- checkValue(nbrBalls1)
      nbrBalls2 <- as.numeric(input$begNbrOfBalls2)
      nbrBalls2 <- checkValue(nbrBalls2)
      
      isReturn  <- input$ReturnBalls 
      nbrRetur1 <- as.numeric(input$nbrOfReturns1)
      nbrRetur1 <- checkValue(nbrRetur1)
      nbrRetur2 <- as.numeric(input$nbrOfReturns2)
      nbrRetur2 <- checkValue(nbrRetur2)
      
      isRelapse <- input$Relapse
      nbrRelap1 <- as.numeric(input$nbrOfRelapse1)
      nbrRelap2 <- as.numeric(input$nbrOfRelapse2)
     
      
      ######################################
      ###### pipeline #####################
      
      if(verType == "simple"){                    # simulates urn in simple mode (only two treatments)
        plotRatio(rfuncSimple(nbrSim,nbrPat))
      }else if(verType == "intermediate"){        # simulates urn in intermediate (>2 treatments available)
        rfuncInter(nbrSim,nbrTre,nbrPat,nbrPlt)
      }else{                                      # simulates urn in advanced mode
        if(isBalls == "No"){
          print("isBallsNo")
          nbrBalls1 = 1
          nbrBalls2 = 1
        }
        
        if(isReturn == "No"){
          print("isReturnNo")
          nbrRetur1 = 1
          nbrRetur2 = 1
        }
        
        if (isRelapse == "No"){
          print("isReleaseNo")
          nbrRelap1 = 0
          nbrRelap2 = 0
        }
        
        funcAdvan(nbrSim,2,nbrPat,
                  nbrBalls1, nbrBalls2,
                  nbrRetur1, nbrRetur2,
                  nbrRelap1, nbrRelap2)
       
      }
      
    }
    )
  }
)
