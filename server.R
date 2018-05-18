shinyServer(
  function(input, output, session){
  
    source("func.R") # calls function file

  ################################################    
  ##### get reactive variables ###################
  ################################################  
    
    nbrT <- reactive({ as.numeric(input$`Number of Treatments`)}) 
    nbrP <- reactive({ as.numeric(input$`Number of Patients`)})
    nbrS <- reactive({ as.numeric(input$`Number of Simulations`)})
  
    polya <- reactive({polyasUrnFunc(nbrT(),nbrP(),nbrS())})
    
    
##################### functions ##############################    
    
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
      
      

      ####################################
      ###### get variables ###############
      
      verType <- input$Version
      
      
      #nbrTre <- checkValue(nbrTre)
      #nbrPat <- nbrP()
      #nbrPat <- checkValue(nbrPat)
      #nbrSim <- nbrS()
      #nbrSim <- checkValue(nbrSim)
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
        plotRatio(polya(),verType)
      }else if(verType == "intermediate"){        # simulates urn in intermediate (>2 treatments available)
        plotRatio(polya(),verType,nbrPlt)
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
        
        plotRatio(polya(), verType)
       
      }
    }
    )
    
    output$bestTrt <- renderValueBox({
      valueBox(
        value = bestTrt(polya()),
        subtitle = "Best Treatment after all simulations",
        icon = icon("table"),
        color = "yellow"
      )
    })
  }
)
