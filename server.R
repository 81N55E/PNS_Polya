#############################################################################
# Shiny Server 
# @author mitja seibold \email{mitja.seibold@student.uva.nl}
# @created May 2018

# function that represents the server of the shiny app

# @param input, output, session

# @return updates graph depending on the output of the polya's urn including different info boxes regarding best treatments etc

###############################################################################
shinyServer(function(input, output, session) {
  
  ################################################
  ##### get reactive variables ###################
  ################################################
  inpVer <- reactive({
    input$Version
  })
  
  nbrT <-
    reactive({
      checkValue(as.numeric(input$`Number of Treatments`))
    })
  nbrP <-
    reactive({
      checkValue(as.numeric(input$`Number of Patients`))
    })
  nbrS <-
    reactive({
      checkValue(as.numeric(input$`Number of Simulations`))
    })
  
  probTrt <- reactive({as.numeric(input$`Treatment Success`)})
  
  probTrt1 <- reactive({as.numeric(input$`Treatment Success1`)})
  probTrt2 <- reactive({as.numeric(input$`Treatment Success2`)})
  
  nbrBalls1 <-
    reactive({
      checkValue(as.numeric(input$begNbrOfBalls1))
    })
  nbrBalls2 <-
    reactive({
      checkValue(as.numeric(input$begNbrOfBalls2))
    })
  
  nbrRetur1 <-
    reactive({
      checkValue(as.numeric(input$nbrOfReturns1))
    })
  nbrRetur2 <-
    reactive({
      checkValue(as.numeric(input$nbrOfReturns2))
    })
  
  nbrRelap1 <- reactive({
    as.numeric(input$nbrOfRelapse1)
  })
  nbrRelap2 <- reactive({
    as.numeric(input$nbrOfRelapse2)
  })
  
######################################################
############ run polya urn simulation ################ 
######################################################
 
   # running simple version
  polya    <- reactive({
    polyasUrnFuncSimple(nbrP(), nbrS(), nbrT(), probTrt())
  })
  # running the advanced version
  polyaAdv    <- reactive({
    polyasUrnFuncAdv(
      nbrP(),
      nbrS(),
      nbrT(),
      probTrt1(),
      probTrt2(),
      nbrBalls1(),
      nbrBalls2(),
      nbrRetur1(),
      nbrRetur2(),
      nbrRelap1(),
      nbrRelap2()
    )
  })
  
  
  
  
  ###################################################
  ############# create Value Boxes ##################
  ###################################################
  
  output$rate <- renderValueBox({
    valueBox(
      value = input$`Number of Treatments`,
      subtitle = "Number of Treatments",
      icon = icon("download"),
      color = "blue"
    )
  })
  
  output$users <- renderValueBox({
    valueBox(
      value = input$`Number of Patients`,
      subtitle = "Number of Patients",
      icon = icon("users"),
      color = "blue"
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
  
#########################################
########## create plot ##################
#########################################
  
  output$myplot <- renderPlot({
    
    ####################################
    ###### get variables ###############
    
    # number of lines plotted
    nbrPlt <- as.numeric(input$Plot3BestTrts)
    
    # which treatment is assigned to which Line
    nbrRedPlot <- checkValue(as.numeric(input$RedPlot))
    nbrBluePlot <- checkValue(as.numeric(input$BluePlot))
    nbrBlackPlot <- checkValue(as.numeric(input$BlackPlot))
    
    
    #####################################
    ###### pipeline #####################
    
    # Plot
    if (inpVer() == "advanced") {
      plotRatio(polyaAdv(), nbrPlt, nbrRedPlot, nbrBluePlot, nbrBlackPlot)
    } else{
      plotRatio(polya(), nbrPlt, nbrRedPlot, nbrBluePlot, nbrBlackPlot)
    }
    
  })
  
##############################################
############# create Info Boxes ##############
##############################################
  
  output$bestTrt <- renderInfoBox({
    infoBox(
      title = "Treatment Nr",
      value = ifelse(inpVer() == "advanced",bestTrt(polyaAdv())[1],bestTrt(polya())[1]),
      subtitle = "is best Treatment (after all simulations)",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ndTrt <- renderInfoBox({
    infoBox(
      title = "Treatment Nr",
      value = ifelse(inpVer() == "advanced",bestTrt(polyaAdv())[2],bestTrt(polya())[2]),
      subtitle = "is 2nd best Treatment (after all simulations)",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$rdTrt <- renderInfoBox({
    infoBox(
      title = "Treatment Nr",
      value = ifelse(inpVer() == "advanced",bestTrt(polyaAdv())[3],bestTrt(polya())[3]),
      subtitle = "is 3rd best Treatment (after all simulations)",
      icon = icon("list"),
      color = "blue"
    )
  })
})
