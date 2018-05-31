#' Polya's Shiny Roundup Function
#' 
#' Function that starts the shiny app of the polya's urn simulation - the clinical trial dilemma
#' 
#' @param  None
#' 
#' @return opens up the shiny app and runs it
#' 
#' @author mitja seibold \email{mitja.seibold@@student.uva.nl}
#' 
#' @examples 
#' polyasShinyRoundup()
#' 
#' @export
polyasShinyRoundup <- function(){
  
  require(shiny)
  require(shinydashboard)
  
  shinyApp(
    
    #############################################################################
    # Shiny User Interface 
    # @author mitja seibold \email{mitja.seibold@student.uva.nl}
    # @created May 2018
    
    # function that represents the user interface of the shiny app 
    # and interacts with the server.R function
    
    ###############################################################################
    
    
    ui <- dashboardPage(
      skin = "blue",
      # get specific skin
      dashboardHeader(title = "Polya's Urn Simulation"),
      # create Dashboard sidebar
      dashboardSidebar(sidebarMenu(
        menuItem(
          "Introduction",
          tabName = "dashboard",
          icon = icon("dashboard")
        ),
        menuItem(
          "User Input",
          tabName = "userinput",
          icon = icon("cog", lib = "glyphicon")
        ),
        menuItem("Charts", tabName = "charts", icon = icon("bar-chart-o"))
      )),
      # creates the Body of the dashboard
      dashboardBody(tabItems(
        
        # First tab content - Introduction
        tabItem(tabName = "dashboard",
                # calls introPrint function to create Intro text 
                introPrint()),
        
        # Second tab content - User Input
        tabItem(
          tabName = "userinput",
          box(
            title = "INPUT",
            status = "primary",
            width = 6,
            selectInput(
              "Version",
              "Please select version:",
              choices = c("simple", "advanced")
            ),
            # text input of the treatment, patients and simulations
            textInput("Number of Patients", "Please select number of patients: ", 50),
            textInput("Number of Simulations", "Please select number of simulations: ", 1),
            textInput("Number of Treatments", "Please select number of treatments: ", 2),
            
            # conditional panel for treatment success
            conditionalPanel(
              condition = "input.Version =='simple'",
              sliderInput(
                "Treatment Success",
                "Please select the treatment success (in percent) for a participant:",
                min = 0,
                max = 100,
                value = 100,
                step = 5
              )
            ),
            
            # conditional panal for treatment success (treatment 1 & 2)
            conditionalPanel(
              condition = "input.Version =='advanced'",
              sliderInput(
                "Treatment Success1",
                "Please select the treatment success for treatment 1 (in percent) for a participant:",
                min = 0,
                max = 100,
                value = 100,
                step = 5
              ),
              sliderInput(
                "Treatment Success2",
                "Please select the treatment success for treatment 2 (in percent) for a participant:",
                min = 0,
                max = 100,
                value = 100,
                step = 5
              )
              
            )
            
          ),
          
          
          # conditional panal for treatment 1 & 2: 
          # number of balls in the beginning
          # number of return balls
          # the relapse rate 
          conditionalPanel(
            condition = "input.Version == 'advanced'",
            box(
              title = "INPUT-Advanced",
              status = "primary",
              width = 6,
              
              textInput(
                "begNbrOfBalls1",
                "Please select the beginning treatment 1 rate:",
                1
              ),
              textInput(
                "begNbrOfBalls2",
                "Please select the beginning treatment 2 rate:",
                1
              ),
              
              textInput(
                "nbrOfReturns1",
                "Please select for treatment 1 the number of returns after each patient:",
                1
              ),
              textInput(
                "nbrOfReturns2",
                "Please select for treatment 2 the number of returns after each patient:",
                1
              ),
              
              sliderInput(
                "nbrOfRelapse1",
                "Please select for treatment 1 the relapse rate:",
                min = 0,
                max = 10,
                value = 0,
                step = 1
              ),
              sliderInput(
                "nbrOfRelapse2",
                "Please select for treatment 2 the relapse rate:",
                min = 0,
                max = 10,
                value = 0,
                step = 1
              )
            )
          )
        ),
        
        # Third tab content - creating of the Output:
        # Value Boxes + plot + Info Boxes 
        # deciding which and how many treatments plotted
        tabItem(
          tabName = "charts",
          fluidRow(
            valueBoxOutput("rate"),
            valueBoxOutput("count"),
            valueBoxOutput("users")
          ),
          fluidRow(
            box(width = 8, title = "Graph",
                plotOutput("myplot")),
            infoBoxOutput("bestTrt"),
            infoBoxOutput("bestTrtRat"),
            infoBoxOutput("ndTrt"),
            infoBoxOutput("rdTrt")
          ),
          fluidRow(
            box(
              width = 4,
              textInput("RedPlot", "Which Treatment should be plotted in Red?", 1)
            ),
            box(
              width = 4,
              textInput("BluePlot", "Which Treatment should be plotted in Blue?", 2)
            ),
            box(
              width = 4,
              textInput("BlackPlot", "Which Treatment should be plotted in Black?", 3)
            )
          ),
          # box that creates the number of treatments plotted
          fluidRow(
            box(
              title = "Number of Treatments Plotted",
              width = 12,
              sliderInput(
                "Plot3BestTrts",
                "Please select number of best treatments plotted (max 3):",
                min = 1,
                max = 3,
                value = 2 ,
                step = 1
              )
            )
          )
        )
      ))
    ),
    
    #############################################################################
    # Shiny Server 
    # @author mitja seibold \email{mitja.seibold@student.uva.nl}
    # @created May 2018
    
    # function that represents the server of the shiny app
    
    # @param input, output, session
    
    # @return updates graph depending on the output of the polya's urn including different info boxes regarding best treatments etc
    
    ###############################################################################
    server <- function(input, output) {
      
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
      
      # get best treatment over all simulations
      output$bestTrt <- renderInfoBox({
        infoBox( 
          title = "Treatment Nr",
          value = ifelse(inpVer() == "advanced",
                         as.numeric(names(sort(table(bestTrt(polyaAdv())[1, ]), decreasing = TRUE)[1])),
                         as.numeric(names(sort(table(bestTrt(polya())[1, ]), decreasing = TRUE)[1]))),
          subtitle = "is best Treatment (after all simulations)",
          icon = icon("list"),
          color = "blue"
        )
      })
      
      # get ratio of best treatment
      output$bestTrtRat <- renderInfoBox({
        infoBox( 
          title = "Is best threatment in",
          value = ifelse(inpVer() == "advanced",
                         (as.numeric(sort(table(bestTrt(polyaAdv())[1, ]), decreasing = TRUE)[1])/nbrS())*100,
                         (as.numeric(sort(table(bestTrt(polya())[1, ]), decreasing = TRUE)[1])/nbrS())*100),
          subtitle = "% of simulations",
          icon = icon("list")
        )
      })
      
      # get second best treatment over all simulations
      output$ndTrt <- renderInfoBox({
        infoBox(
          title = "Treatment Nr",
          value = ifelse(inpVer() == "advanced",
                         as.numeric(names(sort(table(bestTrt(polyaAdv())[2, ]), decreasing = TRUE)[1])),
                         as.numeric(names(sort(table(bestTrt(polya())[2, ]), decreasing = TRUE)[1]))),
          subtitle = "is 2nd best Treatment (after all simulations)",
          icon = icon("list"),
          color = "blue"
        )
      })
      
      # get third best treatment over all simulations
      output$rdTrt <- renderInfoBox({
        infoBox(
          title = "Treatment Nr",
          value = ifelse(inpVer() == "advanced",
                         as.numeric(names(sort(table(bestTrt(polyaAdv())[3, ]), decreasing = TRUE)[1])),
                         as.numeric(names(sort(table(bestTrt(polya())[3, ]), decreasing = TRUE)[1]))),
          subtitle = "is 3rd best Treatment (after all simulations)",
          icon = icon("list"),
          color = "blue"
        )
      })
    })
}