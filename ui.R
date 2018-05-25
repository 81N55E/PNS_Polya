#############################################################################
# Shiny User Interface 
# @author mitja seibold \email{mitja.seibold@student.uva.nl}
# @created May 2018

# function that represents the user interface of the shiny app 
# and interacts with the server.R function

###############################################################################
library(PolyasShinyUrn)
library(shiny)
library(shinydashboard)


shinyUI <- dashboardPage(
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
)

