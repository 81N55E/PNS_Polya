# Dashboard
# add plot
# more user friendly
# add add ons 
library(shiny)
library(shinydashboard)



shinyUI <- dashboardPage( skin = "red",
  dashboardHeader(title = "Polya's Urn Simulation"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("User Input", tabName = "userinput", icon = icon("cog", lib = "glyphicon")),
      menuItem("Charts", tabName = "charts", icon = icon("bar-chart-o"))
    )
  ),
  dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            h2("The Clinical Trial Dilemma - a Polya's Urn Simulation")
    ),
    
    # Second tab content
    tabItem(tabName = "userinput",
            box( title = "INPUT", status = "primary", width = 8,
                 selectInput("Version", "Please select version:",
                             choices = c("simple","intermediate","advanced")),
                 
                 
                 textInput("Number of Patients", "Please select number of patients: ", 50),
                 textInput("Number of Simulations", "Please select number of simulations: ", 1),
                 
                 conditionalPanel(condition = "input.Version == 'intermediate'",
                                  textInput("Number of Treatments", "Please select number of treatments: ", 2),
                                  sliderInput("Plot3BestTrts", "Please select number of best treatments plotted (max 3):",
                                              min = 1, max = 3, value = 2 ,step = 1)
                 ),
                 # TODO add for 3 treatments different nbr of balls in the beginning
                 # TODO add different polya's urn returns
                 # TODO random relapse
                 conditionalPanel(condition = "input.Version == 'advanced'",
                                  selectInput("Treatments", "Please select number of treatments:",
                                              choices = c("2","3")),
                                  conditionalPanel(condition = "input.Treatments == '2'",
                                                   selectInput("BegBalls", "Do you want to change the rate of each treatment at the beginning?",
                                                               choices = c("No","Yes")),
                                                   conditionalPanel(condition = "input.BegBalls == 'Yes'",
                                                                    textInput("begNbrOfBalls1", "Please select the beginning treatment 1 rate:",
                                                                              1 ),
                                                                    textInput("begNbrOfBalls2", "Please select the beginning treatment 2 rate:",
                                                                              1 )
                                                   ),
                                                   selectInput("ReturnBalls", "Do you want to change the rate of returns for all (or a specific) treatment?",
                                                               choices = c("No","Yes")),
                                                   conditionalPanel(condition = "input.ReturnBalls == 'Yes'",
                                                                    textInput("nbrOfReturns1", "Please select for treatment 1 the number of returns after each patient:",
                                                                              1 ),
                                                                    textInput("nbrOfReturns2", "Please select for treatment 2 the number of returns after each patient:",
                                                                              1 )
                                                   ),
                                                   selectInput("Relapse", "Do you want to add a relapse rate (that happens at random) for all (or a specific) treatment(s)?",
                                                               choices = c("No","Yes")),
                                                   conditionalPanel( condition = "input.Relapse == 'Yes'",
                                                                     sliderInput("nbrOfRelapse1", "Please select for treatment 1 the relapse rate:",
                                                                                 min = 0, max = 10, value = 2, step = 1 ),
                                                                     sliderInput("nbrOfRelapse2", "Please select for treatment 2 the relapse rate:",
                                                                                 min = 0, max = 10, value = 2, step = 1 )
                                                   )
                                                   
                                  )
                 )
            )  
    ),
    
    # Third tab content
    tabItem(tabName = "charts",
            fluidRow(
              box(
                width = 8, title = "Graph",
                plotOutput("myplot")),
              valueBoxOutput("rate"),
              valueBoxOutput("count"),
              valueBoxOutput("users"),
              valueBoxOutput("bestTrt")
            )
    )
  )
 )
)

