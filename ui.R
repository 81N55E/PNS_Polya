shinyUI(
  pageWithSidebar(
    headerPanel("Polya's Urn Simulation"),
    
    sidebarPanel(
      selectInput("Version", "Please select version:",
                  choices = c("simple","intermediate","advanced")),
      
      
      textInput("Number of Patients", "Please select number of patients: ", 50),
      textInput("Number of Simulations", "Please select number of simulations: ", 1),
      
      conditionalPanel(condition = "input.Version == 'intermediate'",
                 textInput("Number of Treatments", "Please select number of treatments: ", 2),
                 sliderInput("Plot3BestTrts", "Please select number of best treatments plotted (max 3):",
                             min = 1, max = 3, value = 2 ,step = 1)
                 ),
      
      conditionalPanel(condition = "input.Version == 'advanced'",
                 textInput("Number of Treatments", "Please select number of treatments: ", 2),
                 selectInput("special1", "Please select Advanced One:",
                             choices = c("Yes","No") ),
                 selectInput("special2", "Please select Advanced Two:",
                             choices = c("Yes","No") )
                 )
       ),
    mainPanel(
      plotOutput("myplot")
    )
  )
)