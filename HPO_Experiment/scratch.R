ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

fluidPage(
  titlePanel("Hyperparameter Optimization of Random Forest"),
  h3(textOutput("hello")),
  tags$hr(),
  #conditionalPanel(condition = "output.login == false", tags$hr()),
  conditionalPanel(condition = "output.budget > 0", 
                   sidebarLayout(#fluid=FALSE,
                     sidebarPanel(width=7, fluidPage(
                       fluidRow(
                         column(3, numericInput("N", "N", NA, min=0, width=150)),
                         column(3, numericInput("P", "P", NA, min=0, width=150)),
                         column(3, numericInput("K", "K", NA, min=0, width=150)),
                         column(3, numericInput("Na", "Na", NA, min=0, width=150))
                       ), fluidRow(
                         column(3, numericInput("Ca", "Ca", NA, min=0, width=150)),
                         column(3, numericInput("Mg", "Mg", NA, min=0, width=150)),
                         column(3, numericInput("Nx", "Nx", NA, min=0, width=150)),
                         column(3, sliderInput("reps", "Reps", min=1, max=10, value=1))#,
                       ))),
                     mainPanel(width=3,
                               fluidRow(textOutput("check")),
                               conditionalPanel(condition="output.check == 'Ready to run!'",
                                                fluidRow(p("Note: there is no option to undo.")),
                                                fluidRow(column(2, br(), actionButton("srun", "Run"))))
                     ))
  ),
  #DT::dataTableOutput("yield.data", width = 300),
  conditionalPanel(condition = "output.budget > -101",
                   downloadButton("downloadData", "Download"))
)


cv.rf <- function(input){
  load(input$DataSet)
  cv.response <- input$ntree + input$mtry
  
  return(cv.response)
}