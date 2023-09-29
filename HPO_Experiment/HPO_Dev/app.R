library(randomForest)
library(shinycssloaders)

cv.rf <- function(input){
  
  load(input$DataSet)
  
  # Define required objects.
  dat <- cbind(y, X)
  cv_fold <- 10
  
  # Preprocess some factors.
  if (input$replace == "True"){ replace.val <- TRUE }
  if (input$replace == "False"){ replace.val <- FALSE }
  cut.off.val <- c(input$cutoff, 1 - input$cutoff)
  class.wt.value <- c(input$classwt, 1 - input$classwt)
  
  # Calculate the Cross-Validation accuracy.
  cv_index <- rep(1:cv_fold, each = nrow(dat) %/% cv_fold + 1)
  cv_index <- cv_index[1:nrow(dat)]
  cv_index <- cv_index[sample(nrow(dat))]
  cv_accuracy <- 0
  for(l in 1:cv_fold) {
    model <- randomForest(y~., data = dat[cv_index == l, ],
                          ntree = input$ntree, mtry = input$mtry,
                          nodesize = input$nodesize, maxnodes = input$maxnodes,
                          classwt = class.wt.value, cutoff = cut.off.val,
                          replace = replace.val)
    yhat <- predict(model, newdata = dat[cv_index != l, ])
    cv_accuracy <- cv_accuracy + mean(yhat == y[cv_index != l])
  }
  
  cv.response <- cv_accuracy / cv_fold
  
  return(round(cv.response, 5))
}


# Define UI
ui <- fluidPage(
 
  fluidRow(
    column(12,
           includeHTML("HPO_Introduction.html")
    )
  ),  
  
  titlePanel("Random Forest Simulator"),
  h3(textOutput("hello")),
  tags$hr(),
   
 sidebarLayout(#fluid=FALSE,
   sidebarPanel(width=10, fluidPage(
     fluidRow(
       column(3, numericInput("ntree", "ntree", NA, min=100, max = 1000, step = 10, width=150)),
       column(3, radioButtons("replace", label = "replace", choices = c("True", "False"), width=150)),
       column(3, numericInput("mtry", "mtry", NA, min=2, max = 6, step = 1, width=150)),
       column(3, numericInput("nodesize", "nodesize", NA, min=1, max = 11, step = 1, width=150))
     ), fluidRow(
       column(3, numericInput("maxnodes", "maxnodes", NA, min=10, max = 1000, step = 10, width=150)),
       column(3, numericInput("classwt", "classwt", NA, min=0.5, max = 0.9, step = 0.1,width=150)),
       column(3, numericInput("cutoff", "cutoff", NA, min=0.2, max = 0.8, step = 0.1, width=150)),
       column(3, selectInput("DataSet", 
                             label = "Select Data Set", 
                             choices = list("Cardiovascular" = "data/cardiovascular.Rdata", 
                                            "Diabetes" = "data/diabetes.Rdata",
                                            "Heart" = "data/heart.Rdata"),
                             selected = "Cardiovascular",
                             width = "150%"))
     ))),
   mainPanel(width=5,
             fluidRow(textOutput("check")),
             conditionalPanel(condition="output.check == 'Ready to run!'",
                              fluidRow(column(2, br(), actionButton("srun", "Run"))),
                              fluidRow(p("\n")))
   )),
  withSpinner(textOutput("text"))
)

# Server logic
server <- function(input, output) {
  
  output$check <- reactive({
    validate(
      need(input$ntree >= 100 || input$ntree <= 1000, "ntree must be between 100 and 1000.")
    )
    validate(
      need(input$mtry >= 2 || input$mtry <= 6, "mtry must be between 2 and 6.")
    )
    validate(
      need(input$nodesize >= 1 || input$nodesize <= 11, "nodesize must be between 1 and 11.")
    )
    validate(
      need(input$maxnodes >= 10 || input$maxnodes <= 1000, "maxnodes must be between 10 and 1000.")
    )
    validate(
      need(input$classwt >= 0.5 || input$classwt <= 0.9, "classwt must be between 0.5 and 0.9")
    )
    validate(
      need(input$cutoff >= 0.2 || input$cutoff <= 0.8, "cutoff must be between 0.2 and 0.8")
    )
    "Ready to run!"
  })
  
  CF.output <- eventReactive(eventExpr = input$srun, 
                             valueExpr = {paste0("The 10-fold cross-validated accuracy is ", cv.rf(input))})
  
  output$text <- renderText({
    CF.output()  
  })
  
}

# Complete app with UI and server components
shinyApp(ui, server)