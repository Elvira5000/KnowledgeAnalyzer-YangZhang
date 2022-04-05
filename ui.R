library(shiny)
library(shinyjs)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Knowledge Analyzer"),
  sidebarLayout(
    sidebarPanel(
      #-------------------------------
      # test on starting page
      radioButtons("RBstart","Welcome to the test",
                   selected = T,
                   choiceNames = c("Supply chain manager",
                                   "Security manager",
                                   "Data analyst",
                                   "Project Manager"),
                   choiceValues = 1:4),
      actionButton("start", "start test"),
      
      # ------------------------------
      
      shinyjs::hidden(actionButton("backButton","Prev")),
      shinyjs::hidden(actionButton("goButton", "Next")),
      shinyjs::hidden(actionButton('submitButton', 'Submit')),
      shinyjs::hidden(actionButton("toMain","back")),
      actionButton("scoreButton", "studentScore")
    ),
    mainPanel(shinyjs::hidden(textOutput("question")),
              shinyjs::hidden(uiOutput("answers")),
              shinyjs::hidden(tableOutput("grade")),
              shinyjs::hidden(textOutput("test")),
              shinyjs::hidden(textOutput("test2")),
              shinyjs::hidden(tableOutput("scoretable")),
              shinyjs::hidden(downloadButton("download","download the table")),
              shinyjs::hidden(textInput("nameInput","name")),
              shinyjs::hidden(textInput("passwordInput", "password")),
              shinyjs::hidden(actionButton("loginButton","login"))
    )
  )
)