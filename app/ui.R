library(shiny)

shinyUI(pageWithSidebar(
    
  headerPanel("Fragilty Index for meta-analyses"),

  sidebarPanel(
      fileInput("file1", "Upload meta-analysis data as CSV File",
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
               ),

      selectInput(inputId = "method", label = strong("Method"),
                  choices = c("Mantel-Haenszel" = "MH",
                              "Inverse Variance" = "Inverse",
                              "Peto" = "PETO"),
                  selected = "MH"),
      selectInput(inputId = "measure", label = strong("Measure"),
                  choices = c("Odds Ratio" = "OR",
                              "Risk Difference" = "RD",
                              "Risk Ratio" = "RR"),
                  selected = "RR"),
      selectInput(inputId = "random", label = strong("Random effects"),
                  choices = c("Fixed effects" = "NO",
                              "Random effects" = "YES"),
                  selected = "NO"),
      actionButton("compute_fragility", "Compute Fragility Index"),
      width = 3

  ),

  mainPanel(
    plotOutput("initPlot",width = "100%",height = "auto"),
    h5(textOutput("is_significant"),align="center"),
      conditionalPanel(condition = "output.show == true",                       
                       h5(textOutput("fragility_index"),align="center"),
                       tags$div(style = "border: 1px solid black",
                                h4(textOutput("fragility_index2"),align="center")),
                       plotOutput('fragilePlot',width = "100%",height = "auto")
                       ),
      width = 9
  )

))
