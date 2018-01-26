library(shiny)

shinyUI(pageWithSidebar(

  headerPanel("Fragilty Index calculation"),

  sidebarPanel(
      
      fileInput("file1", "Choose CSV File",
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
               ),

      selectInput(inputId = "method", label = strong("Method"),
                  choices = c("MH","IV","PETO"),
                  selected = "MH"),
      selectInput(inputId = "measure", label = strong("Measure"),
                  choices = c("OR","PETO_OR","RD","RR"),
                  selected = "RR"),
      selectInput(inputId = "random", label = strong("Random effects"),
                  choices = c("NO","YES"),
                  selected = "NO"),
      actionButton("compute_fragility", "Compute Fragility")

  ),

    
  mainPanel(
#    tableOutput("view"), 
    plotOutput("initPlot"),
    textOutput("is_significant"),
    conditionalPanel(condition = "output.show == true",
                     wellPanel(textOutput("fragility_index"),
                               plotOutput('fragilePlot'))
                    )
  )
    
))
