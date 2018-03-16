library(shiny)

shinyUI(
    navbarPage(
        "Fragilty Index of meta-analyses",
        #titlePanel("Fragilty Index for meta-analyses"),
        
        tabPanel("About",
                 h3("The Fragility Index of meta-analyses"),
                 tags$p("The conclusion of a meta-analysis of a binary outcome could be changed if the outcomes of a few patients were modified within the RCTs included in it."),
                 tags$p(tags$strong("The Fragility Index for meta-analyses"), 
                        " of randomized controlled trials is an intuitive measure for the confidence we have in the conclusions of a meta-analysis."),
                 tags$p("It is defined as the ",
                        tags$em("minimum number of patients"),
                        " from one or more trials included in the meta-analysis for which a modification on the event status (ie, changing events to non-events, or non-events to events) would change the statistical significance of the pooled treatment effect. After specific event-status modifications, a statistically significant pooled treatment effect could be turned non-significant, and a statistically non-significant treatment effect could be turned significant."),
                 
                 h3("Evaluate the Fragility Index for your own meta-analysis"),
                 tags$p("Go to the tab ", tags$em("Evaluate the Fragility")," to evaluate the fragility of the conclusions of your own meta-analysis!"),
                 h3("How does it work"),
                 tags$p("The method used to evaluate the Fragility Index of meta-analyses is schematically represented in the tab ",
                        tags$em("Method"),". You can also check our code on ",
                        tags$a(href="https://github.com/iatal/fragility_ma/tree/master/app/functions_fragility",
                               "Github.")),
                 tags$p(""),
                 tags$p("Contact: ignacioatal--at--gmail--dot--com")
                 
                 ),
        
        
        tabPanel("Evaluate the Fragility",
                 h3("Evaluate the Fragility Index for your own meta-analysis"),
                 sidebarPanel(
                     fileInput("file1", "1- Upload meta-analysis data as CSV File",
                               accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                     tags$p(tags$strong("2- Select the parameters of the meta-analysis")),
                     selectInput(inputId = "method", label = "Method",
                                 choices = c("Mantel-Haenszel" = "MH",
                                             "Inverse Variance" = "Inverse",
                                             "Peto" = "PETO"),
                                 selected = "MH"),
                     selectInput(inputId = "measure", label = "Measure",
                                 choices = c("Odds Ratio" = "OR",
                                             "Risk Difference" = "RD",
                                             "Risk Ratio" = "RR"),
                                 selected = "RR"),
                     selectInput(inputId = "random", label = "Random effects",
                                 choices = c("Fixed effects" = "NO",
                                             "Random effects" = "YES"),
                                 selected = "NO"),
                     tags$p(tags$strong("3- Evaluate the Fagility Index")),
                     actionButton("compute_fragility", "Evaluate the Fagility Index!"),
                     width = 3),
                 
                 mainPanel(
                     h4(textOutput("is_significant"),align="center"),
                     plotOutput("initPlot",width = "100%",height = "auto"),
                     tags$hr(),
                     conditionalPanel(condition = "output.show == true",       
                                      tags$div(style = "border: 1px solid black",
                                               h4(textOutput("fragility_index"),align="center"),
                                               h3(textOutput("fragility_index2"),align="center")),
                                      plotOutput('fragilePlot',width = "100%",height = "auto")),
                     width = 9)
                 ),
        
        tabPanel("Method",
                 tags$h3("Schemamic representation of the iterative method used to evaluate the Fragility Index of a meta-analysis"),
                 HTML("<img src='schema_algorithm_v4_plain2.svg' alt='' width='1000px' />")
                 ),
        
        tabPanel("References",
                 tags$h3("References"),
                 tags$p("Walsh M, Srinathan SK, McAuley DF, et al. The statistical significance of randomized controlled trial results is frequently fragile: a case for a Fragility Index. J Clin Epidemiol. 2014 Jun;67(6):622-8. doi: 10.1016/j.jclinepi.2013.10.019. ",
                        tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/24508144","PubMed PMID:24508144."))
                 
                 )
        )
)
