library(shiny)

shinyUI(
    navbarPage("Fragilty Index of meta-analyses", id = "inTabset",
                       
        tabPanel("About", value = "about",
                 fluidRow(
                     column(6,                                                             
                         h3("The Fragility Index of meta-analyses"),
                         tags$p("The conclusion of a meta-analysis of a binary outcome could be changed if the outcomes of a few patients were modified within the randomized controlled trials included in it."),
                         tags$p(tags$strong("The Fragility Index for meta-analyses"), 
                                " of randomized controlled trials is an intuitive measure for the confidence we have in the conclusions of a meta-analysis."),
                         tags$p("It is defined as the ",
                                tags$em("minimum number of patients"),
                                " from one or more trials included in the meta-analysis for which a modification on the event status (ie, changing events to non-events, or non-events to events) would change the statistical significance of the pooled treatment effect."),
                         tags$p("After specific event-status modifications, a statistically significant pooled treatment effect could be turned non-significant, and a statistically non-significant treatment effect could be turned significant."),

                         h3("Evaluate the Fragility Index for your own meta-analysis"),
                         tags$p(actionLink('jumpToFragility', "Evaluate the fragility"),
                                " of the conclusions of your own meta-analysis!"),
                         h3("How does it work"),
                         tags$p("The method used to evaluate the Fragility Index of meta-analyses is based on an iterative re-evaluation of the statistical significance of the pooled treatment effect of modified meta-analyses, iteratively derived from the original meta-analysis by performing single event-status modifications in each arm of each trial in turn."),   
                         tags$p("A schematic represention is available ",actionLink('jumpToMethod', "here"),". You can also check our code on ",
                                tags$a(href="https://github.com/iatal/fragility_ma/tree/master/app/functions_fragility",
                                       "Github.")),
                         tags$p(""),
                         tags$p("Contact: ignacioatal--at--gmail--dot--com")
                            ),
                     column(6,
                            HTML("<center><h4><u>Example of a statistically significant meta-analysis with Fragility Index = 2</u></h4></center>"),
                            tags$p(""),
                            HTML("<center><img src='schema_exemple_plain2.svg' alt='' width='600px' /></center>")                            
                            )
                     )
                 ),
        
        
        tabPanel("Evaluate the Fragility", value = "fragility",
                 h3("Evaluate the Fragility Index for your own meta-analysis"),
                 sidebarPanel(
                     tags$p(tags$strong("1- Upload meta-analysis data as CSV File")),
                     tags$em("(See ", actionLink('jumpToData', 'data requirements'), " for help)"),
                     fileInput("file1", label = NULL,
                               accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                     tags$p(tags$strong("2- Select the parameters of the meta-analysis")),
                     selectInput(inputId = "method", label = tags$p(tags$u("Method:"),
                                                                    style = "font-weight:normal;"),
                                 choices = c("Mantel-Haenszel" = "MH",
                                             "Inverse Variance" = "Inverse",
                                             "Peto" = "PETO"),
                                 selected = "MH"),
                     selectInput(inputId = "measure", label = tags$p(tags$u("Measure:"),
                                                                    style = "font-weight:normal;"),
                                 choices = c("Odds Ratio" = "OR",
                                             "Risk Difference" = "RD",
                                             "Risk Ratio" = "RR"),
                                 selected = "RR"),
                     selectInput(inputId = "random", label = tags$p(tags$u("Random effects:"),
                                                                    style = "font-weight:normal;"),
                                 choices = c("Fixed effects" = "NO",
                                             "Random effects" = "YES"),
                                 selected = "NO"),
                     tags$p(tags$strong("3- Evaluate the Fragility Index")),
                     actionButton("compute_fragility", "Evaluate the Fragility Index!"),
                     width = 3),
                 
                 mainPanel(
                     h4(textOutput("is_significant"),align="center"),
                     plotOutput("initPlot",width = "100%",height = "auto"),
                     tags$hr(),
#                     conditionalPanel(condition="$('html').hasClass('shiny-busy') & output.show == false",
#                                      tags$div(h4("Loading..."),id="loadmessage")),
                     conditionalPanel(condition = "output.show == true",       
                                      tags$div(style = "border: 1px solid black",
                                               h4(textOutput("fragility_index"),align="center"),
                                               h3(textOutput("fragility_index2"),align="center")),
                                      plotOutput('fragilePlot',width = "100%",height = "auto"),
                                      tags$hr(),
                                      htmlOutput("modifications_text")),
                     width = 9)
                 ),

        tabPanel("Data requirements", value = "data",
                 tags$h3("Data requirements"),
                 tags$p("The data you upload should be a CSV file looking like this:"),
                 tableOutput('ex_table'),
                 tags$p("Where the following headers need to be present:"),
                 tags$ul(tags$li(tags$b("STUDY_ID: "),"an ID for each trial in the meta-analysis"),
                         tags$li(tags$b("EVENTS_1: "),"the number of events in Arm 1"),
                         tags$li(tags$b("TOTAL_1: "),"the total number of patients included in Arm 1"),
                         tags$li(tags$b("EVENTS_2: "),"the number of events in Arm 2"),
                         tags$li(tags$b("TOTAL_2: "),"the total number of patients included in Arm 2")),
                 tags$p('Fields in the CSV should be separated with a comma (,), and text should be delimited with quotation marks (")'),
                 tags$p(actionLink('jumpToFragility2', "Evaluate the Fragility Index"),
                        " of your own meta-analysis!")
                 ),
        
        tabPanel("Method", value = "method",
                 tags$h3("Schemamic representation of the iterative method used to evaluate the Fragility Index of a meta-analysis"),
                 HTML("<img src='schema_algorithm_v4_plain2.svg' alt='' width='1000px' />")
                 )
               #,
        
#        tabPanel("References",
#                 tags$h3("References"),
#                 tags$p("Walsh M, Srinathan SK, McAuley DF, et al. The statistical significance of randomized controlled trial results is frequently fragile: a case for a Fragility Index. J Clin Epidemiol. 2014 Jun;67(6):622-8. doi: 10.1016/j.jclinepi.2013.10.019. ",
#                        tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/24508144","PubMed PMID:24508144."))                 
#                 )
    
        )
)
