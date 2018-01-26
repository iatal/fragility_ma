library(shiny)

shinyServer(
	function(input,output){
        
            
        data <- reactive({
            if (is.null(input$file1)) return(example)
            read.csv(input$file1$datapath)
                })
        
        meta1 <- reactive({
            metabin(event.e=EVENTS_1, n.e=TOTAL_1, event.c=EVENTS_2, n.c=TOTAL_2,
                    studlab = STUDY_ID,
                    data = data(),
                    method = ifelse(input$method=="IV","Inverse",input$method),
                    sm=ifelse(input$measure=="PETO_OR","OR",input$measure),
                    comb.random = input$random=="YES",
                    comb.fixed = input$random=="NO",
                    RR.cochrane = TRUE)
            })
        
        #output$view <- renderTable({
        #    head(data())
        #})
                            
        output$initPlot <- renderPlot({
            forest(meta1(),layout="RevMan5",
                   plotwidth = "15cm")

        })

        pval <- reactive({
            ifelse(input$random=="YES", meta1()$pval.random, meta1()$pval.fixed)
        })

        output$is_significant <- renderText({
            paste0("The meta-analysis is statistically ",ifelse(pval()<0.05,"","non-"),"significant")
        })

        #For hiding the results when someone changes a value    
        values <- reactiveValues(show=TRUE)

        observeEvent(c(input$method,input$measure,input$random,input$file1),{
            values$show <- FALSE
        },priority=10)
            
        observeEvent(input$compute_fragility,{
            values$show <- TRUE
        },priority=10)    

        output$show <- reactive({
            return(values$show)
        })

        #Bouton: calcul de fragilité
        fragility <- eventReactive(input$compute_fragility, {
            if(pval()<0.05) frag_ma(data(),input$method,input$random,input$measure)
                else frag_ma_ns(data(),input$method,input$random,input$measure)
            })
        
        meta2 <- reactive({
            metabin(event.e=EVENTS_1, n.e=TOTAL_1, event.c=EVENTS_2, n.c=TOTAL_2,
                    studlab = STUDY_ID,
                    data = fragility()[[2]],
                    method = isolate(ifelse(input$method=="IV","Inverse",input$method)),
                    sm= isolate(ifelse(input$measure=="PETO_OR","OR",input$measure)),
                    comb.random = isolate(input$random=="YES"),
                    comb.fixed = isolate(input$random=="NO"),
                    RR.cochrane = TRUE)
            })

        
        output$fragility_index <- renderText({
            paste0("Fragility Index is ",fragility()[[1]])
        })
        
        output$fragilePlot <- renderPlot({
            forest(meta2(),layout="RevMan5",
#                   squaresize = 1,
                   plotwidth = "15cm")
        })
                
        outputOptions(output, "show", suspendWhenHidden = FALSE)         
        
})

