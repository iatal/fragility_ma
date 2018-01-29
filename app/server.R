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
                    sm=input$measure,
                    comb.random = input$random=="YES",
                    comb.fixed = input$random=="NO",
                    RR.cochrane = TRUE)
            })
                                    
        observe({
            output$initPlot <- renderPlot({
                forest_plot(meta1())
                }, height = 200*nrow(data())/7 + 40)})

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
                    sm= isolate(input$measure),
                    comb.random = isolate(input$random=="YES"),
                    comb.fixed = isolate(input$random=="NO"),
                    RR.cochrane = TRUE)
            })

        #modifs_frag <- reactive({
        modifs_frag <- reactiveValues(val = NULL)
                
        observeEvent(fragility(),{
            modifs_frag$val <- data()[,c("EVENTS_1","EVENTS_2")] - fragility()[[2]][,c("EVENTS_1","EVENTS_2")]
        })

        output$fragility_index <- renderText({
            paste0("and ",fragility()[[1]]," event modification", 
                   ifelse(fragility()[[1]]==1," is","s are")," needed to change the conclusion.")
        })
        output$fragility_index2 <- renderText({
            paste0("Fragility index = ",fragility()[[1]])
        })
                
        
        observe({
            output$fragilePlot <- renderPlot({
                forest_plot(meta2(),fragile = TRUE, modifs = modifs_frag$val)
                }, height = 200*nrow(data())/7 + 40)})
                
        outputOptions(output, "show", suspendWhenHidden = FALSE)         
        
})

