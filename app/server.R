library(shiny)

`%then%` <- shiny:::`%OR%`

shinyServer(
	function(input,output){
        
            
        data <- reactive({
                        
            if (is.null(input$file1)) return(example)
                else{
                    dr <- read.csv(input$file1$datapath)
                    dife <- setdiff(c("STUDY_ID","EVENTS_1","EVENTS_2","TOTAL_1","TOTAL_2"),
                                     colnames(dr))
                    #validate input data
                    validate(
                        #do we have all columns?
                        need(length(dife)==0,paste("Missing column(s):",paste(dife,collapse=", "))) %then%
                        #is there at least 2 studies?
                        need(nrow(dr)>1,"Please provide data for at least 2 studies") %then%
                        #is there missing values?
                        need(sum(is.na(dr[,c("STUDY_ID","EVENTS_1","EVENTS_2","TOTAL_1","TOTAL_2")]))==0,
                             "Please provide data without NA values") %then%
                        #are all nb of events and sample size integer?
                        need(is.integer(dr$EVENTS_1)*is.integer(dr$EVENTS_2)*
                             is.integer(dr$TOTAL_1)*is.integer(dr$TOTAL_2)==1,
                             paste("All numbers in ",
                                   paste(c("EVENTS_1","EVENTS_2","TOTAL_1","TOTAL_2")[!c(is.integer(dr$EVENTS_1),
                                                                                         is.integer(dr$EVENTS_2),
                                                                                         is.integer(dr$TOTAL_1),
                                                                                         is.integer(dr$TOTAL_2))],
                                         collapse=", "),
                                   "should be positive integers!",collapse=" ")) %then%
                        #are numbers positive?
                        need(sum(dr$EVENTS_1<0)+sum(dr$EVENTS_2<0)+
                             sum(dr$TOTAL_1<0)+sum(dr$TOTAL_2<0)==0,
                             paste("All numbers in ",
                                   paste(c("EVENTS_1","EVENTS_2","TOTAL_1","TOTAL_2")[c(sum(dr$EVENTS_1<0),
                                                                                        sum(dr$EVENTS_2<0),
                                                                                        sum(dr$TOTAL_1<0),
                                                                                        sum(dr$TOTAL_2<0))!=0],
                                         collapse=", "),
                                   "should be positive!",collapse=" ")) %then%
                        #is there at least 1 event?
                        need(sum(dr$EVENTS_1)+sum(dr$EVENTS_1)!=0,
                             "Please provide data with at least 1 event in at least 1 arm") %then%
                        #total sample sizes should be greater than number of events
                        need(sum(dr$EVENTS_1>dr$TOTAL_1)+sum(dr$EVENTS_1>dr$TOTAL_1)==0,
                             "The number of events should be lower than the sample size!")
                    )
                    return(dr)
                    }

                })
                    
        meta1 <- reactive({
            
            validate(need(input$method!="PETO" | (input$method=="PETO" & input$measure=="OR"),
                          "Peto's method is only possible with Measure = Odds Ratio"))

            metabin(event.e=EVENTS_1, n.e=TOTAL_1, event.c=EVENTS_2, n.c=TOTAL_2,
                    studlab = STUDY_ID,
                    data = data(),
                    method = input$method,
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
            cat(file=stderr(), "statistical significativity set", "\n")
            paste0("Forest plot of a statistically ",ifelse(pval()<0.05,"","non-"),
                   "significant meta-analysis of ",nrow(data()), " trials")
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
            
            validate(need(input$method!="PETO" | (input$method=="PETO" & input$measure=="OR"),
              "Peto's method is only possible with Measure = Odds Ratio"))

            if(pval()<0.05) frag_ma(data(),input$method,input$random,input$measure)
                else frag_ma_ns(data(),input$method,input$random,input$measure)
            })
        
        meta2 <- reactive({
            
            validate(need(input$method!="PETO" | (input$method=="PETO" & input$measure=="OR"),
                          "Peto's method is only possible with Measure = Odds Ratio"))
            
            metabin(event.e=EVENTS_1, n.e=TOTAL_1, event.c=EVENTS_2, n.c=TOTAL_2,
                    studlab = STUDY_ID,
                    data = fragility()[[2]],
                    method = isolate(input$method),
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
            paste0("After ",fragility()[[1]]," specific event-status modification", 
                   ifelse(fragility()[[1]]==1," is","s,")," the conclusion of the meta-analysis was turned to statistically ",
                   ifelse(pval()<0.05,"non-",""),"significant.")
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

