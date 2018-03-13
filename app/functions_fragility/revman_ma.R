#Revman Meta-analysis
rev_ma <- function(data,method,random,measure){

    #data should have as columns: EVENTS_1, EVENTS_2, TOTAL_1, TOTAL_2
    #method can be: "Inverse", "MH" or "PETO"
    #random can be: "YES" or "NO"
    #measure can be: "RR","RD" or "OR"
    
    dbin <- data
    if(nrow(dbin)==0) return(NULL)
    else{
        meta1 <- metabin(event.e=EVENTS_1, n.e=TOTAL_1, event.c=EVENTS_2, n.c=TOTAL_2,
                         data = dbin,
                         method = method,
                         sm = measure,
                         RR.cochrane = TRUE)

        #For fragility, we calculate the exponnential of the CI, even if measure = RD
        #In order to have allways a comparison of CI to 1
        res <- ifelse(random=="NO",list(exp(c(meta1$lower.fixed,meta1$upper.fixed))),
                                   list(exp(c(meta1$lower.random,meta1$upper.random))))

        return(unlist(res))
        }
        
}
