#Fragility Function for statistically significant meta-analyses
############################################################################
frag_ma <- function(data,method,random,measure,with_progress = FALSE, progress = NULL){

    #data should have as columns: EVENTS_1, EVENTS_2, TOTAL_1, TOTAL_2
    #method can be: "Inverse", "MH" or "PETO"
    #random can be: "YES" or "NO"
    #measure can be: "RR","RD" or "OR"

    frag <- 0
    ddoi <- data
    nb_test <- 0
    
    init <- rev_ma(ddoi,method,random,measure)
    
    #Significant result < 1
    #We need to either add events to first arm, or supress events from second arm
    
    if(init[2] < 1){
        
        ci_up <- init[2]

        while(ci_up<1){
            #We test modification of upper bound of confidence interval when 
            #adding events to first arm of each study
            
            Ladd <- sapply(1:nrow(ddoi),function(i){
                    dch <- ddoi
                    dch$EVENTS_1[i] <- dch$EVENTS_1[i] + 1
                    if(dch$EVENTS_1[i]>dch$TOTAL_1[i]) return(0)
                    else return(rev_ma(dch,method,random,measure)[2])

                 })
            
            #We test modification of upper bound of confidence interval when 
            #suppressing events to second arm of each study
            Lsupp <- sapply(1:nrow(ddoi),function(i){
                    dch <- ddoi
                    dch$EVENTS_2[i] <- dch$EVENTS_2[i] - 1
                    if(dch$EVENTS_2[i]<0) return(0)
                    else return(rev_ma(dch,method,random,measure)[2])
             })

            #We keep only the addition or suppression of event modifying the most
            #the upper bound of the confidence interval            
            madd <- which.max(Ladd)
            msupp <- which.max(Lsupp)
            if(length(msupp)==0){
                ddoi$EVENTS_1[madd] <- ddoi$EVENTS_1[madd] + 1
                ci_up <- Ladd[madd]
            }
            else{
                if(Ladd[madd] > Lsupp[msupp]) {
                    ddoi$EVENTS_1[madd] <- ddoi$EVENTS_1[madd] + 1
                    ci_up <- Ladd[madd]
                    }
                else {
                    ddoi$EVENTS_2[msupp] <- ddoi$EVENTS_2[msupp] - 1
                    ci_up <- Lsupp[msupp]
                    }                
            }            
            #increment of fragility    
            frag <- frag + 1

            nb_test <- nb_test + 2*nrow(ddoi)

            #Progress function for app                                                
            if(with_progress) {
                progress$inc(2*nrow(ddoi),
                             detail = ifelse(frag < 50,
                                             paste(c("Calculating ",nb_test," new pooled treatment effects"),
                                                   collapse = ""),
                                             paste(c("Wow! Fragility Index superior to 50! Still calculating ",
                                                     nb_test," new pooled treatment effects"),
                                                   collapse = ""))
                             )
                             }
            
        }        
    }
    
    #Significant result > 1
    #We need to either add events to second arm, or supress events from first arm
                        
    if(init[1] > 1){
        
        ci_low <- init[1]

        while(ci_low>1){
            #We test modification of upper bound of confidence interval when 
            #adding events to first arm of each study
            Ladd <- sapply(1:nrow(ddoi),function(i){
                    dch <- ddoi
                    dch$EVENTS_2[i] <- dch$EVENTS_2[i] + 1
                    if(dch$EVENTS_2[i]>dch$TOTAL_2[i]) return(Inf)
                    else return(rev_ma(dch,method,random,measure)[1])
                 })
            #We test modification of upper bound of confidence interval when 
            #suppressing events to second arm of each study
            Lsupp <- sapply(1:nrow(ddoi),function(i){
                    dch <- ddoi
                    dch$EVENTS_1[i] <- dch$EVENTS_1[i] - 1
                    if(dch$EVENTS_1[i]<0) return(Inf)
                    else return(rev_ma(dch,method,random,measure)[1])
             })

            #We keep only the addition or suppression of event modifying the most
            #the upper bound of the confidence interval            
            madd <- which.min(Ladd)
            msupp <- which.min(Lsupp)
            if(length(msupp)==0){
                ddoi$EVENTS_2[madd] <- ddoi$EVENTS_2[madd] + 1
                ci_low <- Ladd[madd]                
            }            
            else{
                if(Ladd[madd] < Lsupp[msupp]) {
                    ddoi$EVENTS_2[madd] <- ddoi$EVENTS_2[madd] + 1
                    ci_low <- Ladd[madd]
                    }
                else {
                    ddoi$EVENTS_1[msupp] <- ddoi$EVENTS_1[msupp] - 1
                    ci_low <- Lsupp[msupp]
                    }
                }

            #increment of fragility    
            frag <- frag + 1
            nb_test <- nb_test + 2*nrow(ddoi)

            #Progress function for app                                    
            if(with_progress) {
                progress$inc(2*nrow(ddoi),
                             detail = ifelse(frag < 50,
                                             paste(c("Calculating ",nb_test," new pooled treatment effects"),
                                                   collapse = ""),
                                             paste(c("Wow! Fragility Index superior to 50! Still calculating ",
                                                     nb_test," new pooled treatment effects"),
                                                   collapse = ""))
                             )
                             }
                        
        }
    }

    return(list(frag,ddoi))

}
