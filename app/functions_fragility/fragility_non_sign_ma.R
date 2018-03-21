#Fragility function for statistically non-significant results
frag_ma_ns <- function(data,method,random,measure,with_progress = FALSE, progress = NULL){    
    
    #data should have as columns: EVENTS_1, EVENTS_2, TOTAL_1, TOTAL_2
    #method can be: "Inverse", "MH" or "PETO"
    #random can be: "YES" or "NO"
    #measure can be: "RR","RD" or "OR"

    ddoi <- data
    frag <- c(0,0)
    ddoiA <- ddoi
    ddoiB <- ddoi
    nb_test <- 0

    init <- rev_ma(ddoi,method,random,measure)

    #We first test if it is possible to have a CI greater than 1
    #In that case we will either add events to first arm, or supress events from second arm

    dtest0 <- ddoi
    dtest0$EVENTS_1 <- dtest0$TOTAL_1
    dtest0$EVENTS_2 <- 0

    #If in the worst case scenario we still overlap 1, fragility is Inf
    if(rev_ma(dtest0,method,random,measure)[1]<1) frag[1] <- Inf

    #If not, we count the minimal number of modifs to have a CI greater than 1     
    else{
      ci_low <- init[1]

      while(ci_low<1){

        #We test modification of lower bound of confidence interval when 
        #adding events to first arm of each study
        Ladd <- sapply(1:nrow(ddoiA),function(i){
                dch <- ddoiA
                dch$EVENTS_1[i] <- dch$EVENTS_1[i] + 1
                if(dch$EVENTS_1[i]>dch$TOTAL_1[i]) return(0)
                else return(rev_ma(dch,method,random,measure)[1])
             })
        #We test modification of lower bound of confidence interval when 
        #suppressing events to second arm of each study
        Lsupp <- sapply(1:nrow(ddoiA),function(i){
                dch <- ddoiA
                dch$EVENTS_2[i] <- dch$EVENTS_2[i] - 1
                if(dch$EVENTS_2[i]<0) return(0)
                else return(rev_ma(dch,method,random,measure)[1])
             })
        #We keep only the addition or suppression of event modifying the most
        #the lower bound of the confidence interval to become the closest to 1
        madd <- which.max(Ladd)
        msupp <- which.max(Lsupp)
        if(length(msupp)==0){
            ddoiA$EVENTS_1[madd] <- ddoiA$EVENTS_1[madd] + 1
            ci_low <- Ladd[madd]
            }
        else{            
            if(Ladd[madd] > Lsupp[msupp]) {
                ddoiA$EVENTS_1[madd] <- ddoiA$EVENTS_1[madd] + 1
                ci_low <- Ladd[madd]
                }
            else {
                ddoiA$EVENTS_2[msupp] <- ddoiA$EVENTS_2[msupp] - 1
                ci_low <- Lsupp[msupp]
                }                
        }            
        #increment of fragility    
        frag[1] <- frag[1] + 1
        
        nb_test <- nb_test + 2*nrow(ddoi)
                    
        #Progress function for app            
        if(with_progress) {
            progress$inc(2*nrow(ddoi),
                         detail = ifelse(frag[1] < 50,
                                         paste(c("Calculating ", nb_test," new pooled treatment effects"),
                                               collapse = ""),
                                         paste(c("The Fragility Index may be superior to 50... Still calculating ",
                                                 nb_test," new pooled treatment effects"),
                                               collapse = ""))
                         )
                         }
                    

        }

     }       

    #We secondly test if it is possible to have a CI lower than 1
    #In that case we will either add events to second arm, or supress events from first arm

    dtest0 <- ddoi
    dtest0$EVENTS_1 <- 0
    dtest0$EVENTS_2 <- ddoi$TOTAL_2

    #If in the worst case scenario we still overlap 1, fragility is Inf
    if(rev_ma(dtest0,method,random,measure)[2]>1) frag[2] <- Inf

    #If not, we count the minimal number of modifs to have a CI greater than 1     
    else{
      ci_up <- init[2]

      while(ci_up>1){

        #We test modification of upper bound of confidence interval when 
        #adding events to second arm of each study
        Ladd <- sapply(1:nrow(ddoiB),function(i){
                dch <- ddoiB
                dch$EVENTS_2[i] <- dch$EVENTS_2[i] + 1
                if(dch$EVENTS_2[i]>dch$TOTAL_2[i]) return(Inf)
                else return(rev_ma(dch,method,random,measure)[2])

             })
        #We test modification of upper bound of confidence interval when 
        #suppressing events to first arm of each study
        Lsupp <- sapply(1:nrow(ddoiB),function(i){
                dch <- ddoiB
                dch$EVENTS_1[i] <- dch$EVENTS_1[i] - 1
                if(dch$EVENTS_1[i]<0) return(Inf)
                else return(rev_ma(dch,method,random,measure)[2])
             })
        #We keep only the addition or suppression of event modifying the most
        #the upper bound of the confidence interval to become the closest to 1
        madd <- which.min(Ladd)
        msupp <- which.min(Lsupp)
        if(length(msupp)==0){
            ddoiB$EVENTS_2[madd] <- ddoiB$EVENTS_2[madd] + 1
            ci_up <- Ladd[madd]
            }
        else{            
            if(Ladd[madd] < Lsupp[msupp]) {
                ddoiB$EVENTS_2[madd] <- ddoiB$EVENTS_2[madd] + 1
                ci_up <- Ladd[madd]
                }
            else {
                ddoiB$EVENTS_1[msupp] <- ddoiB$EVENTS_1[msupp] - 1
                ci_up <- Lsupp[msupp]
                }
            }
        #increment of fragility    
        frag[2] <- frag[2] + 1   
        nb_test <- nb_test + 2*nrow(ddoi)

        if(frag[2] > frag[1]) break
            
        #Progress function for app
        if(with_progress) {
            progress$inc(2*nrow(ddoi),
                         detail = ifelse(frag[2] < 50,
                                         paste(c("Calculating ", nb_test," new pooled treatment effects"),
                                               collapse = ""),
                                         paste(c("Wow! Fragility Index is be superior to 50! Still calculating ",
                                                 nb_test," new pooled treatment effects"),
                                               collapse = ""))
                         )
                         }

        }

     }

    return(list(min(frag),list(ddoiA,ddoiB)[[which.min(frag)]]))

}  
