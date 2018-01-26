library(meta)

example <- read.csv("./data/example.csv")
#Mettre précisions dans readcsv
#Unicode: UTF-8
#Field delimiter: ,
#Text delimiter: "

#Revman Meta-analysis
rev_ma <- function(data,method,random,measure){

    dbin <- data
    if(measure!="RD") dbin <- dbin[dbin$EVENTS_1+dbin$EVENTS_2!=0,]
    if(nrow(dbin)==0) return(NULL)
    else{
        meta1 <- metabin(event.e=EVENTS_1, n.e=TOTAL_1, event.c=EVENTS_2, n.c=TOTAL_2,
                         data = dbin,
                         method = ifelse(method=="IV","Inverse",method),
                         sm=ifelse(measure=="PETO_OR","OR",measure),
                         RR.cochrane = TRUE)

        #For fragility, we calculate the exponnential of the CI, even if measure = RD
        #In order to have allways a comparison of CI to 1
        res <- ifelse(random=="NO",list(exp(c(meta1$lower.fixed,meta1$upper.fixed))),
                                   list(exp(c(meta1$lower.random,meta1$upper.random))))

        return(unlist(res))
        }
}

#data <- example
#method <- "MH"
#random <- "NO"
#measure <- "RR"
        
#Fragility Function
############################################################################
frag_ma <- function(data,method,random,measure){

    frag <- 0
    ddoi <- data
    
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
        }
    }

    return(list(frag,ddoi))

}

        
#Fragility function for statistically non-significant results
frag_ma_ns <- function(data,method,random,measure){    
    
    ddoi <- data
    frag <- c(0,0)
    ddoiA <- ddoi
    ddoiB <- ddoi

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

        }

     }

    return(list(min(frag),list(ddoiA,ddoiB)[[which.min(frag)]]))

}        
        