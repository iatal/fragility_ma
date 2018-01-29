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
                         sm = measure,
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
                        
                        
##############################################################################                        
#text output
rn1 <- function(x){
    l <- strsplit(as.character(10*round(x,1)),"")
    sapply(l,function(y){
        if(length(y)==1) y <- c("0",y)
        paste(c(y[-length(y)],".",y[length(y)]),collapse="")})
}

rn2 <- function(x){
    l <- strsplit(as.character(100*round(x,2)),"")
    sapply(l,function(y){
        if(length(y)<3) y <- c(rep("0",3-length(y)),y)
        paste(c(y[-c(-1:0+length(y))],".",y[-1:0+length(y)]),collapse="")})
}

           
#forest plots
                        
forest_plot <- function(meta1, fragile=FALSE, modifs = NULL){
    
    cex <- 1.2
    n <- length(meta1$event.e)

    font_std <- rep(1,n)
    font_ev1 <- rep(1,n)
    font_ev2 <- rep(1,n)
    
    if(fragile==TRUE){
        font_std[modifs$EVENTS_1!=0 | modifs$EVENTS_2!=0] <- 2
        font_ev1[modifs$EVENTS_1!=0] <- 2
        font_ev2[modifs$EVENTS_2!=0] <- 2
        }
    
    par(mar = c(2,1,1,1))
    layout(matrix(c(1,1,1,1,1,1,1,2,2,2), nrow = 1))

    plot(c(-1,1),c(-0.5,n+2.5),type = "n", yaxt = "n", xaxt = "n", bty="n")
    text("Arm A", x= -0.3, y = n+2, adj = 1, font = 2,cex=cex)
    text("Arm B", x= 0.1, y = n+2, adj = 1, font = 2,cex=cex)
    text("Study", x= -1, y = n+1, adj = 0, font = 2,cex=cex)
    text("Events", x= c(-0.5,-0.1), y = n+1, adj = 1, font = 2,cex=cex)
    text("Totals", x= c(-0.3,0.1), y = n+1, adj = 1, font = 2,cex=cex)    
    text("Weight", x= 0.4, y = n+1, adj = 1, font = 2,cex=cex)    
    text(meta1$studlab, x= -1, y = n:1, adj = 0,cex=cex, font = font_std)
    text(meta1$event.e, x= -0.5, y = n:1, adj = 1,cex=cex, font = font_ev1)
    text(meta1$n.e, x= -0.3, y = n:1, adj = 1,cex=cex)
    text(meta1$event.c, x= -0.1, y = n:1, adj = 1,cex=cex, font = font_ev2)
    text(meta1$n.c, x= 0.1, y = n:1, adj = 1,cex=cex)
    
    if(meta1$comb.fixed==TRUE){
        text(paste(rn1(100*meta1$w.fixed/sum(meta1$w.fixed)),"%",sep=""),
                   x= 0.4, y = n:1, adj = 1,cex=cex)
        text(rn2(exp(meta1$TE.fixed)),x= 0.6, y = 0, adj = 1, font = 2,cex=cex)
        text(paste("[",as.character(rn2(exp(meta1$lower.fixed))),"--",
                   as.character(rn2(exp(meta1$upper.fixed))),"]",sep=""),
                   x= 0.62, y = 0, adj = 0, font = 2,cex=cex)        
        text("Fixed", x= 0.8, y = n+1, adj = 0, font=2,cex=cex)
    }

    if(meta1$comb.random==TRUE){
        text(paste(rn1(100*meta1$w.random/sum(meta1$w.random)),"%",sep=""),
                   x= 0.4, y = n:1, adj = 1,cex=cex)
        text(rn2(exp(meta1$TE.random)),x= 0.6, y = 0, adj = 1, font = 2,cex=cex)
        text(paste("[",as.character(rn2(exp(meta1$lower.random))),"--",
                   as.character(rn2(exp(meta1$upper.random))),"]",sep=""),
                   x= 0.62, y = 0, adj = 0, font = 2,cex=cex)        
        text("Random", x= 0.8, y = n+1, adj = 0, font=2,cex=cex)
    }

    text(rn2(exp(meta1$TE)),x= 0.6, y = n:1, adj = 1,cex=cex)
    text(paste("[",as.character(rn2(exp(meta1$lower))),"--",
               as.character(rn2(exp(meta1$upper))),"]",sep=""),
               x= 0.62, y = n:1-0.03, adj = 0,cex=cex)

    text("Total (95%CI)", x= -1, y = 0, adj = 0, font=2,cex=cex)
    text(sum(meta1$n.e), x= -0.3, y = 0, adj = 1, font = 2,cex=cex)
    text(sum(meta1$n.c), x= 0.1, y = 0, adj = 1, font = 2,cex=cex)

    text(meta1$sm, x= 0.6, y = n+2, adj = 0, font=2,cex=cex)
    text(meta1$method, x= 0.6, y = n+1, adj = 0, font=2,cex=cex)

    plot(c(0.01,100),c(-0.5,n+2.5),log = "x", type = "n", yaxt = "n", xaxt = "n",bty = "n")
    axis(1, at=c(0.01,0.1,1,10,100),labels=c("0.01","0.1","1","10","100"),cex.axis=cex)
    text(meta1$sm, x= 1, y = n+2, adj = 0, font=2,cex=cex)
    text(meta1$method, x= 1, y = n+1, adj = 0, font=2,cex=cex)
    lines(x = c(1,1), y = c(-1,0.5+n))
    
    points(x = exp(meta1$TE), 
           y = n:1,pch = 3)
    segments(x0 = exp(meta1$upper), x1 = exp(meta1$lower), y0 = n:1, y1 = n:1)

    if(meta1$comb.fixed==TRUE){
        lines(x = exp(c(meta1$TE.fixed,meta1$TE.fixed)), y = c(-1,0.5+n), lty = 3)
        msq <- max(meta1$w.fixed)
        rect(xleft = exp(meta1$TE - (meta1$w.fixed/msq)*0.4), 
             ybottom = n:1 - (meta1$w.fixed/msq)*0.48,
             xright = exp(meta1$TE + (meta1$w.fixed/msq)*0.4), 
             ytop = n:1 + (meta1$w.fixed/msq)*0.45, col = "blue")
        polygon(x =exp(c(meta1$TE.fixed,meta1$lower.fixed,meta1$TE.fixed,meta1$upper.fixed)), 
                y = c(-0.2,0,0.2,0), col="black")        
    }
    
    if(meta1$comb.random==TRUE){
        lines(x = exp(c(meta1$TE.random,meta1$TE.random)), y = c(-1,0.5+n), lty = 3)
        msq <- max(meta1$w.random)
        rect(xleft = exp(meta1$TE - (meta1$w.random/msq)*0.4), 
             ybottom = n:1 - (meta1$w.random/msq)*0.48,
             xright = exp(meta1$TE + (meta1$w.random/msq)*0.4), 
             ytop = n:1 + (meta1$w.random/msq)*0.45, col = "blue")
        polygon(x =exp(c(meta1$TE.random,meta1$lower.random,meta1$TE.random,meta1$upper.random)), 
                y = c(-0.2,0,0.2,0), col="black")        
    }
    
}