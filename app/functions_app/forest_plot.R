##############################################################################                               
#forest plots

forest_plot <- function(meta1, fragile=FALSE, modifs = NULL){
    
    cex <- 1.5
    n <- length(meta1$event.e)

    font_std <- rep(1,n)
    font_ev1 <- rep(1,n)
    font_ev2 <- rep(1,n)
    
    if(fragile==TRUE){
        font_std[modifs$EVENTS_1!=0 | modifs$EVENTS_2!=0] <- 2
        font_ev1[modifs$EVENTS_1!=0] <- 2
        font_ev2[modifs$EVENTS_2!=0] <- 2
        }

    if(meta1$sm=="RR") meas <- "Risk Ratio"
    if(meta1$sm=="RD") meas <- "Risk Difference"
    if(meta1$sm=="OR") meas <- "Odds Ratio"
    
    par(mar = c(3,1,1,0))
    layout(matrix(c(1,1,1,1,1,1,1,1,2,2,2), nrow = 1))

    plot(c(-1,0.85),c(-0.5,n+2.5),type = "n", yaxt = "n", xaxt = "n", bty="n")
    text("Arm A", x= -0.4, y = n+2, adj = 0.5, font = 2,cex=cex)
    text("Arm B", x= 0, y = n+2, adj = 0.5, font = 2,cex=cex)
    text("Study", x= -1, y = n+1, adj = 0, font = 2,cex=cex)
    text("Events", x= c(-0.5,-0.1), y = n+1, adj = 1, font = 2,cex=cex)
    text("Totals", x= c(-0.3,0.1), y = n+1, adj = 1, font = 2,cex=cex)    
    text("Weight", x= 0.38, y = n+1, adj = 1, font = 2,cex=cex)    
    text(meta1$studlab, x= -1, y = n:1, adj = 0,cex=cex, font = font_std)
    text(meta1$event.e, x= -0.5, y = n:1, adj = 1,cex=cex, font = font_ev1)
    text(meta1$n.e, x= -0.3, y = n:1, adj = 1,cex=cex)
    text(meta1$event.c, x= -0.1, y = n:1, adj = 1,cex=cex, font = font_ev2)
    text(meta1$n.c, x= 0.1, y = n:1, adj = 1,cex=cex)
    
    if(meta1$comb.fixed==TRUE){
        #Study level weights
        text(paste(rn1(100*meta1$w.fixed/sum(meta1$w.fixed)),"%",sep=""),
                   x= 0.38, y = n:1, adj = 1,cex=cex)
        #Pooled treatment effect
        text(rn2(exp(meta1$TE.fixed)),x= 0.6, y = 0, adj = 1, font = 2,cex=cex)
        #CI Pooled treatment effect
        text(paste("[",as.character(rn2(exp(meta1$lower.fixed))),"--",
                   as.character(rn2(exp(meta1$upper.fixed))),"]",sep=""),
                   x= 0.62, y = 0, adj = 0, font = 2,cex=cex)        
        #Method
        text(paste(meta1$method," Fixed, 95% CI",sep=","),
             x= 0.67, y = n+1, adj = 0.5, font=2,cex=cex)
    }

    if(meta1$comb.random==TRUE){
        #Study level weights
        text(paste(rn1(100*meta1$w.random/sum(meta1$w.random)),"%",sep=""),
                   x= 0.38, y = n:1, adj = 1,cex=cex)
        #Pooled treatment effect
        text(rn2(exp(meta1$TE.random)),x= 0.6, y = 0, adj = 1, font = 2,cex=cex)
        #CI Pooled treatment effect
        text(paste("[",as.character(rn2(exp(meta1$lower.random))),"--",
                   as.character(rn2(exp(meta1$upper.random))),"]",sep=""),
                   x= 0.62, y = 0, adj = 0, font = 2,cex=cex)
        #Method
        text(paste(meta1$method," Random, 95% CI",sep=","),
             x= 0.67, y = n+1, adj = 0.5, font=2,cex=cex)
    }

    #Study level treatment effect estimate
    text(rn2(exp(meta1$TE[!is.na(meta1$TE)])),x= 0.6, y = (n:1)[!is.na(meta1$TE)], adj = 1,cex=cex)
    text(paste("[",as.character(rn2(exp(meta1$lower[!is.na(meta1$TE)]))),"--",
               as.character(rn2(exp(meta1$upper[!is.na(meta1$TE)]))),"]",sep=""),
               x= 0.62, y = (n:1-0.03)[!is.na(meta1$TE)], adj = 0,cex=cex)

    text("Total (95%CI)", x= -1, y = 0, adj = 0, font=2,cex=cex)
    text(sum(meta1$n.e), x= -0.3, y = 0, adj = 1, font = 2,cex=cex)
    text(sum(meta1$n.c), x= 0.1, y = 0, adj = 1, font = 2,cex=cex)
    #Heterogeneity
    mtext(paste(c("Heterogeneity: I2 = ",round(100*meta1$I2),"%"),collapse=""),side = 1,
          at= c(-1, -1), adj = 0, cex=cex*par("cex"))
    
    #Measure
    text(meas, x= 0.67, y = n+2, adj = 0.5, font=2,cex=cex)

    #Forest plot
    par(mar = c(3,0,1,3))
    plot(c(0.01,100),c(-0.5,n+2.5),log = "x", type = "n", yaxt = "n", xaxt = "n",bty = "n")
    axis(1, at=c(0.01,0.1,1,10,100),labels=c("0.01","0.1","1","10","100"),cex.axis=cex)
    text(meas, x= 1, y = n+2, adj = 0.5, font=2,cex=cex)
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
        text(paste(meta1$method," Fixed, 95% CI",sep=","),
             x= 1, y = n+1, adj = 0.5, font=2,cex=cex)        
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
        text(paste(meta1$method," Random, 95% CI",sep=","),
             x= 1, y = n+1, adj = 0.5, font=2,cex=cex)
    }
        
}