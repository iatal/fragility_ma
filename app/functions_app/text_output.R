#correct text display for decimal numbers
rn1 <- function(x){
    l <- strsplit(as.character(10*round(x,1)),"")
    sapply(l,function(y){
            if(length(y)==1) y <- c("0",y)
            paste(c(y[-length(y)],".",y[length(y)]),collapse="")
            })
}

rn2 <- function(x){
    l <- strsplit(as.character(100*round(x,2)),"")
    sapply(l,function(y){
        if(length(y)<3) y <- c(rep("0",3-length(y)),y)
        paste(c(y[-c(-1:0+length(y))],".",y[-1:0+length(y)]),collapse="")
            })
}
