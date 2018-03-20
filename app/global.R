library(meta)

example <- read.csv("./data/example.csv")
#Mettre précisions dans readcsv
#Unicode: UTF-8
#Field delimiter: ,
#Text delimiter: "

#data <- example
#method <- "MH"
#random <- "NO"
#measure <- "RR"

#upload fraglity functions
sapply(paste0("functions_fragility/",
              list.files("functions_fragility/")[grep(".R",list.files("functions_fragility/"),
                                                      fixed=TRUE)]),
       source)
#upload app functions
sapply(paste0("functions_app/",list.files("functions_app/")),source)