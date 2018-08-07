    rm(list = ls())

    bank <- read.csv("bank-full.csv")

    str(bank)

    sapply(bank , function(x) class(x))

    banki <- bank[ , sapply(bank , function(x) class(x)) == "integer" ]

    bankf <- bank[ , sapply(bank , function(x) class(x)) == "factor"]

    cormat <- round(cor(banki),2)

    library(reshape2)

    melted.cormat <- melt(cormat)

    library(ggplot2)

    ggplot(melted.cormat , aes(x = Var1 , y = Var2 , fill = value)) +
      geom_tile()

    getlowertri <- function(x) {
      x[upper.tri(x)] <- NA
      return(x)
    }

    lowertri <- getlowertri(cormat)

    melted.lowertri <- melt(lowertri ,na.rm = T)

    ggplot(melted.lowertri , aes(x=Var1 , y=Var2 , fill=value)) +
         geom_tile(color = "black") + 
     scale_fill_gradient2(low = "red" , high = "green",
                        mid = "white" , midpoint = 0 , 
                        limit = c(-1,1),space = "Lab",
                        name = "correlation")
