library(shiny)
library(plotly)
library(ggplot2)
library(reshape2)

#ggplot(per_capita) + aes(x = reorder(State,`2012`), y= `2012`) +xlab("State")+geom_bar(stat="identity") + coord_flip()

#Spreminjanje per capita plač skozi leta (per_capita)
per_capita_t <- per_capita
row.names(per_capita_t) <- per_capita[,1]
per_capita_t <- t(per_capita_t[-1])

top_5 = subset(per_capita_t, select = (colnames(per_capita_t))[c(1:5,c((length(colnames(per_capita_t))-5):length(colnames(per_capita_t))))])
top_5 = melt(top_5,id=row.names(top_5))

graf_top_5 <- ggplot(top_5) + aes(x=Var1, y = value ,colour=Var2) + geom_line() + xlab("Year") + ylab("$ per capita") + geom_point()
######################################################################

