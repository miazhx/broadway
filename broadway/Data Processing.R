# 
library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)

broadway <- fread(file = "./broadway.csv")
head(broadway)


broadway.count <- broadway%>%select(.,Show.Name)%>%group_by(.,Show.Name)%>%
  summarise(.,count=n())%>%arrange(desc(count))%>%top_n(20)
head(broadway.count)
ggplot(data = broadway.count,aes(x = reorder(Show.Name, -count), y = count)) + geom_bar(stat="identity")

