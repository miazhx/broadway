# 
library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)

broadway <- fread(file = "./broadway.csv")
head(broadway)


broadway.count <- broadway%>%select(.,Show.Name)%>%group_by(.,Show.Name)%>%
  summarise(.,count=n())%>%arrange(desc(count))
head(broadway.count)
ggplot(data = broadway.count,aes(x = reorder(Show.Name, -count), y = count)) + geom_bar(stat="identity")


ggplot(data = broadway.count,aes(x = reorder(Show.Name, -count), y = count)) + 
  geom_bar(stat="identity",show.legend = FALSE) +
  geom_hline(aes(yintercept = 250), linetype = 2, show.legend = FALSE) +
  geom_text(data = broadway.count, aes(label = Show.Name), angle = 90, size = 3.8, colour = "white",
            hjust = 1.1, family = "Courier") +
  labs(x = "Track", y = "Running Weeks") +
  theme_minimal() +
  theme(text = element_text(family = "Courier"),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(colour = "black", size = 1),
        axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_line(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

