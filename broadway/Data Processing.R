# 
library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
library(tidytext)
library(tidygraph)
library(wordcloud2)
library(glue)
library(visNetwork)

library(paletteer)
library("wesanderson")

back_col <- paletteer_d(ghibli, MarnieLight1)[2]

library("extrafont")
fonttable()
windowsFonts("Courier" = windowsFont("Courier"))


broadway <- fread(file = "./broadway.csv")
theatre <- fread(file = "./theatre.csv")
broadway <- broadway%>%mutate(.,typecolor=case_when(Show.Type == "Musical" ~ "black",
                                                    Show.Type == "Play" ~ "#E00008",
                                                    Show.Type == "Special" ~ "#858B8E"))
head(theatre)


# Running Weeks
broadway.count <- broadway%>%select(.,Show.Name,typecolor)%>%group_by(.,Show.Name)%>%
  summarise(.,count=n(),Type.Color=first(typecolor))%>%arrange(desc(count))%>%top_n(20, count)
broadway.count
# ggplot(data = broadway.count,aes(x = reorder(Show.Name, -count), y = count)) + geom_bar(stat="identity")


# Prices 
broadway.price <- broadway%>%filter(Date.Year==2014)%>%sample_n(500)%>%
  mutate(Price=Statistics.Gross/Statistics.Attendance)
broadway.price

ggplot(broadway.price, aes(x = factor(Date.Month), y = Price)) +
  geom_jitter(aes(size = Price, fill = Show.Type),
              width = 0.1, alpha = 0.6, shape = 21) +
  geom_boxplot(aes(fill = Price), colour = "white", width = 0.4,
               outlier.shape = NA, alpha = 0.4) + 
  coord_flip()+ theme(legend.position="bottom")+
  scale_color_paletteer_d(wesanderson, Royal2, direction = -1) +
  scale_fill_paletteer_d(wesanderson, Royal2, direction = -1) +
  labs(x = "", y = "Price") +
  theme(text = element_text(colour = "white", family = "AnimeAce"),
        plot.title = element_text(family = "AnimeAceBold", size = rel(1.5)),
        plot.caption = element_text(size = rel(0.7)),
        plot.background = element_rect(fill = back_col),
        panel.background = element_rect(fill = back_col),
        panel.grid = element_line(colour = "gray40"),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(colour = "white"),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = back_col),
        legend.box.background = element_rect(fill = back_col),
        legend.key = element_rect(fill = back_col,  colour = back_col),
        legend.title = element_text(size = rel(0.8)))

# theatres 

theatrelist <- c("American Airlines","Booth","Lyceum","Cort","Brooks Atkinson")

theatre %>%
  filter(Show.Theatre %in% theatrelist) %>%
  as_tbl_graph()%>%
  mutate(color.background = if_else(name %in% theatrelist, "#E00008", "black"),
         color.border = if_else(name %in% theatrelist, "#E00008", "black"),
  label = name,
  labelHighlightBold = TRUE,
  size = if_else(name == "American Airlines", 70, 25),
  font.face = "Courier",
  font.size = if_else(name == "American Airlines", 70, 40),
  font.color = if_else(name %in% theatrelist, "#E00008", "black"),
  shape = if_else(name %in% theatrelist, "icon", "dot"),
  icon.face = "FontAwesome",
  icon.code = "f004",
  icon.size = if_else(name == "American Airline", 200, 100),
  icon.color = if_else(name %in% theatrelist, "#E00008", "black")) %>%
  activate(edges) %>%
  mutate(scaling.max = 20)



