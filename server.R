#function library
library(shiny)
library(data.table)
library(tidyverse)
library(tidytext)
library(tidygraph)
library(wordcloud2)
library(visNetwork)
library(rsconnect)
library(DT)


# rsconnect
rsconnect::setAccountInfo(name='miazhx', token='183A6BC278ABA1062FB3E91D49643CF2', secret='ac3bIPDCYCNzH9/rrFeFpeUqSmp+Iwk23ZMviBS7')


#color library
pal <- c("black", "#E00008", "#858B8E", "white")
back_col <- "#2B2522"

#font library
# library("extrafont")
# fonttable()


# read data 
broadway <- fread(file = "./broadway.csv")
theatre <- fread(file = "./theatre.csv")
theatre1 <- theatre%>%select(Show.Theatre,Show.New.Name)
Database <- fread(file = "./database.csv")
reviews <- readr::read_csv("reviews.csv")

# future work - add color to show type
# broadway <- broadway%>%mutate(.,typecolor=case_when(Show.Type == "Musical" ~ "black",
#                                                     Show.Type == "Play" ~ "#E00008",
#                                                     Show.Type == "Special" ~ "#858B8E"))

# tidy text
reviews_tidy <- reviews %>% unnest_tokens(word, reviews)
word_counts <- reviews_tidy  %>% select(word) %>% anti_join(stop_words, by = "word") %>% count(word, sort = TRUE) 


#server 
server <- function(input, output) {
    
    # running weeks data
    broadway.count <- reactive({
        req(input$showtype)
        filter(broadway, Show.Type %in% input$showtype)%>%
            filter(., Date.Year %in% c(input$showyear[1]:input$showyear[2]))%>%
            select(.,Show.Name)%>%group_by(.,Show.Name)%>%
            summarise(.,count=n())%>%arrange(desc(count))%>%top_n(20,count)%>%ungroup()
    })
    
    broadway.mean <- reactive({
        req(input$showtype)
        filter(broadway, Show.Type %in% input$showtype)%>%
          filter(., Date.Year %in% c(input$showyear[1]:input$showyear[2]))%>%
          select(.,Show.Name)%>%group_by(.,Show.Name)%>%
            summarise(.,count=n())%>%summarise(.mean=mean(count))
    })
    
    # future work - add arrow 
    # yend <- reactive({
    #   req(input$showtype)
    #   filter(broadway, Show.Type %in% input$showtype)%>%
    #     filter(., Date.Year %in% c(input$showyear[1]:input$showyear[2]))%>%
    #     select(.,Show.Name)%>%group_by(.,Show.Name)%>%
    #     summarise(.,count=n())%>%summarise(.,max=max(count))
    # })
    
    # broadway price data
    broadway.price <- reactive({
      req(input$showtype2)
      filter(broadway, Show.Type %in% input$showtype2)%>%
        filter(., Date.Year %in% c(input$showyear2[1]:input$showyear2[2]))%>%
        sample_n(500,replace = TRUE)%>%
        mutate(Price=Statistics.Gross/Statistics.Attendance)
    })  
    
    # wordclouds
    output$wordcloud <- renderWordcloud2({
       wordcloud2(word_counts, size = 1.6, fontFamily = "Courier",
                  color=rep_len(pal[2:4], nrow(word_counts)), backgroundColor = "black")
     })
    
    
    # running weeks
    output$bookofshows <- renderPlot({
        ggplot(data = broadway.count(),aes(x = reorder(Show.Name, -count), y = count)) + 
            geom_bar(fill = "Black",stat="identity") +
            geom_hline(aes(yintercept = broadway.mean()[[1]]), color="#E00008",linetype = 2) +
            geom_text(data = broadway.count(), aes(label = Show.Name), angle = 90, size = 3.8, colour = "white",
                      hjust = 1.1) +
            # future work - add curve 
            # geom_curve(aes(xend = 3, yend = yend()[[1]]+5,color = "#E00008"), 
            #        data = broadway.count()%>%top_n(1),
            #        curvature = -0.2, 
            #        arrow = arrow(type = "closed", length = unit(0.2,"cm")),
            #        show.legend = FALSE) +
            labs(y = "Running Weeks") +
            theme_minimal() +
            theme(panel.grid.minor.x = element_blank(),
                  axis.line.x = element_line(colour = "black", size = 1),
                  axis.text = element_text(size = 12),
                  panel.grid.major.x = element_blank(),
                  axis.ticks.x = element_line(),
                  plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank())
    }, height = 500, width = 800)
    
    
    # Longest Run Show Names
    output$shownames <- renderText({
        paste0(paste(broadway.count()[[1]][1:5], sep="", collapse = ', ')," are the longest running shows from ",input$showyear[1], " to ", input$showyear[2], "." )
    })
    

    
    # Price
    output$broadwayprice <- renderPlot({
      ggplot(broadway.price(), aes(x = factor(Date.Month), y = Price)) +
        geom_jitter(aes(size = Price, fill = Show.Type),
                    width = 0.1, alpha = 0.5, shape = 21,colour="Grey") +
        geom_boxplot(aes(fill = Price), colour = "white", width = 0.5,
                     outlier.shape = NA, alpha = 0.4) + 
        theme(legend.position="bottom")+
        scale_fill_manual(values = c("#E00008", "white","#e06800"))+
        # scale_color_paletteer_d(wesanderson, Royal1, direction = 1) +
        # scale_fill_paletteer_d(wesanderson, Royal1, direction = 1) +
        labs(x = "", y = "Price") +
        theme(text = element_text(colour = "white"),
              plot.title = element_text(size = rel(1.5)),
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
      
    }, height = 500, width = 800)
    
    
    # theatre Network
    theatre_graph_data <- reactive({
        req(input$theatrelist)
        theatre1 %>%
            filter(Show.Theatre %in% input$theatrelist) %>%
            as_tbl_graph() %>%
            mutate(type = if_else(name %in% theatre$Show.Theatre, "Theatre", "Show")) %>%
            mutate(color.background = if_else(name %in% input$theatrelist, "#E00008", "black"),
                   color.border = if_else(name %in% input$theatrelist, "#E00008", "black"),
                   title = name,
                   label = name,
                   labelHighlightBold = TRUE,
                   size = if_else(name == "American Airlines", 70, 25),
                   font.face = "Courier",
                   font.size = if_else(name == "American Airlines", 70, 40),
                   font.color = if_else(name %in% input$theatrelist, "#E00008", "black"),
                   shape = if_else(name %in% input$theatrelist, "icon", "dot"),
                   icon.face = "FontAwesome",
                   icon.code = "f005",
                   icon.size = if_else(name == "American Airline", 200, 100),
                   icon.color = if_else(name %in% input$theatrelist, "#E00008", "black")) %>%
            activate(edges) %>%
            mutate(scaling.max = 20)

    })


    output$theatrenetwork <- renderVisNetwork({

        visIgraph(theatre_graph_data()) %>%
            visInteraction(hover = TRUE, tooltipDelay = 0) %>%
            addFontAwesome()

    })
    
    
    # book of shows database    
    output$database <- renderDT({
      datatable(Database,
                colnames = c("Opening Year","Show Name","Show Type","Theatre"),
                rownames = FALSE,
                escape = FALSE,
                class = 'display',
                options = list(pageLength = 20,
                               lengthMenu = c(10, 20, 50))
                      )
    })

   
}

