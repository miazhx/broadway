#function library
library(shiny)
library(data.table)
library(tidyverse)
library(tidytext)
library(tidygraph)
library(wordcloud2)
library(glue)
library(visNetwork)

#color library
library(paletteer)
library("wesanderson")
back_col <- paletteer_d(ghibli, MarnieLight1)[2]

#font library
library("extrafont")
fonttable()


# read data 
broadway <- fread(file = "./broadway.csv")
theatre <- fread(file = "./theatre.csv")
theatre1 <- theatre%>%select(Show.Theatre,Show.New.Name)
broadway <- broadway%>%mutate(.,typecolor=case_when(Show.Type == "Musical" ~ "black",
                                                    Show.Type == "Play" ~ "#E00008",
                                                    Show.Type == "Special" ~ "#858B8E"))


#server 
server <- function(input, output) {
    
    # broadway running week data
    broadway.count <- reactive({
        req(input$showtype)
        filter(broadway, Show.Type %in% input$showtype)%>%
            filter(., Date.Year %in% c(input$showyear[1]:input$showyear[2]))%>%
            select(.,Show.Name,typecolor)%>%group_by(.,Show.Name)%>%
            summarise(.,count=n(),Type.Color=first(typecolor))%>%arrange(desc(count))%>%top_n(20,count)%>%ungroup()
    })
    broadway.mean <- reactive({
        req(input$showtype)
        filter(broadway, Show.Type %in% input$showtype)%>%
          filter(., Date.Year %in% c(input$showyear[1]:input$showyear[2]))%>%
          select(.,Show.Name)%>%group_by(.,Show.Name)%>%
            summarise(.,count=n())%>%summarise(.mean=mean(count))
    })
    
    
    # broadway price data
    broadway.price <- reactive({
      req(input$showtype2)
      filter(broadway, Show.Type %in% input$showtype2)%>%
        filter(., Date.Year %in% c(input$showyear2[1]:input$showyear2[2]))%>%
        sample_n(300)%>%
        mutate(Price=Statistics.Gross/Statistics.Attendance)
    })  
    
    # The Book of Shows
    output$bookofshows <- renderPlot({
        
        
        ggplot(data = broadway.count(),aes(x = reorder(Show.Name, -count), y = count)) + 
            geom_bar(fill = "Black",stat="identity") +
            geom_hline(aes(yintercept = broadway.mean()[[1]]), color="#E00008",linetype = 2) +
            geom_text(data = broadway.count(), aes(label = Show.Name), angle = 90, size = 3.8, colour = "white",
                      hjust = 1.1) +
            geom_curve(aes(xend = 10, yend = 100,colour = "Black"), 
                   data = broadway.count()%>%top_n(1),
                   curvature = -0.2, 
                   arrow = arrow(type = "closed", length = unit(0.2,"cm")),
                   show.legend = FALSE) +
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
    
    #Dear Evan Hansen - Price
    output$broadwayprice <- renderPlot({
      
      
      ggplot(broadway.price(), aes(x = factor(Date.Month), y = Price)) +
        geom_jitter(aes(size = Price, fill = Show.Type),
                    width = 0.1, alpha = 0.3, shape = 21) +
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

    # 
    # # sentiment plot
    # sentiment_pal <- c("black", "#E00008", "#858B8E", "#62A8E5", "#4000FF", "#1D2951")
    # 
    # output$sentimentPlot <- renderPlot({
    #     ggplot(song_sentiment_tidy, aes(x = order, y = perc)) +
    #         geom_col(aes_string(fill = input$sent_fill)) + 
    #         geom_text(aes(label = song, y = 0.48), hjust = 1,
    #                   family = "Courier") +
    #         geom_text(aes(label = scales::percent(perc, accuracy = .1)), hjust = 1.1,
    #                   family = "Courier", colour = "white") +
    #         coord_flip() +
    #         facet_wrap(~ sentiment, scales = "free_y") +
    #         scale_x_continuous(breaks = song_sentiment_tidy$order,
    #                            expand = c(0,0)) +
    #         scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1),
    #                            limits = c(0,0.5)) +
    #         scale_fill_manual(values = sentiment_pal) +
    #         labs(y = "\n% of Positive/Negative Sentiment", fill = NULL) +
    #         theme_minimal() +
    #         theme(text = element_text(family = "Courier"),
    #               panel.grid.minor.y = element_blank(),
    #               panel.spacing = unit(1, "cm"),
    #               panel.border = element_rect(fill = NA, colour = "black", size = 1),
    #               strip.background = element_rect(fill = "white", colour = "black", size = 1),
    #               strip.text = element_text(colour = "black", face = "bold", size = 18),
    #               #axis.line.y = element_line(colour = "black", size = 1),
    #               panel.grid.major.y = element_blank(),
    #               axis.text.y = element_blank(),
    #               axis.title.y = element_blank(),
    #               axis.ticks.x = element_line(),
    #               plot.margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt"),
    #               legend.position = "bottom") +
    #         guides(fill = guide_legend(nrow = 1))
    # })
    # 
    # # Sentiment text
    # output$sentiment_text <- renderUI({
    #     if (input$sent_fill == "disc_number") {
    #         HTML("<h4>Volumes:</h4><p>No songs from Volume 3 appear in the top 10 most positive list, with the majority coming from Volume 1.</p><p>3 songs from Volume 3 do appear in the negative list. Is Volume 3 the dark album?</p>")
    #     } else {
    #         HTML("<h4>Singers:</h4><p>Four guest vocalists appear on 69 Love Songs.</p><p>Dudley Klute takes on 2 of the highest proportioned negative songs, but misses out on the most positive songs.</p>")
    #     }
    # })
    # 
    # # Plot of blues
    # blueshades <- tibble(colourname = c("Pantone 292", "Crayola Blue", "Liberty", "Space Cadet", "Teal", "Ultramarine"),
    #                      colourhex = c("#62A8E5", "#1F75FE", "#545AA7", "#1D2951", "#008080", "#4000FF"))
    # 
    # blueshade <- reactive({
    #     blueshades %>% 
    #         filter(colourname == input$shade)
    # })
    # 
    # output$blues <- renderPlot({
    #     ggplot(tibble(x = 1:10, y = 1:10, label = paste0("...", blueshade()$colourname)), 
    #            aes(x, y)) +
    #         geom_point(colour = blueshade()$colourhex, show.legend = FALSE) +
    #         geom_text(aes(label = label, x = 5, y = 5), family = "Courier", size = 8,
    #                   colour = "white", show.legend = FALSE) +
    #         scale_x_continuous(limits = c(0, 10), expand = c(0,0)) +
    #         scale_y_continuous(limits = c(0, 10), expand = c(0,0)) +
    #         theme_void() +
    #         theme(plot.background = element_rect(fill = blueshade()$colourhex, colour = blueshade()$colourhex))
    # })
}

