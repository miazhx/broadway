library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)





broadway <- fread(file = "./broadway.csv")
head(broadway)


broadway.count <- broadway%>%select(.,Show.Name,Show.Type)%>%group_by(.,Show.Name)%>%
    summarise(.,count=n())%>%arrange(desc(count))
head(broadway.count)
ggplot(data = broadway.count,aes(x = reorder(Show.Name, -count), y = count)) + geom_bar(stat="identity")



server <- function(input, output) {
    
    # broadway running week data
    broadway.count <- reactive({
        req(input$showtype)
        filter(broadway, Show.Type %in% input$showtype)%>%
            filter(., Date.Year %in% c(input$showyear[1]:input$showyear[2]))%>%
            select(.,Show.Name)%>%group_by(.,Show.Name)%>%
            summarise(.,count=n())%>%arrange(desc(count))%>%top_n(20)
    })
    broadway.mean <- reactive({
        req(input$showtype)
        filter(broadway, Show.Type %in% input$showtype)%>%select(.,Show.Name)%>%group_by(.,Show.Name)%>%
            summarise(.,count=n())%>%summarise(.mean=mean(count))
    })
    # broadway.year <- reactive({
    #     req(input$shownames)
    #     filter(commentary, disc_number %in% input$disc)
    # })
    # averages_subset <- reactive({
    #     req(input$disc)
    #     filter(averages, disc_number %in% input$disc) %>% 
    #         pull(avg_love)
    # })   
    
    # The Book of Shows
    output$bookofshows <- renderPlot({
        
        ggplot(data = broadway.count(),aes(x = reorder(Show.Name, -count), y = count)) + 
            geom_bar(stat="identity",show.legend = FALSE) +
            geom_hline(aes(yintercept = broadway.mean()[[1]]), linetype = 2, show.legend = FALSE) +
            geom_text(data = broadway.count(), aes(label = Show.Name), angle = 90, size = 3.8, colour = "white",
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
        
    }, height = 500, width = 800)
    
    
    # Longest Run Show Names
    output$shownames <- renderText({
        paste0(paste(broadway.count()[[1]][1:5], sep="", collapse = ', ')," are the longest running shows from ",input$showyear[1], " to ", input$showyear[2], "." )
    })

    # # word counts (excluding stop words) for word clouds
    # word_counts <- reactive({
    #     req(input$disc)
    #     lovesongs_tidy %>%
    #         filter(disc_number %in% input$disc_cloud) %>%
    #         select(word) %>%
    #         anti_join(stop_words, by = "word") %>%
    #         #filter(word != "love") %>%
    #         count(word, sort = TRUE)
    # })
    # 
    # # Word Clouds
    # output$wordcloud <- renderWordcloud2({
    #     wordcloud2(word_counts(), size = 1.6, fontFamily = "Courier",
    #                color=rep_len(pal[2:4], nrow(word_counts())), backgroundColor = "black")
    # })
    # 
    # # Word search table
    # output$counttable = DT::renderDataTable({
    #     DT::datatable(word_counts(), options = list(lengthMenu = c(10, 20, 50), pageLength = 10),
    #                   rownames = FALSE, colnames = c("Word", "Count"), class = 'compact',
    #                   caption = 'Common words (e.g. the, is, at) are excluded')
    # })
    # 
    # # Love Network
    # 
    # love_graph_data <- reactive({
    #     req(input$love_list)
    #     bigram_separated_love %>%
    #         filter(word1 %in% input$love_list | word2 %in% input$love_list) %>% 
    #         as_tbl_graph() %>% 
    #         mutate(color.background = if_else(name %in% input$love_list, "#E00008", "black"),
    #                color.border = if_else(name %in% input$love_list, "#E00008", "black"),
    #                label = name,
    #                labelHighlightBold = TRUE,
    #                #size = if_else(name == "love", 70, 25),
    #                font.face = "Courier",
    #                font.size = if_else(name == "love", 70, 40),
    #                font.color = if_else(name %in% input$love_list, "#E00008", "black"),
    #                shape = if_else(name %in% input$love_list, "icon", "dot"),
    #                icon.face = "FontAwesome",
    #                icon.code = "f004",
    #                icon.size = if_else(name == "love", 200, 100),
    #                icon.color = if_else(name %in% input$love_list, "#E00008", "black")) %>% 
    #         activate(edges) %>% 
    #         mutate(hoverWidth = n,
    #                selectionWidth = n,
    #                scaling.max = 20)
    #     
    # }) 
    # 
    # 
    # output$lovenetwork <- renderVisNetwork({
    #     
    #     visIgraph(love_graph_data()) %>% 
    #         visInteraction(hover = TRUE, tooltipDelay = 0) %>% 
    #         addFontAwesome()
    #     
    # })
    # 
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

