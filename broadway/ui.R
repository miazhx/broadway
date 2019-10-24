# Broadway Shows - Lyric Analysis

# Load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(tidygraph)
library(wordcloud2)
library(glue)
library(visNetwork)

library(shiny)
library(shinydashboard)
library(leaflet)


ui <- navbarPage(inverse = TRUE, "Broadway Shows",
                 
                 # First Page - Intro        
                 tabPanel("Intro", includeCSS("styles.css"),
                          fluidPage(h1("Spotlight: What to know about Broadway Shows"),
                                    br(),
                                    p(strong(em("\"Don't fall in love with me yet, we've only recently met...\""), "1.1 - Absolutely Cuckoo")),
                                    br(),
                                    p("In 1999, the indie-pop band", a("Magnetic Fields", href = "https://en.wikipedia.org/wiki/The_Magnetic_Fields"), "released the album", 
                                      a("69 Love Songs.", href = "https://www.mergerecords.com/69-love-songs"), 
                                      "Conceived and written by frontman, Stephin Merritt, it is a three-volume concept album containing (yep, you guessed it) 69 love songs."), 
                                    p("Merritt has described the album as \"...not remotely an album about love. It's an album about love songs...\""),
                                    p("So, what does Stephin sing about when he sings about love (songs)?"),
                                    p("Play with this interactive tool and find out!"),
                                    br(),
                                    br(),
                                    #div(img(src = "magfieldsstrip.png", height = 187, width = 800), style="text-align: center;"),
                                    br(),
                                    br(),
                                    br(),
                                    div(p(strong("Built by"), a("Mia Zhang", href = "https://twitter.com/committedtotape"), "using the power of Rstudio and Shiny."), 
                                        p(strong("R Packages:"), "tidyverse, tidytext, wordcloud2, tidygraph, vizNetwork, glue."),
                                        p(strong("Sources:"), a("genius.com", href = "https://genius.com/albums/The-magnetic-fields/69-love-songs"), "for data,", a("wikipedia", href = "https://en.wikipedia.org/wiki/69_Love_Songs"), "for design."),
                                        style="text-align: right;")
                          )
                 ),
                 
                 # Second Page  - Love Counts       
                 tabPanel("The Book of Shows",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: black",
                                                               wellPanel(style = "background: white",
                                                                         selectInput("showtype",
                                                                                     "Select Show Type:",
                                                                                     choices = c("All" = "all", "Musical" = "musical", "Play" = "play", "Special" = "special"),
                                                                                     selected = 1)),
                                                               wellPanel(style = "background: white",
                                                                         h3("Info:"),
                                                                         textOutput("lovecount_desc"),
                                                                         br(),
                                                                         p("The 'loveliest' songs are annotated.")),
                                                               wellPanel(style = "background: white",
                                                                         h3("Notes:")             
                                                               )
                                                  ),
                                                  
                                                  mainPanel(
                                                      p(strong(em("\"How Fucking Romantic, must we really waltz?\""), "1.14 - How Fucking Romantic")),
                                                      br(),
                                                      p("But just ", em("how")," romantic is 69 Love Songs? Let's measure this (crudely) by the number of 'loves' in each song:"),
                                                      br(),
                                                      plotOutput("bookofshows", width = "100%")
                                                  )
                          )
                          )
                 ),
                 
                 # Third Page - Clouds        
                 tabPanel("The Book of Shows",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: black",
                                                               wellPanel(style = "background: white",
                                                                         checkboxGroupInput("disc_cloud",
                                                                                            "Select your Volume(s):",
                                                                                            choices = 1:3,
                                                                                            selected = 1:3)),
                                                               DT::dataTableOutput("counttable")),
                                                  
                                                  mainPanel( 
                                                      p(strong(em("\"...It's full of flowers and heart-shaped boxes, and things we're all too young to know.\""), "1.12 - The Book of Love")),
                                                      p("Unsurprisingly there's a lot of love, but what else? Hover over the word cloud below, or search for words in the table to the right:"),
                                                      wordcloud2Output("wordcloud", width = "100%", height = "565px")
                                                  )
                          )
                          )
                 ),
                 
                 
                 # Sentiment analysis by disc and singer
                 tabPanel("Love and Trouble",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: black",
                                                               wellPanel(style = "background: white",
                                                                         selectInput("sent_fill",
                                                                                     "Colour bars by:",
                                                                                     choices = c("Volume" = "disc_number", "Singer" = "singer"),
                                                                                     selected = "disc_number")),
                                                               wellPanel(style = "background: white",
                                                                         p("Two songs, A Chicken with Its Head Cut Off and My Sentimental Melody, 
                                                           have high proportions of both positive and negative words,
                                                           suggesting they are 'songs of love and trouble'."),
                                                                         p("I Shatter comfortably takes the most negative award.")),
                                                               wellPanel(style = "background: white",
                                                                         htmlOutput("sentiment_text"))),
                                                  mainPanel(
                                                      p(strong(em("\"Some of us can only live in songs of love and trouble.\""), "2.11 - My Only Friend")),
                                                      br(),
                                                      p("Let's try to assess sentiment, based on songs with the highest % of positive or negative words:"),
                                                      br(),
                                                      plotOutput("sentimentPlot"),
                                                      br(),
                                                      br(),
                                                      p(strong("Notes:"), "Songs with less than 20 distinct words were not considered. So for example, Punk Love, with it's repeated refrain \"Punk Rock Love\" (3 pretty positive things right?) does not appear.",
                                                        p("Positive and Negative sentiment was established using the Bing Lexicon."))
                                                  )
                          )
                          )
                 )
)