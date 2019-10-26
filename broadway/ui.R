# Broadway Shows Analysis

# Load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(tidygraph)
library(wordcloud2)
library(glue)
library(visNetwork)

library(shinyWidgets)


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
                                    div(p(strong("Built by"), a("Mia Zhang", href = "https://github.com/miazhx/broadway"), "using the power of Rstudio and Shiny."), 
                                        p(strong("R Packages:"), "tidyverse, tidytext, wordcloud2, tidygraph, vizNetwork, glue."),
                                        p(strong("Sources:"), a("CORGIS Dataset Project", href = "https://corgis-edu.github.io/corgis/csv/broadway/"), "for data,", a("David Smale", href = "https://twitter.com/committedtotape"), "for design."),
                                        style="text-align: right;")
                          )
                 ),
                 
                 # Second Page  - The Book of Shows       
                 tabPanel("The Book of Shows",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: black",
                                                               wellPanel(style = "background: white",
                                                                         selectInput("showtype",
                                                                                     "Select Show Type:",
                                                                                     choices = c("Musical" = "Musical", "Play" = "Play", "Special Performance" = "Special"),
                                                                                     selected = 1),
                                                                         setSliderColor(c("Black ", "Black", "Black", "Black"), c(1,2)),chooseSliderSkin("Nice"),sliderInput("showyear",
                                                                                     "Select Year Range:",min = 1991, 
                                                                                     max = 2016, value = c(1991, 2016),
                                                                                     sep="")),
                                                               wellPanel(style = "background: white",
                                                                         h3("Longest Running Shows:"),
                                                                         textOutput("shownames"),
                                                                         br(),
                                                                         p("It never goes wrong with these shows!")),
                                                               wellPanel(style = "background: white",
                                                                         h3("Notes:")             
                                                               )
                                                  ),
                                                  
                                                  mainPanel(
                                                      p(strong(em("\"In inches, in miles, in laughter, in strife, in 525,600 minutes, how do you measure a year in the life?\""), "Seasons of Love - Rent")),
                                                      br(),
                                                      p("Let's measure this by the number of running weeks for each show."),
                                                      br(),
                                                      plotOutput("bookofshows", width = "100%")
                                                  )
                          )
                          )
                 ),
                 
                 
                 # Third Page  - Dear Evan Hansen       
                 tabPanel("DeaR Evan Hansen",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: black",
                                                               wellPanel(style = "background: white",
                                                                         checkboxGroupInput("showtype2",
                                                                                            "Select Show Type:",
                                                                                            choices = c("Musical" = "Musical", "Play" = "Play", "Special Performance" = "Special"),
                                                                                            selected = c("Musical" = "Musical", "Play" = "Play", "Special Performance" = "Special"),inline = TRUE),

                                                                         sliderInput("showyear2",
                                                                                     "Select Year Range:",min = 1991, 
                                                                                     max = 2016, value = c(1991, 2016),
                                                                                     sep="")),
                                                               wellPanel(style = "background: white",
                                                                         h3("Longest Running Shows:"),
                                                                         textOutput("shownames2"),
                                                                         br(),
                                                                         p("It never goes wrong with these shows!")),
                                                               wellPanel(style = "background: white",
                                                                         h3("Notes:")             
                                                               )
                                                  ),
                                                  
                                                  mainPanel(
                                                    p(strong(em("\"In inches, in miles, in laughter, in strife, in 525,600 minutes, how do you measure a year in the life?\""), "Seasons of Love - Rent")),
                                                    br(),
                                                    p("Let's measure this by the number of running weeks for each show."),
                                                    br(),
                                                    plotOutput("broadwayprice", width = "100%")
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