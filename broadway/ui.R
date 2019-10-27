# Broadway Shows Analysis

# Load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(tidygraph)
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
                 tabPanel("Dear Evan Hansen",
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
        
                 # Network diagram - theatre bigrams
                 tabPanel("Broadway Threatres",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: black",
                                                               wellPanel(style = "background: white",
                                                                         checkboxGroupInput("theatrelist",
                                                                                            "What theatres you want to see:",
                                                                                            choices = c("American Airlines","Booth","Lyceum","Cort","Brooks Atkinson","Vivian Beaumont","Music Box","Belasco","Ethel Barrymore"),
                                                                                            selected = "American Airlines",
                                                                                            inline = TRUE)),
                                                               wellPanel(style = "background: white",
                                                                         h4("Info:"),
                                                                         p("The words appearing before or after the word 'love' (and variants if chosen above), are shown in this network."),
                                                                         p("Words appearing after 'love' have a red line, and words appearing before 'love' have a black line.")),
                                                               wellPanel(style = "background: white",
                                                                         h4("Interact:"),
                                                                         p("Zoom in, drag, hover and select nodes to reveal the strength of the connection."),
                                                                         p("For example, common combinations such as 'in love' and 'love you' have thicker lines."))),
                                                  
                                                  mainPanel( 
                                                    p(strong(em("\"I'm for free love, and I'm in free fall. This could be love or nothing at all.\""), "1.4 - A Chicken With Its Head Cut Off")),
                                                    br(),
                                                    p("Let's put all this love into context. Explore the network of love below:"),
                                                    visNetworkOutput("theatrenetwork", width = "100%", height = "565px")
                                                  )
                          )
                          )
                 ),                 
                 
                 # Blue blue blue
                 tabPanel("Making Me Blue",
                          fluidPage(p(strong(em("\"You know you enthrall me, and yet you don't call me. It's making me blue...\""), "1.5 - Reno Dakota")),
                                    br(),
                                    DTOutput("database", width = "100%", height = "380px"),
                                    br(),
                                    fluidRow(column(9,
                                                    p("The protagonist in 'Reno Dakota' not only sings of being blue, but can describe the feeling with an exact hue.",
                                                      p("However, perhaps Pantone 292 doesn't convey", em("your"), "blue-ness accurately.
                                   Why not try another shade on for size?"), 
                                                      p("Choose from a selected palette of popular blues on the right.")))
                                    )
                          )
                 )
)