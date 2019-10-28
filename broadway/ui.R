# Broadway Shows Spotlight

# Load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(tidygraph)
library(visNetwork)
library(shinyWidgets)
library(wordcloud2)



ui <- navbarPage(inverse = TRUE, "Broadway Shows",
                 
                 # First Page - Intro        
                 tabPanel("Intro", includeCSS("styles.css"),
                          fluidPage(h1("Broadway Spotlight: "),
                                    br(),
                                    p(strong(em("\"Give my regards to Broadway, remember me to Herald Square, tell all the gang at 42nd Street, that I will soon be there\""), " - George M. Cohan")),
                                    br(),
                                    p(a("Broadway theatre,", href = "https://en.wikipedia.org/wiki/Broadway_theatre"),"also known simply as Broadway, refers to the theatrical performances presented in professional theatres, each with 500 or more seats located in the Theater District and Lincoln Center along Broadway, in Midtown Manhattan, New York City." 
                                      
                                      ), 
                                    p("So, what do we need to know about Broadway Shows?"),
                                    p("Let's play with this interactive tool and find out!"),
                                    wordcloud2Output("wordcloud", width = "100%", height = "400px"),
                                    div(p(strong("Built by"), a("Mia Zhang", href = "https://github.com/miazhx/broadway"), "using the power of Rstudio and Shiny."), 
                                        p(strong("Sources:"), a("CORGIS Dataset Project", href = "https://corgis-edu.github.io/corgis/csv/broadway/"), "for data,", a("David Smale", href = "https://twitter.com/committedtotape"), "for design."),
                                        style="text-align: right;")
                          )
                 ),
                 
                 # Running Weeks       
                 tabPanel("Running Weeks",
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
                                                                         p("If it's your first time, you can't go wrong with classics!")),
                                                               wellPanel(style = "background: white",
                                                                         h3("Notes:"),
                                                                         p("The red line represents the average running weeks for the selected show type and year range.")
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
                 
                 
                 # Price       
                 tabPanel("Price",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: black",
                                                               wellPanel(style = "background: white",
                                                                         # selectInput("xaxistype",
                                                                         #             "Select value:",
                                                                         #             choices = c("Price" = "Price", "Gross" = "Statistics.Gross", "Capacity" = "Statistics.Capacity","Attendance"="Statistics.Attendance","Performances"="Statistics.Performances"),
                                                                         #             selected = 1),
                                                                         checkboxGroupInput("showtype2",
                                                                                            "Select Show Type:",
                                                                                            choices = c("Musical" = "Musical", "Play" = "Play", "Special Performance" = "Special"),
                                                                                            selected = c("Musical" = "Musical", "Play" = "Play", "Special Performance" = "Special"),inline = TRUE),

                                                                         sliderInput("showyear2",
                                                                                     "Select Year Range:",min = 1991, 
                                                                                     max = 2016, value = c(1991, 2016),
                                                                                     sep="")),
                                                               wellPanel(style = "background: white",
                                                                         h3("Notes:"),
                                                                         p("Price is weekly gross divided by weekly attendance, which is an average price for seats in different sections such as orchestra, mezzanine and balcony.")),
                                                               wellPanel(style = "background: white",
                                                                         h3("What's your choice:"),
                                                                         p("Musical or Play?"),
                                                                         p("Holiday Season or Not Holiday Season?"),
                                                               )
                                                  ),
                                                  
                                                  mainPanel(
                                                    p(strong(em("\"There was a time when love was blind, and the world was a song.\""), "I Dreamed a Dream - Les Miserables")),
                                                    br(),
                                                    p("Let's measure this by the number of running weeks for each show."),
                                                    br(),
                                                    plotOutput("broadwayprice", width = "100%")
                                                  )
                          )
                          )
                 ),
        
                 # Network diagram - theatre bigrams
                 tabPanel("Broadway Theatres",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: black",
                                                               wellPanel(style = "background: white",
                                                                         checkboxGroupInput("theatrelist",
                                                                                            "Theatres host most shows:",
                                                                                            choices = c("American Airlines","Booth","Lyceum","Cort","Brooks Atkinson","Vivian Beaumont","Music Box","Belasco","Ethel Barrymore"),
                                                                                            selected = "American Airlines",
                                                                                            inline = TRUE)),
                                                               wellPanel(style = "background: white",
                                                                         h4("Info:"),
                                                                         p("Each red star represents a theatre which hosts the most shows and each black dot represents a Broadway show from 1991 to 2016."),
                                                                         p("The lines link a show to the theatre(s) they were at.")),
                                                               wellPanel(style = "background: white",
                                                                         h4("Interact:"),
                                                                         p("Zoom in, drag, hover and select nodes to reveal the strength of the connection."),
                                                                         p("For example, Macbeth has 4 productions in 4 different theatres in 25 years."))),
                                                  
                                                  mainPanel( 
                                                    p(strong(em("\"From the west, from the south. Honey in my ears, spice in my mouth.\""), "Omar Sharif - The Band's Visit")),
                                                    br(),
                                                    p("Explore the network of shows and theatres below:"),
                                                    visNetworkOutput("theatrenetwork", width = "100%", height = "565px")
                                                  )
                          )
                          )
                 ),   
                 
                 
                 # # Gross       
                 # tabPanel("Gross",
                 #          fluidPage(sidebarLayout(position = "right",
                 #                                  sidebarPanel(style = "background: black",
                 #                                               wellPanel(style = "background: white",
                 #                                                         selectInput("xaxistype3",
                 #                                                                     "Select value:",
                 #                                                                     choices = c("Price" = "Price", "Gross" = "Statistics.Gross", "Capacity" = "Statistics.Capacity","Attendance"="Statistics.Attendance","Performances"="Statistics.Performances"),
                 #                                                                     selected = 1),
                 #                                                         checkboxGroupInput("showtype3",
                 #                                                                            "Select Show Type:",
                 #                                                                            choices = c("Musical" = "Musical", "Play" = "Play", "Special Performance" = "Special"),
                 #                                                                            selected = c("Musical" = "Musical", "Play" = "Play", "Special Performance" = "Special"),inline = TRUE),
                 #                                                         
                 #                                                         sliderInput("showyear3",
                 #                                                                     "Select Year Range:",min = 1991, 
                 #                                                                     max = 2016, value = c(1991, 2016),
                 #                                                                     sep="")),
                 #                                               wellPanel(style = "background: white",
                 #                                                         h3("Longest Running Shows:"),
                 #                                                         textOutput("shownames3"),
                 #                                                         br(),
                 #                                                         p("If it's your first time, you can't go wrong with classics!")),
                 #                                               wellPanel(style = "background: white",
                 #                                                         h3("Find out yourself:")             
                 #                                               )
                 #                                  ),
                 #                                  
                 #                                  mainPanel(
                 #                                    p(strong(em("\"There was a time when love was blind, and the world was a song.\""), "I Dreamed a Dream - Les Miserables")),
                 #                                    br(),
                 #                                    p("Let's measure this by the number of running weeks for each show."),
                 #                                    br(),
                 #                                    plotOutput("showgross", width = "100%")
                 #                                  )
                 #          )
                 #          )
                 # ),                 
                 
                 # database 
                 tabPanel("The Book of Shows",
                          fluidPage(p(strong(em("\"When you're falling in a forest and there's nobody around, do you ever really crash, or even make a sound?\""), "Waving Through A Window - Dear Evan Hansen")),
                                    br(),
                                    DTOutput("database", width = "100%", height = "380px"),
                                    br(),
                                    fluidRow(column(9,
                                                    p("The dataset stretches from the 1990s to August 2016, and only shows that reported capacity were included.",
                                                      p("It is made available by the Broadway League and organized by Austin Cory Bart from the CORGIS Dataset Project.") 
                                                      ))
                                    )
                          )
                 )
)