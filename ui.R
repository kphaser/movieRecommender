library(shiny)

# load('moviedata.rda')
# titles <- colnames(moviedata)
movies2 <- read.csv("data/movies2.csv", header = TRUE, stringsAsFactors=FALSE)

shinyUI(fluidPage(
    titlePanel("Movie Recommendation Engine"),
    fluidRow(
        column(5,
               selectInput("select", label = h3("Select Your 3 Favorite Movies"),
                           choices = as.character(movies2$title)),
               
               selectInput("select2", label = NA,
                           choices = as.character(movies2$title)),
               
               selectInput("select3", label = NA,
                           choices = as.character(movies2$title)),
               
               submitButton("Show me some movies")
               
        ),
        
        column(7,
               h3("Check out these titles!"),
               tableOutput("table"))
    )
))

# shinyUI(fluidPage(
#     titlePanel("Movie Recommendation System using User Collaborative Filtering"),
#     fluidRow(
#         
#         column(4, h3("Select Movies that you like:"),
#                wellPanel(
#                    selectInput("movie1", "Movie #1",
#                                as.character(titles[order(titles)])),
#                    selectInput("movie2", "Movie #2",
#                                as.character(titles[order(titles)])),
#                    selectInput("movie3", "Movie #3",
#                                as.character(titles[order(titles)])),
#                    submitButton("Show me some movies")
#                )),
#         
#         # column(4, h3("Select Movies You Like of these Genres:"),
#         #        wellPanel(
#         #            # This outputs the dynamic UI component
#         #            uiOutput("ui"),
#         #            uiOutput("ui2"),
#         #            uiOutput("ui3")
#         #            #submitButton("Get Recommendations")
#         #        )),
#         
#         column(4,
#                h3("Here are some suggestions:"),
#                tableOutput("table")
#                #verbatimTextOutput("dynamic_value")
#         )
#     )
#     
#     # fluidRow(
#     #     column(12,
#     #            helpText("For a detailed description of this project please visit", 
#     #                     a("the link", href="http://rpubs.com/jeknov/movieRec", target="_blank")),
#     #            helpText("For a code, press", a("here", href = "https://github.com/jeknov/movieRec", target="_blank"))
#     #            
#     #     )
#     # )
# ))
