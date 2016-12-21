#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(proxy)
library(recommenderlab)
library(reshape2)
source("helper.R")

shinyServer(function(input, output) {
    
    output$table <- renderTable({
        movie_recommendation(input$select, input$select2, input$select3)
    })
    
}
)




# # Define server logic required to draw a histogram
# shinyServer(function(input, output) {
#    
#     # Text for the 3 boxes showing average scores
#     formulaText1 <- reactive({
#         paste(input$movie1)
#     })
#     formulaText2 <- reactive({
#         paste(input$movie2)
#     })
#     formulaText3 <- reactive({
#         paste(input$movie3)
#     })
#     
#     output$movie1 <- renderText({
#         formulaText1()
#     })
#     output$movie2 <- renderText({
#         formulaText2()
#     })
#     output$movie3 <- renderText({
#         formulaText3()
#     })
#     
#     
#     # Table containing recommendations
#     output$table <- renderTable({
#         movie_recommendation <- function(movie1, movie2, movie3) {
#             row_num <- which(titles == movie1)
#             row_num2 <- which(titles == movie2)
#             row_num3 <- which(titles == movie3)
#             userSelect <- matrix(NA, length(titles))
#             
#             userSelect <- moviedata[,]
#             mat <- as(userSelect, "matrix")
#             mat[1, 1:ncol(mat)] <- NA
#             mat[row_num] <- 5 #hard code first selection to rating 5
#             mat[row_num2] <- 4 #hard code second selection to rating 4
#             mat[row_num3] <- 4 #hard code third selection to rating 4
#             userSelect <- as(mat, "realRatingMatrix")
#             
#             #Create Recommender Model
#             recommender_model <- Recommender(moviedata, method = "UBCF")
#             recom <- predict(recommender_model, userSelect, n = 10)
#             recom_list <- as(recom, "list")
#             recom_result <- data.frame(recom_list)
#             colnames(recom_result) <- "Check these out:"
#             return(recom_result)
#         }
#         
#         recommends <-
#             movie_recommendation(input$movie1, input$movie2, input$movie3)
#         recommends
#     })
#     
#     # Generate a table summarizing each players stats
#     # output$table <- renderDataTable({
#     #     movie_recommendation(input$movie1, input$movie2, input$movie3)
#     # })
#     
# })
