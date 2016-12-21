### Movie Recommender System in R ###

# We will be using a collaborative filtering system, which is essentially items recommended based on other similar users.
# The other is content-based filtering which is based on your past history. We will build a recommendation system for movies 
# and compare the different methods. We will build a shiny app using the best evaluated model.


#################
### Load data ###
#################
library(recommenderlab)
library(reshape2)
library(ggplot2)
library(data.table)
library(DBI)
library(RSQLite)

# Connect to SQLite database
getwd()
setwd("C:/Users/kphas_000/home/projects/movierec/")
con = dbConnect(SQLite(), dbname="movielens.db")

# Test SQL query
myQuery <- dbSendQuery(con, "SELECT * FROM movies LIMIT 10")
test_result <- dbFetch(myQuery)
test_result

# Read in the movies and ratings tables from the movielens database
movies <- dbSendQuery(con, "SELECT * FROM movies")
movies <- dbFetch(movies)

ratings <- dbSendQuery(con, "SELECT * FROM ratings")
ratings <- dbFetch(ratings)

head(movies)
str(movies)
head(ratings)
str(ratings)

# # if you want to use dplyr to load and merge the datasets
# library(dplyr)
# db <- src_sqlite('movielens.db')
# movs = tbl(db, "movies")
# rats = tbl(db, "ratings")
# movs %>% left_join(rats)


# The first row are just a bunch of headers and the timestamp column does not seem to provide any information. Let's remove those
movies <- movies[-1,]
ratings <- ratings[-1,]
ratings$time_stamp <- NULL


# when loading from SQL database, all columns are converted to character
# convert certain character columns to numeric as we would expect
movies$movieId <- as.integer(movies$movieId)
ratings$userId <- as.integer(ratings$userId)
ratings$movieId <- as.integer(ratings$movieId)
ratings$rating <- as.numeric(ratings$rating)
str(movies)
str(ratings)

##########################
### Data preprocessing ###
##########################

# extract the genres into a matrix
genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)

genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', 
                                   type.convert=TRUE), 
                         stringsAsFactors=FALSE)
colnames(genres2) <- c(1:10)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western") # we have 18 genres in total

genre_matrix <- matrix(0,9126,18) #empty matrix, 10330=no of movies+1, 18=no of genres
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix
for (i in 1:nrow(genres2)) {
    for (c in 1:ncol(genres2)) {
        genmat_col = which(genre_matrix[1,] == genres2[i,c])
        genre_matrix[i+1,genmat_col] <- 1
    }
}

#convert into dataframe
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {
    genre_matrix2[,c] <- as.integer(genre_matrix2[,c])  #convert from characters to integers
} 

head(genre_matrix2)



######

#create user profile matrix where ratings greater than 3 are good (+1) and under 3 are not so good (-1)
binaryratings <- ratings
for (i in 1:nrow(binaryratings)){
    if (binaryratings[i,3] > 3){
        binaryratings[i,3] <- 1
    }
    else{
        binaryratings[i,3] <- -1
    }
}

#get binaryratings matrix in correct format from long to wide format
#sub non-rated movies for users with 0 in place of NAs
binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binaryratings2)){
    binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
binaryratings2 = binaryratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds


#Remove rows that are not rated from movies dataset
length(unique(movies$movieId)) #9125
length(unique(ratings$movieId)) #9066
movieIds <- unique(movies$movieId) 
ratingmovieIds <- unique(ratings$movieId)
movies2 <- movies[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(movies2) <- NULL
write.csv(movies2,"movies2.csv",row.names=FALSE)
#Remove rows that are not rated from genre_matrix2
genre_matrix3 <- genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(genre_matrix3) <- NULL

#Calculate dot product for User Profiles
result = matrix(0,18,671) #binaryratings2 matrix has 9066 rows and 671 cols
for (c in 1:ncol(binaryratings2)){
    for (i in 1:ncol(genre_matrix3)){
        result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c]))
    }
}

#Convert to Binary scale
for (i in 1:nrow(result)){
    if (result[i] < 0){
        result[i] <- 0
    }
    else {
        result[i] <- 1
    }
}


result2 <- result[1,] #First user's profile
sim_mat <- rbind.data.frame(result2, genre_matrix3)
sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer

#Calculate Jaccard distance between user profile and all movies
library(proxy)
sim_results <- dist(sim_mat, method = "Jaccard")
sim_results <- as.data.frame(as.matrix(sim_results[1:9066]))
rows <- which(sim_results == min(sim_results))
#Recommended movies
movies[rows,2]
movies[rows,]



## User based collaborative filtering
#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds
dim(ratingmat)

#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

#Normalize the data
ratingmat_norm <- normalize(ratingmat)

#Create Recommender Model. "UBCF" stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat_norm, method = "UBCF", param=list(method="Cosine",nn=30))
recom <- predict(recommender_model, ratingmat[1], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list <- as(recom, "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in c(1:10)){
    recom_result[i] <- movies[as.integer(recom_list[[1]][i]),2]
}

recom_result

###Evaluate
evaluation_scheme <- evaluationScheme(ratingmat, method="cross-validation", k=5, given=3, goodRating=5) #k=5 meaning a 5-fold cross validation. given=3 meaning a Given-3 protocol
evaluation_results <- evaluate(evaluation_scheme, method="UBCF", n=c(1,3,5,10,15,20))
eval_results <- getConfusionMatrix(evaluation_results)[[1]]


#parameters
models_to_evaluate <- list(
    IBCF_cos = list(name = "IBCF", 
                    param = list(method = "cosine")),
    UBCF_cos = list(name = "UBCF", 
                    param = list(method = "cosine")),
    POPULAR_cor = list(name = "POPULAR", param = NULL),
    RANDOM = list(name = "RANDOM", param=NULL)
)

n_recommendations <- c(1, 3, 5, 10, 15, 20)
list_results <- evaluate(x = evaluation_scheme, 
                         method = models_to_evaluate, 
                         n = n_recommendations)

sapply(list_results, class) == "evaluationResults"

avg_matrices <- lapply(list_results, avg)
head(avg_matrices$IBCF_cos[, 5:8])


plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")


# hyper parameterization
vector_nn <- c(seq(10, 50, 10))
models_to_evaluate <- lapply(vector_nn, function(nn){
    list(name = "UBCF",
         param = list(method = "cosine", nn = nn))
})
names(models_to_evaluate) <- paste0("UBCF_nn_", vector_nn)

n_recommendations <- c(1, 5, seq(10, 100, 10))
list_results <- evaluate(x = evaluation_scheme, 
                         method = models_to_evaluate, 
                         n = n_recommendations)

plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")












###### 


#create matrix to search movie by genre
search_matrix <- cbind(movies[,1:2], genre_matrix2)
head(search_matrix)

#convert ratings matrix to correct format
#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds

#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")
ratingmat


#explore parameters of types of recommendation models
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models)
lapply(recommender_models, "[[", "description")

#let's build 4 different models and compare each--Random,IBCF,UBCF,and SVDF
recommender_models$IBCF_realRatingMatrix$parameters
recommender_models$UBCF_realRatingMatrix$parameters
recommender_models$RANDOM_realRatingMatrix$parameters
recommender_models$SVDF_realRatingMatrix$parameters


###########
### EDA ###
###########

# similarity analysis
similarity_users <- similarity(ratingmat[1:4, ], 
                               method = "cosine", 
                               which = "users")
as.matrix(similarity_users)
image(as.matrix(similarity_users), main = "User similarity")

similarity_items <- similarity(ratingmat[, 1:4], method =
                                   "cosine", which = "items")
as.matrix(similarity_items)
image(as.matrix(similarity_items), main = "Movies similarity")

# ratings
vector_ratings <- as.vector(ratingmat@data)
unique(vector_ratings) # what are unique values of ratings

table_ratings <- table(vector_ratings) # what is the count of each rating value
table_ratings

vector_ratings <- vector_ratings[vector_ratings != 0] # rating == 0 are NA values
vector_ratings <- factor(vector_ratings)

qplot(vector_ratings) + 
    ggtitle("Distribution of the ratings")

# most viewed movies
views_per_movie <- colCounts(ratingmat) # count views for each movie

table_views <- data.frame(movie = names(views_per_movie),
                          views = views_per_movie) # create dataframe of views
table_views <- table_views[order(table_views$views, 
                                 decreasing = TRUE), ] # sort by number of views
table_views$title <- NA
for (i in 1:9066){
    table_views[i,3] <- as.character(subset(movies, 
                                            movies$movieId == table_views[i,1])$title)
}

table_views[1:10,]

ggplot(table_views[1:10, ], aes(x = title, y = views)) +
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ggtitle("Number of views of the top movies")


# distribution of avg movie rating
average_ratings <- colMeans(ratingmat)

qplot(average_ratings) + 
    stat_bin(binwidth = 0.1) +
    ggtitle("Distribution of the average movie rating")

average_ratings_relevant <- average_ratings[views_per_movie > 50] 
qplot(average_ratings_relevant) + 
    stat_bin(binwidth = 0.1) +
    ggtitle(paste("Distribution of the relevant average ratings"))

##################################
### Build Recommendation Model ###
##################################

#Normalize the data
ratingmat_norm <- normalize(ratingmat)
sum(rowMeans(ratingmat_norm) > 0.00001)

#Find out what the parameter settings are for each recommender system types
recommenderRegistry$get_entries(dataType ="realRatingMatrix")

#Create Recommender Model
recommender_model <- Recommender(ratingmat_norm,
                     method = "UBCF",
                     param = list(method = "Cosine", nn = 30))
rec_model <- predict(recommender_model, ratingmat[1], n = 10) #Obtain top 10 recommendations for 1st user in dataset
recom_list <- as(rec_model, "list") #convert recommenderlab object to readable list

#Obtain Top 10 recommendations
recom_result <- matrix(0, 10)
for (i in c(1:10)) {
    recom_result[i] <- as.integer(recom_list[[1]][i])
}
recom_result <- as.data.frame(movies[recom_result, 2])
colnames(recom_result) <- list("Top 10 Movies")
recom_result


#######################
#### Evaluate Model ###
#######################

percentage_training <- 0.8

min(rowCounts(ratings_movies)) 
items_to_keep <- 5 #number of items to generate recommendations
rating_threshold <- 3 # threshold with the minimum rating that is considered good
n_eval <- 1 #number of times to run evaluation

eval_sets <- evaluationScheme(data = ratings_movies, 
                              method = "split",
                              train = percentage_training, 
                              given = items_to_keep, 
                              goodRating = rating_threshold, 
                              k = n_eval) 
eval_sets

getData(eval_sets, "train") # training set
getData(eval_sets, "known") # set with the items used to build the recommendations
getData(eval_sets, "unknown") # set with the items used to test the recommendations

qplot(rowCounts(getData(eval_sets, "unknown"))) + 
    geom_histogram(binwidth = 10) + 
    ggtitle("unknown items by the users")

model_to_evaluate <- "IBCF"
model_parameters <- NULL

eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate, 
                                parameter = model_parameters)

items_to_recommend <- 10
eval_prediction <- predict(object = eval_recommender, 
                           newdata = getData(eval_sets, "known"), 
                           n = items_to_recommend, 
                           type = "ratings")

qplot(rowCounts(eval_prediction)) + 
    geom_histogram(binwidth = 10) +
    ggtitle("Distribution of movies per user")


results <- evaluate(x = eval_sets, 
                    method = model_to_evaluate, 
                    n = seq(10, 100, 10))

head(getConfusionMatrix(results)[[1]])

eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, 
                                        data = getData(eval_sets, "unknown"), 
                                        byUser = TRUE)
head(eval_accuracy)

qplot(eval_accuracy[, "RMSE"]) + 
    geom_histogram(binwidth = 0.1) +
    ggtitle("Distribution of the RMSE by user")

eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, 
                                        data = getData(eval_sets, "unknown"), 
                                        byUser = FALSE) 
eval_accuracy

results <- evaluate(x = eval_sets, 
                    method = model_to_evaluate, 
                    n = seq(10, 100, 10))

head(getConfusionMatrix(results)[[1]])


columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(results))[, columns_to_sum]
head(indices_summed)

plot(results, annotate = TRUE, main = "ROC curve")
plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")


models_to_evaluate <- list(
    IBCF_cos = list(name = "IBCF", 
                    param = list(method = "cosine")),
    UBCF_cos = list(name = "UBCF", 
                    param = list(method = "cosine")),
    POPULAR_cor = list(name = "POPULAR", param = NULL),
    RANDOM = list(name = "RANDOM", param=NULL)
)

n_recommendations <- c(1, 3, 5, seq(10, 100, 10))
list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate, 
                         n = n_recommendations)

sapply(list_results, class) == "evaluationResults"

avg_matrices <- lapply(list_results, avg)
head(avg_matrices$IBCF_cos[, 5:8])


plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")


# hyper parameterization
vector_nn <- c(seq(10, 50, 10))
models_to_evaluate <- lapply(vector_nn, function(nn){
    list(name = "UBCF",
         param = list(method = "cosine", nn = nn))
})
names(models_to_evaluate) <- paste0("UBCF_nn_", vector_nn)

n_recommendations <- c(1, 5, seq(10, 100, 10))
list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate, 
                         n = n_recommendations)

plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")

#get results for all runs of 'random items'
eval_results <- getConfusionMatrix(list_results[[1]])
#alternatively, get avged result for 'random items'
avg(list_results)


# #k=5 meaning a 5-fold cross validation. given=3 meaning 3 items withheld for evaluation
# evaluation_scheme <- evaluationScheme(ratingmat, 
#                                       method = "cross-validation",
#                                       k = 5,
#                                       given = 3,
#                                       goodRating = 5
#                                       )
# 
# algorithms <- list(
#     "item-based CF" = list(name = "IBCF", param = list(method = "Cosine", k = 30)),
#     "popular items" = list(name = "POPULAR", param = NULL),
#     "user-based CF" = list(name = "UBCF", param = list(method = "Cosine", nn = 30))
#     )
# 
# evaluation_results <- evaluate(evaluation_scheme, algorithms, n = c(1, 3, 5, 10, 20)) #n=c denote top-N
# 
# plot(evaluation_results, legend = "bottomright") #plot the avged ROC
# plot(evaluation_results, "prec/rec") #plot the avged prec/rec
# 
# #get results for all runs of 'random items'
# eval_results <- getConfusionMatrix(evaluation_results[[1]])
# #alternatively, get avged result for 'random items'
# avg(evaluation_results[[1]])
