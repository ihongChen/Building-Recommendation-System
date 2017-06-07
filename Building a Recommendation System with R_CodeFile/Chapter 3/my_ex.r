##### ch3 . recommenderlab #### 

library(recommenderlab)
library(tidyverse)
data_package <- data(package = "recommenderlab")

data_package %>% class()
data("MovieLense")
MovieLense ## 943 x1644 ui rating matrix 
class(MovieLense)
methods(class= class(MovieLense)) # list all method for this object 

object.size(MovieLense)
object.size(as(MovieLense,"matrix")) 

## compare size sparese matrix vs matrix
object.size(as(MovieLense,"matrix")) /object.size(MovieLense)

## compute similarity 
similarity_user <- similarity(MovieLense[1:4,],method = "cosine",which="user")
class(similarity_user)
as.matrix(similarity_user)

image(as.matrix(similarity_user),main="User similarity")

similarity_item <- similarity(MovieLense[,1:4], method="cosine",which="items")
as(similarity_item,"matrix")

image(as.matrix(similarity_item,main = "Item similarity"))

#### Recommendation models ####
recommender_models <- recommenderRegistry$get_entries(dataType="realRatingMatrix")
names(recommender_models)

lapply(recommender_models,"[[","description")

recommender_models$IBCF_realRatingMatrix$parameters
## doc :: help(package= "recommenderlab")

#### MovieLens datasets -1 : data explore ####
typeof(MovieLense) ## s4 class 
class(MovieLense)
slotNames(MovieLense) ## data, normalize slots
MovieLense@data %>%  class() ## belogn to dgcMatrix class,
dim(MovieLense@data) #

vector_ratings <- as.vector(MovieLense@data)
unique(vector_ratings)
table_ratings <- table(vector_ratings)
table_ratings
vector_ratings <- vector_ratings[vector_ratings!=0]
vector_ratings <- factor(vector_ratings)

qplot(vector_ratings) + ggtitle('Distribution of ratings')

## colCounts , colMeans,
views_per_movie <- colCounts(MovieLense)
table_views <- tibble(
  movie = names(views_per_movie),
  views = views_per_movie
)
table_views <- table_views %>% arrange(desc(views))

ggplot(table_views[1:6,],aes(x=movie,y=views)) +
  geom_bar(stat="identity") +
  theme(axis.text.x =
        element_text(angle = 45, hjust = 1)) + 
  ggtitle("Number of views of the top movies")

## avg rating 
average_ratings <- colMeans(MovieLense)
qplot(average_ratings) + stat_bin(binwidth = 0.1) + 
  ggtitle("Distrubtion of the average Movie rating")
average_ratings 
average_ratings_relevant <- average_ratings[views_per_movie>100]
qplot(average_ratings_relevant) + stat_bin(binwidth = 0.1) +
  ggtitle(paste("Distribution of the relevant average ratings"))


## visual matrix 
image(MovieLense,main="Heat map of rating matrix")

# smaller piece of rating matrix
image(MovieLense[1:10,1:15])

#  visual top 1% users
min_n_movies <- quantile(rowCounts(MovieLense),0.99)
min_n_users <- quantile(colCounts(MovieLense),0.99)
min_n_users

image(MovieLense[rowCounts(MovieLense) > min_n_movies,
                 colCounts(MovieLense) > min_n_users],
  main = "Heatmap of the top users and movies")

############################################
### Data preparation 
############################################
##### 1. select most relavant data  ######

## rule of thumbs 
## Users who have rated at least 50 movies
## Movies that have been watched at least 100 times

ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                             colCounts(MovieLense) > 100]
ratings_movies ## 560 * 332 
min_movies <- quantile(rowCounts(ratings_movies), 0.98)
min_users <- quantile(colCounts(ratings_movies), 0.98)
qplot(rowCounts(ratings_movies))

image(ratings_movies[rowCounts(ratings_movies) > min_movies,
                     colCounts(ratings_movies) > min_users], 
      main = "Heatmap of the top users and movies")

average_ratings_per_user <- rowMeans(ratings_movies)

qplot(average_ratings_per_user) + stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average rating per user")

### normalize the datasets ##

ratings_movies_norm <- normalize(ratings_movies)
sum(rowMeans(ratings_movies_norm) > 0.00001)
# visualize the normalized matrix
image(ratings_movies_norm[
  rowCounts(ratings_movies_norm) > min_movies,
  colCounts(ratings_movies_norm) > min_users], 
  main = "Heatmap of the top users and movies")

### binarize the data ###

rating_movies_watched <- binarize(ratings_movies, minRating = 1)
#### 2. Normalize data####


min_movies_binary <- quantile(rowCounts(ratings_movies), 0.95)
min_users_binary <- quantile(colCounts(ratings_movies), 0.95)

image(rating_movies_watched[rowCounts(ratings_movies) > min_movies_binary,
        colCounts(ratings_movies) > min_users_binary], main = "Heatmap of the top users and movies")

########################################################
##   IBCF   ###
########################################################


which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_movies),
                      replace = TRUE, prob = c(0.8, 0.2))

recc_data_train <-ratings_movies[which_train,]
recc_data_test <- ratings_movies[!which_train,]

## k=5 fold 
which_set <- sample(x = 1:5, size = nrow(ratings_movies), replace =TRUE)
for(i_model in 1:5) {
  which_train <- which_set == i_model
  recc_data_train <- ratings_movies[which_train, ]
  recc_data_test <- ratings_movies[!which_train, ]
  # build the recommender
}

########### build the model ###############
recommender_models <- 
  recommenderRegistry$get_entries(dataType =
                                    "realRatingMatrix")
recommender_models$IBCF_realRatingMatrix$parameters

recc_model <- Recommender(data = recc_data_train,method="IBCF",parameter=list(k=30))
recc_model
class(recc_model)

model_details <- getModel(recc_model)
model_details$description
model_details$k
class(model_details$sim)

n_items_top <- 20 
image(model_details$sim[1:n_items_top, 1:n_items_top],main="Heatmap")


row_sums <- rowSums(model_details$sim > 0)
table(row_sums)
col_sums <- colSums(model_details$sim > 0)
qplot(col_sums) + stat_bin(binwidth = 1) + ggtitle("Distribution of the column count")
n_recommended <- 6
recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended)
slotNames(recc_predicted)
class(recc_predicted)
recc_predicted@items[1:5]
recc_predicted@itemLabels[1:10]

recc_user_1 <- recc_predicted@items[[1]]
recc_predicted@itemLabels[recc_user_1]
movies_user_1 <- recc_predicted@itemLabels[recc_user_1]

recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_movies)[x]
})

dim(recc_matrix)
recc_matrix[,1:4]
number_of_items <- factor(table(recc_matrix))
chart_title <- "Distribution of the number of items for IBCF"

qplot(number_of_items) + ggtitle(chart_title)



########################################################
##   UBCF   ###
########################################################


