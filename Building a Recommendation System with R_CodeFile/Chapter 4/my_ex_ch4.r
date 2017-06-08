##### ch4-evaluate recommender system ##
library(tidyverse)
library(recommenderlab)
data("MovieLense")
ratings_movies <- MovieLense[rowCounts(MovieLense)>50,colCounts(MovieLense)>100]
ratings_movies
percentage_training <- 0.8 

min(rowCounts(ratings_Movies))

items_to_keep <- 15
rating_threshold <- 3
n_eval <- 1 # how many times run evaluation

eval_sets <- evaluationScheme(
    data = ratings_movies, method = "split",
    train = percentage_training, 
    given = items_to_keep, 
    goodRating = rating_threshold, k = n_eval)
    
eval_sets

getData(eval_sets,"train")
nrow(getData(eval_sets,"train"))/nrow(ratings_movies)

getData(eval_sets, "known")
nrow(getData(eval_sets,"known"))/nrow(ratings_movies)

getData(eval_sets, "unknown")
nrow(getData(eval_sets,"unknown"))/nrow(ratings_movies)


unique(rowCounts(getData(eval_sets, "known")))

qplot(rowCounts(getData(eval_sets,"unknown"))) + ggtitle("unknown items by the users")

#### k-fold valide model ####

n_fold <- 4

eval_sets <- 
  evaluationScheme(data = ratings_movies, 
                   method = "cross-validation",
                   k = n_fold, 
                   given = items_to_keep, 
                   goodRating = rating_threshold)

eval_sets %>% slotNames()
sapply(eval_sets@runsTrain,length)

###### evaluating rating #########
n_fold <- 4
items_to_keep <- 15
rating_threshold <- 3
eval_sets <- 
  evaluationScheme(data = ratings_movies, 
                   method = "cross-validation",
                   k = n_fold,
                   given = items_to_keep, 
                   goodRating = rating_threshold)

model_to_evaluate <- "IBCF"
model_parameters <- NULL

eval_recommender <- 
  Recommender(data = getData(eval_sets, "train"),
              method = model_to_evaluate, 
              parameter = model_parameters)

items_to_recommend <- 10


eval_prediction <- 
  predict(object = eval_recommender, 
                           newdata =getData(eval_sets, "known"),
                           n = items_to_recommend, type = "ratings")


### 
eval_acc <- calcPredictionAccuracy(
  x=eval_prediction,
  data = getData(eval_sets,"unknown"),byUser=T
)
head(eval_acc)

qplot(eval_acc[, "RMSE"]) + geom_histogram(binwidth = 0.1) +
  ggtitle("Distribution of the RMSE by user")


eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =
    FALSE) 
eval_accuracy


##### test a model ####
results <- evaluate(x=eval_sets,method = model_to_evaluate,n=seq(10,100,10))
class(results)
head(getConfusionMatrix(results)[[1]])

columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", 
                         getConfusionMatrix(results))[, columns_to_sum]

plot(results, annotate = TRUE, main = "ROC curve")
plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")
####### compare multi models ######

models_to_evaluate <- list(
  IBCF_cos = list(name = "IBCF", param = list(method =
                                                "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method =
                                                "pearson")),
  UBCF_cos = list(name = "UBCF", param = list(method =
                                                "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method =
                                                "pearson")),
  random = list(name = "RANDOM", param=NULL),
  popular = list(name= "POPULAR",param=NULL)
)

n_recommendations <- c(1, 5, seq(10, 100, 10))

list_results <- evaluate(x = eval_sets, method = models_to_evaluate, 
                         n= n_recommendations)

avg_matrices <- lapply(list_results, avg)

avg_matrices$IBCF_cos[,5:8]

plot(list_results, annotate = 1, legend = "topleft")
title("ROC curve")


#### multiple hyper-parameters ####
vector_k <- c(5, 10, 20, 30, 40)
models_to_evaluate <- lapply(vector_k, function(k){
  list(name = "IBCF", param = list(method = "cosine", k = k))
})

models_to_evaluate
names(models_to_evaluate) <- paste0("IBCF_k_", vector_k)

names(models_to_evaluate)

# Using the same commands as we did earlier, let's build and evaluate
# the models:
n_recommendations <- c(1, 5, seq(10, 100, 10))
list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n= n_recommendations)

plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")
