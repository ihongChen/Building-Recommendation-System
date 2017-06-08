##### 
library(tidyverse)
library(recommenderlab)
library(data.table)
library(countrycode)
col_types = 
  cols(
    mpg = col_double(),
    cyl = col_integer(),
    disp = col_double(),
    hp = col_integer(),
    drat = col_double(),
    vs = col_integer(),
    wt = col_double(),
    qsec = col_double(),
    am = col_integer(),
    gear = col_integer(),
    carb = col_integer()
  )

table_in <- read_csv("./anonymous-msweb.txt",
                     col_names = F
                       )
table_in[,1:3]
### define users table ###
setnames(table_in,1:2,c("category","value"))
table_users <- table_in[,1:2]  

table_users <-
table_users %>% 
  filter(category %in% c("C","V") )
###################################################
#### define rating matrix ###
##################################################
# table_users[, chunk_user := cumsum(category == "C")] ## only work for Data.table
table_users <- 
table_users %>% 
  # filter(category=="C") %>%
  mutate(chunk_user = cumsum(category=="C"))

table_users2 <- table_users %>% filter(category=='C')

temp2 <- temp %>% 
  filter(category=="C")

table_long<-
table_users2 %>% right_join(table_users,by = c("chunk_user")) %>% 
  mutate(user = value.x,item = value.y) %>% 
  filter(category.y=='V') %>% 
  select(chunk_user,user,item)

head(table_long) ### transaction data (long).... ###

### transform to user-item format ###

temp <- table_long[1:100,]
temp2<-
temp %>% dcast(chunk_user+user ~ paste0("item_",item),drop=T) %>% 
  select(starts_with("item_")) 

temp2[!is.na(temp2)] <- 1
temp2

table_wide <- 
table_long %>% dcast(chunk_user+user ~ paste0("item_",item),drop=T)
  
rownames(table_wide) <- table_wide[,"user"] 
table_wide[,"user"] <- NULL
table_wide[,"chunk_user"] <- NULL
# colnames(table_wide)[1:5]
# rownames(table_wide)[1:5]
table_wide[!is.na(table_wide)] <- 1
table_wide[1:5,1:5] ### u-i data.frame 

matrix_wide <- as.matrix(table_wide)
colnames(matrix_wide)
# head(matrix_wide[,1:6])
matrix_wide %>% dim()

matrix_wide[is.na(matrix_wide)] <- 0
ratings_matrix <- as(matrix_wide,"binaryRatingMatrix")
ratings_matrix ### convert to binaryRatingMatrix ... sparse

image(ratings_matrix[1:50, 1:50], main = "Binary rating matrix")

#####

n_users <- colCounts(ratings_matrix)
qplot(n_users) 

qplot(n_users[n_users < 100]) + stat_bin(binwidth = 10) +
  ggtitle("Distribution of the number of users")

ratings_matrix <- 
ratings_matrix[,colCounts(ratings_matrix)>=5]
colCounts(ratings_matrix)

sum(rowCounts(ratings_matrix)==0)
ratings_matrix <- ratings_matrix[rowCounts(ratings_matrix) >= 5, ]

ratings_matrix

#############################################
##### Extracting item attributes #####
#############################################
table_in <- read.csv('./anonymous-msweb.txt', header = FALSE) ## read_csv lost some column

table_items <- table_in %>% 
  filter(V1 == "A")
table_in %>% head(20)
table_items <- 
table_items %>%  mutate(id= V2,description=V4,url=V5) %>% 
  select(-starts_with("V"))

name_countries <- c(countrycode_data$country.name,
                    "Taiwan", "UK", "Russia", "Venezuela",
                    "Slovenija", "Caribbean", "Netherlands (Holland)",
                    "Europe", "Central America", "MS North Africa")
table_items[description %in% name_countries,]
table_items$description %in% name_countries

table_items <- 
table_items %>% mutate(
  category = ifelse(description %in% name_countries,'region','product')
)

# table_items[, list(n_items = .N), by = category]  
table(table_items$category)

##################################################
########## BUILD MODEL ##########
##################################################

which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_matrix),
                      replace = TRUE,
                      prob = c(0.8, 0.2))

recc_data_train <- ratings_matrix[which_train, ]
recc_data_test <- ratings_matrix[!which_train, ]


recc_model <- Recommender(data = recc_data_train,
                          method = "IBCF",
                          parameter = list(method = "Jaccard"))
# recc_model@model$sim
dim(recc_model@model$sim)

image(recc_model@model$sim)

range(recc_model@model$sim)

dist_ratings <- as(recc_model@model$sim, "matrix") ## distance from purchase history
table_items[, 1 - dist(category == "product")]

dist_category <- 1-dist(table_items$category == "product")
class(dist_category)

dist_category <- as.matrix(dist_category)

rownames(dist_category) <- table_items[,'id']
colnames(dist_category) <-table_items[,'id']
vector_items <- rownames(dist_ratings)

vector_items <- substring(vector_items,6,9)

dist_category <- dist_category[vector_items, vector_items]
image(dist_category)
### hybrid 2 kinds of dist ####
weight_category <- 0.2
dist_tot <- dist_category * weight_category + dist_ratings * (1 - weight_category)
image(dist_tot)

recc_model@model$sim <- as(dist_tot, "dgCMatrix")


### predict ##
n_recommended <- 10
recc_predicted <- predict(object = recc_model,
                          newdata = recc_data_test,
                          n = n_recommended)
head(recc_predicted@itemLabels)

recc_user_1 <- recc_predicted@items[[1]]

