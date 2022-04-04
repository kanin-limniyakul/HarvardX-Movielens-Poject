
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           #title = as.character(title),
                                           #genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Exploratory Data Analysis (EDA)

edx_sum <- data.frame(np_of_rows = nrow(edx),
                          no_of_column = ncol(edx),
                          no_of_users = n_distinct(edx$userId),
                          no_of_movies = n_distinct(edx$movieId),
                          avg_rating = round(mean(edx$rating),2),
                          no_of_genres = n_distinct(edx$genres))

knitr::kable (edx_sum , caption = "Summary of edx dataset" )


## General Rating Information


hist(edx$rating,main ="Rating distribution in edx data set", 
                xlab = "Rating", ylab = "Frequency (n)")
abline(v = mean(edx$rating), col = 'red', lwd=3, lty=2) #mean vertical line
rate_sum <- edx %>% group_by(rating) %>% summarize(n())
knitr::kable (rate_sum, caption = "Rating Summary")


## General Movies information

movie <- edx %>% group_by(movieId) %>% summarise( n = n()) 

movie_top5 <- movie %>% top_n(5)
knitr::kable (movie_top5, caption = "Top 5 rated movies")

#histogram of Number of Ratings for each Movies

hist_id_count <- edx %>% 
                 count(movieId) %>% 
                 ggplot(aes(n)) + 
                 geom_histogram(color = "black", bins = 30) + 
                 scale_x_log10() +
                 xlab("movieId") +
                 ylab("Number of Ratings") +
                 labs(title = "Number of Ratings for each movie",
                      caption= "Data Source : edx dataset")
                 
hist_id_count

## General User information

user <- edx %>% group_by(userId) %>% summarize(n=n(),avg_rating = mean(rating))


hist(user$n, main = "Number of rating per user in edx data set", xlab = "no rating per user", ylab = "Frequency (n)")
abline(v = median(user$n), col = 'blue', lwd=3, lty=2) #median vertical line
abline(v = mean(user$n), col = 'red', lwd=3, lty=2) #mean vertical line

edx %>% group_by(userId) %>%
  summarise(avg_rating = sum(rating)/n()) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(bins=30, color = I("black")) +
  labs(x = "Average rating", y = "Number of users", caption = "Source: edx dataset")+
  labs(title = "Average Ratings Distribution",
       caption= "Data Source : edx dataset")


## General Genres Information

genres <- edx %>% group_by(genres) %>% summarize(no_of_review =n(),avg_rating = mean(rating))

top10_genres <- genres %>% top_n(10,no_of_review) 

knitr::kable (top10_genres, caption = "Top 10 most reviewed genres")

genres_effect <- edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 150000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Genre combination", y = "Average Rating", caption = "Data Source: edx dataset")

genres_effect
`
## General Release date information

edx <- edx %>% mutate(edx, date = as_datetime(timestamp))


edx %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()


# Methodology and Analysis

## Data partitioning of edx dataset 

# Create train set and test sets from edx
set.seed(2022, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
tempo <- edx[test_index,]

# Ensure movieId and userID in test set are also in train set
test_set <- tempo %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(tempo, test_set) 
train_set <- rbind(train_set, removed)

# Remove temporary files to tidy environment
rm(test_index, tempo, removed) 


## Residual Mean Square Error(RMSE)


RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))

## Simple Model

mu_hat <- mean(train_set$rating)

rmse_simple <- RMSE(test_set$rating, mu_hat)


rmse_results <- data_frame(method = "Average", RMSE = rmse_simple)
knitr::kable (rmse_results, caption = "RSME Results - Simple Average")


## Add Movie Effect

mu <- mean(train_set$rating)  
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# Predict ratings adjusting for movie effects

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)



rmse_bi <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results, data_frame(method = "Average + b_i", RMSE = rmse_bi))

knitr::kable (rmse_results, caption = "RSME Results - Add Movie Effect")


## Add User Effect

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Predict ratings adjusting for movie and user effects
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)


rmse_bi_bu <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results, data_frame(method = "Average + b_i + b_u", RMSE = rmse_bi_bu))

knitr::kable (rmse_results, caption = "RSME Results - Add user effect")


## Add Genres Effect

genre_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarise(b_g = mean(rating - mu_hat - b_i - b_u))

# Predict ratings adjusting for movie, user and genre effects
predicted_b_g <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  pull(pred)
# Calculate RMSE based on genre effects model
rmse_bi_bu_bg <- RMSE(predicted_b_g, test_set$rating)
rmse_results <- bind_rows(rmse_results, 
                          data_frame(method = "Average + b_i + b_u + b_g", RMSE = rmse_bi_bu_bg))
knitr::kable (rmse_results, caption = "RSME Results - Add genres effect")


## Regularization 

#Finding the best lamda to yield lowest RMSE

lambdas <- seq(3, 6, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarise(b_g = sum(rating - b_i - b_u - mu_hat)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses) 
#abline(h = lambdas[which.min(rmses)], col = 'red', lwd=3, lty=2) #mean vertical line


lambda <- lambdas[which.min(rmses)]
lambda

rmse_regularised <- min(rmses) 
rmse_results <- bind_rows(rmse_results, 
                          data_frame(method = "Regularised average + b_i + b_u + b_g", RMSE = rmse_regularised))
knitr::kable (rmse_results, caption = "RSME Results - Regularised")


# Matrix Factorization

if(!require(recosystem)) 
  install.packages("recosystem", repos = "http://cran.us.r-project.org")
set.seed(2022, sample.kind = "Rounding") # This is a randomized algorithm



# Convert the train and test sets into recosystem input format
train_data <-  with(train_set, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
test_data  <-  with(test_set,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))

# Create the model object
r <-  recosystem::Reco()

# Select the best tuning parameters
opts <- r$tune(train_data, opts = list(dim = c(10, 20, 30), 
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = 4, niter = 10))

# Train the algorithm  
r_train<- r$train(train_data, opts = c(opts$min, nthread = 4, niter = 20))

# Calculate the predicted values  
y_hat_reco <-  r$predict(test_data)
head(y_hat_reco, 10)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

rmse_reco <- RMSE(test_set$rating, y_hat_reco)

rmse_results <- bind_rows(rmse_results, 
                          data_frame(method = "Matrix Factorizarion", RMSE = rmse_reco))
knitr::kable (rmse_results, caption = "RSME Results - Matrix Factorization")


# Model Validation

b_i_v <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i_v = sum(rating - mu)/(n()+lambda))

b_u_v <- edx %>% 
  left_join(b_i_v, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u_v = sum(rating - b_i_v - mu)/(n()+lambda))

b_g_v <- edx %>%
  left_join(b_i_v, by="movieId") %>%
  left_join(b_u_v, by="userId") %>%
  group_by(genres) %>%
  summarise(b_g_v = sum(rating - b_i_v - b_u_v - mu)/(n()+lambda))

predicted_ratings <- 
  validation %>% 
  left_join(b_i_v, by = "movieId") %>%
  left_join(b_u_v, by = "userId") %>%
  left_join(b_g_v, by = "genres") %>%
  mutate(pred = mu + b_i_v + b_u_v + b_g_v) %>%
  pull(pred)

rsme_final_validation <- RMSE(predicted_ratings,validation$rating)
rmse_validation <-  data_frame(method = "Validate Regularised average + b_i + b_u + b_g", RMSE = rsme_final_validation)
knitr::kable (rmse_validation, caption = "RSME validation")

#Validation factorization

if(!require(recosystem)) 
  install.packages("recosystem", repos = "http://cran.us.r-project.org")
set.seed(2022, sample.kind = "Rounding") # This is a randomized algorithm

# Convert 'edx' and 'validation' sets to recosystem input format
edx_reco <-  with(edx, data_memory(user_index = userId, 
                                   item_index = movieId, 
                                   rating = rating))
validation_reco  <-  with(validation, data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating = rating))

# Create the model object
r <-  recosystem::Reco()

# Tune the parameters
opts <-  r$tune(edx_reco, opts = list(dim = c(10, 20, 30), 
                                      lrate = c(0.1, 0.2),
                                      costp_l2 = c(0.01, 0.1), 
                                      costq_l2 = c(0.01, 0.1),
                                      nthread  = 4, niter = 10))

# Train the model
r$train(edx_reco, opts = c(opts$min, nthread = 4, niter = 20))

# Calculate the prediction
y_hat_final_reco <-  r$predict(validation_reco, out_memory())

rsme_reco_validation <- RMSE(y_hat_final_reco,validation$rating)

rsme_reco_validation

rmse_validation <-  bind_rows(rmse_validation, data_frame(method = "Validate matrix factorization", RMSE = rsme_reco_validation))
knitr::kable (rmse_validation, caption = "RSME validation with Matrix Factorization")



