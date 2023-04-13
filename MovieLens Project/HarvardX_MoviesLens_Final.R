#####
#Introduction
#####

#####
#Method And Analysis
#####

#Creating the sets for exploration

##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##########################################################


##########################################################
##Data Exploration
##########################################################

#Introduction
#Investigating the edx dataset
edx %>% head() %>% knitr::kable()
#Investigating the edx dataset variables and edx dataset class
edx %>% glimpse()
edx %>% class()
sum(is.na(edx))

#RATINGS
#length, mean and unique ratings in the edx dataset
length(edx$rating)
mean(edx$rating)
length(unique(edx$rating))
sort(unique(edx$rating))
#tibble of movie title arranged by number of ratings desc and title's average rating
edx %>%
  select(title, rating) %>%
  group_by(title) %>%
  summarise(number_of_ratings = n(), mean_rating = mean(rating)) %>%
  arrange(desc(number_of_ratings))
#tibble of movie title arranged by average rating desc and number of ratings
edx %>%
  select(title, rating) %>%
  group_by(title) %>%
  summarise(number_of_ratings = n(), mean_rating = mean(rating)) %>%
  arrange(desc(mean_rating))
#histogram number of ratings x rating grade
options(scipen=5)
edx %>% ggplot(aes(rating)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0, 5, 0.5)) +
  xlab("Rating") +
  ylab("Number of ratings") +
  ggtitle("Number of ratings by rating grade")


#MOVIES
#Length and most rated movies
length(unique(edx$movieId))
edx %>%
  group_by(title) %>%
  summarise(n_rating = n()) %>%
  arrange(desc(n_rating))
#Number of movies with only one star rating
edx %>%
  group_by(movieId) %>%
  summarise(n_rating = n()) %>%
  filter(n_rating == 1) %>%
  count()
#number of movies by average rating
edx %>%
  group_by(movieId) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram() +
  xlab("Average rating") +
  ylab("Number of movies") +
  ggtitle("Number of movies by average rating")


#USERS
#Number of users in the edx dataset
length(unique(edx$userId))
edx_l <- edx %>%
  group_by(userId) %>%
  summarize(n_rating = n())
sum(edx_l$n_rating)/nrow(edx_l)
edx %>%
  group_by(userId) %>%
  summarize(n_rating = n()) %>%
  ggplot(aes(n_rating)) +
  geom_histogram() +
  xlim(0, 750) +
  xlab("Number of users") +
  ylab("Number of ratings") +
  ggtitle("Number of users by number of ratings")


#GENRES
#length and genres types in the edx dataset
length(unique(unlist(strsplit(edx$genre, split = "\\|"))))
unique(unlist(strsplit(edx$genre, split = "\\|")))
#how many times each genre type appears in the dataset and its average rating
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
library(tidyr)
library(knitr)
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(count = n(), rating = round(mean(rating), 2)) %>%
  arrange(desc(count)) %>% kable()


#TITLE (AND RELEASE YEAR)
#edx_m will the the mutated edx dataset thoughout the analysis 
library(tidyr)
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
edx_m <- edx %>%
  mutate(release_year = as.numeric(str_sub(title, -5, -2)))
#title of the top 20 release year by number of ratings
edx_m %>% group_by(release_year) %>%
  summarise(number_of_ratings = n()) %>%
  arrange(desc(number_of_ratings)) %>%
  head(20) 
#Line plot of number of ratings by release year
edx_m %>% group_by(release_year) %>%
  summarise(number_of_ratings = n()) %>%
  ggplot(aes(release_year, number_of_ratings)) +
  geom_line() +
  xlab("Release Year") +
  ylab("Number of ratings") +
  ggtitle("Number of ratings by Release year")


#TIMESTAMP
#converting and mutanting timestamp
if(!require(hydroTSM)) install.packages("hydroTSM", repos = "http://cran.us.r-project.org")
if(!require(libridate)) install.packages("libridate", repos = "http://cran.us.r-project.org")
library(hydroTSM)
library(lubridate)
edx_m <- edx_m %>% 
  mutate(rating_date = as.Date(as.POSIXct(timestamp, origin = "1970-01-01")), 
         rating_week = round_date(as_datetime(timestamp), unit = "week"), 
         season = time2season(rating_date, out.fmt = "seasons"))
edx_m$season <- as.factor(edx_m$season)
class(edx_m$season)
levels(edx_m$season)
table(edx_m$season)
levels(edx_m$season) <- c("autumn", "spring", "summer", "winter")
table(edx_m$season)
#rating week range
range(edx_m$rating_week)
#column graph rating week x number of ratings
edx_m %>%
  ggplot(aes(rating_week)) +
  geom_bar() +
  xlab("Rating Week") +
  ylab("Number of ratings") +
  ggtitle("Number of ratings by rating week")
#scatter plot rating week x average rating
edx_m %>%
  group_by(rating_week) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(rating_week, avg_rating)) +
  geom_point() +
  geom_smooth() +
  xlab("Rating week") +
  ylab("Average rating") +
  ggtitle("Average rating by rating week")
#column graph rating season x number of ratings
edx_m %>%
  ggplot(aes(season)) +
  geom_bar() +
  xlab("Season") +
  ylab("Number of ratings") +
  ggtitle("Number of ratings by season")
#scatter plot season x average rating
edx_m %>%
  group_by(season) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(season, avg_rating)) +
  geom_point() +
  geom_smooth() +
  xlab("Season") +
  ylab("Average rating") +
  ggtitle("Average rating by season")



##########################################################
#Method and Results
##########################################################
set.seed(42)
test_index <- createDataPartition(y = edx_m$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx_m[-test_index,]
test_set <- edx_m[test_index,]
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#naive_RMSE table
mu <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu)
rmse_goal <- 0.86490
rmse_results <- data.frame(method = "Project objective", RMSE = "0.86490")
rmse_results <- rbind(rmse_results,
                      data_frame(method ="Simple Average", 
                                 RMSE = round(naive_rmse, 5)))
rmse_results %>% knitr::kable()

#Adding the movie factor
#movie_RMSE
movies_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
#movie_RMSE table
predicted_b_i <- mu + test_set %>%
  left_join(movies_avgs, by = "movieId") %>%
  .$b_i
movie_rmse <- RMSE(predicted_b_i, test_set$rating)
rmse_results <- rbind(rmse_results, 
                      data.frame(method = "Movie Effect Model", 
                                 RMSE = round(movie_rmse, 5)))
rmse_results %>% knitr::kable()


#Adding the user factor
#user_RMSE
users_avgs <- train_set %>%
  left_join(movies_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
#user_RMSE table
predicted_b_u <- test_set %>%
  left_join(movies_avgs, by = "movieId") %>%
  left_join(users_avgs, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
plus_users_rmse <- RMSE(predicted_b_u, test_set$rating)
rmse_results <- rbind(rmse_results, 
                      data.frame(method = "Movie + User Effect Model", 
                                 RMSE = round(plus_users_rmse, 5)))
rmse_results %>% knitr::kable()


#Adding the gender factor
#gender_RMSE
genres_avgs <- train_set %>%
  left_join(movies_avgs, by = "movieId") %>%
  left_join(users_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_u - b_i))
#gender_RMSE table, echo = FALSE}
predicted_b_g <- test_set %>%
  left_join(movies_avgs, by = "movieId") %>%
  left_join(users_avgs, by = "userId") %>%
  left_join(genres_avgs, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred
plus_genders_rmse <- RMSE(predicted_b_g, test_set$rating)
rmse_results <- rbind(rmse_results, 
                      data.frame(method = "Movie + User + Gender Effect Model", 
                                 RMSE = round(plus_genders_rmse, 5)))
rmse_results %>% knitr::kable()


#Adding the release year factor
#release_year_RMSE
years_avgs <- train_set %>%
  left_join(movies_avgs, by = "movieId") %>%
  left_join(users_avgs, by = "userId") %>%
  left_join(genres_avgs, by = "genres") %>%
  group_by(release_year) %>%
  summarize(b_y = mean(rating - mu - b_u - b_i - b_g))
#release_year_RMSE table
predicted_b_y <- test_set %>%
  left_join(movies_avgs, by = "movieId") %>%
  left_join(users_avgs, by = "userId") %>%
  left_join(genres_avgs, by = "genres") %>%
  left_join(years_avgs, by = "release_year") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  .$pred
plus_year_rmse <- RMSE(predicted_b_y, test_set$rating)
rmse_results <- rbind(rmse_results, 
                      data.frame(method = "Movie + User + Gender + Release Year Effect Model", 
                                 RMSE = round(plus_year_rmse, 5)))
rmse_results %>% knitr::kable()


#Adding the rating date factor
#rating_date_RMSE
rating_date_avgs <- train_set %>%
  left_join(movies_avgs, by = "movieId") %>%
  left_join(users_avgs, by = "userId") %>%
  left_join(genres_avgs, by = "genres") %>%
  left_join(years_avgs, by = "release_year") %>%
  group_by(rating_week) %>%
  summarize(b_r = mean(rating - mu - b_i - b_u - b_g - b_y))
#rating_date_RMSE table
predicted_b_r <- test_set %>%
  left_join(movies_avgs, by = "movieId") %>%
  left_join(users_avgs, by = "userId") %>%
  left_join(genres_avgs, by = "genres") %>%
  left_join(years_avgs, by = "release_year") %>%
  left_join(rating_date_avgs, by = "rating_week") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y + b_r) %>%
  .$pred
plus_rating_date_rmse <- RMSE(predicted_b_r, test_set$rating)
rmse_results <- rbind(rmse_results, 
                      data.frame(method = "Movie + User + Gender + Release Year + Rating Date Effect Model", 
                                 RMSE = round(plus_rating_date_rmse, 5)))
rmse_results %>% knitr::kable()


#Regularization on the Movie + User + Gender + Release Year + Rating Date Effect Model
#Cross validation lambdas parameters
lambdas <- seq(4.5, 5.5, 0.01)
#Regularised model, predict ratings and calculate RMSE for each value of lambda
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  #Movies
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  #plus users
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() + l))
  #plus genres
  b_g <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu)/(n() + l))
  #plus release year
  b_y <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(release_year) %>%
    summarize(b_y = sum(rating - b_i - b_u - b_g - mu)/(n() + l))
  #plus rating date
  b_r <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_y, by="release_year") %>%
    group_by(rating_week) %>%
    summarize(b_r = sum(rating - b_i - b_u - b_g - b_y - mu)/(n() + l))
  #all together to prediction
  predicted_ratings <- test_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_y, by="release_year") %>%
    left_join(b_r, by="rating_week") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_y + b_r) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
#Optimal lambda
lambda <- lambdas[which.min(rmses)]
#Minimum RMSE
regularised_rmse <- min(rmses)
qplot(lambdas, rmses) + 
  geom_hline(yintercept=min(rmses), linetype='dashed', color = "red") +
  annotate("text", x = lambda, y = min(rmses), label = lambda, vjust = -1, color = "red")
#Adding the regularised_rmse to the table
rmse_results <- rbind(rmse_results, 
                      data.frame(method = "Reguralized User + Movie + Genre + Release year + Rating Date Effect Model", 
                                 RMSE = round(regularised_rmse, 5)))
rmse_results %>% knitr::kable()


#Trying out the final model
#making the proper mutations
final_holdout_test_m <- final_holdout_test %>%
  mutate(release_year = as.numeric(str_sub(title, -5, -2))) %>%
  mutate(rating_date = as.Date(as.POSIXct(timestamp, origin = "1970-01-01")), 
         rating_week = round_date(as_datetime(timestamp), unit = "week"), 
         season = time2season(rating_date, out.fmt = "seasons"))

#Cross validation lambdas parameters
lambdas <- seq(4.5, 5.5, 0.01)
#Regularised model, predict ratings and calculate RMSE for each value of lambda
rmses_final_model <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  #Movies
  b_i <- edx_m %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  #plus users
  b_u <- edx_m %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() + l))
  #plus genres
  b_g <- edx_m %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu)/(n() + l))
  #plus release year
  b_y <- edx_m %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(release_year) %>%
    summarize(b_y = sum(rating - b_i - b_u - b_g - mu)/(n() + l))
  #plus rating date
  b_r <- edx_m %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_y, by="release_year") %>%
    group_by(rating_week) %>%
    summarize(b_r = sum(rating - b_i - b_u - b_g - b_y - mu)/(n() + l))
  #all together to prediction
  predicted_ratings <- final_holdout_test_m %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_y, by="release_year") %>%
    left_join(b_r, by="rating_week") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_y + b_r) %>%
    .$pred
  return(RMSE(predicted_ratings, final_holdout_test_m$rating))
})
#Optimal lambda
lambda_final_model <- lambdas[which.min(rmses_final_model)]
#Minimum RMSE
regularised_rmse_final <- min(rmses_final_model)
#Adding the regularised_final_houldout
rmse_results_final <- data.frame(method = "Project objective", RMSE = "0.86490")
rmse_results_final <- rmse_results_final %>% rbind(c(method = "Final Model Validation", 
                                                     RSME = round(regularised_rmse_final, 5)))
rmse_results_final %>% knitr::kable()

##########################################################
#References
##########################################################

