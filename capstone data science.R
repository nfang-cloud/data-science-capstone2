##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes
remove.packages("rlang")
install.packages("rlang")
install.packages("vctrs")
install.packages("dplyr")

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
head(ratings)
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

head(movies)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")
head(movielens)

#######################################################
##################### Data clean #####################
######################################################

# Validation set will be 10% of MovieLens data
set.seed(1) # if using R 3.5 or earlier, use `set.seed(1)`
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

#check the dimiasion of edx
dim(edx)
#Check the data set of edx
head(edx)
#summary of edx
summary(edx)

#Since the year of movie is saved in title, we need extract the year and create a new variable
edx <- edx %>% mutate(year = as.numeric(str_sub(title, -5,-2)))
validation <- validation %>% mutate(year = as.numeric(str_sub(title, -5,-2)))


# split the genres
split_edx <- edx %>% separate_rows(genres, sep = "\\|")
split_validation <- validation %>% separate_rows(genres, sep = "\\|")
#double check the datasets
head(edx)
head(split_edx)


####################################################
#########################Data exploration###########
###################################################

########1. General checking ##########
# Check the number of unique users and unique movies, compare the number of observation and total number of the unique users times unique movies
edx %>% summarize (n_users = n_distinct(userId),n_movies = n_distinct(movieId),n_edx=nrow(edx), n_total=n_users*n_movies)

# Thus, not every user rates every movie



#########2. Movie bias############

# check the distribution of movie's rating times
edx %>% count(movieId) %>%
  ggplot(aes(n))+
  geom_histogram(bins=30, color="black")+
  scale_x_log10()+
  ggtitle("Movies")
# Some movies have more ratings compared to others
# Some movie gets very few ratings which will affect our prediction resutls

#########3. User bias############

#####Check the ratings distibution

edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
edx %>% group_by(rating) %>% summarize(count = n()) %>%
  arrange(desc(count))
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()
# we can see that users are likely to rate the movie between 3 to 4

# check the distribution of each user's rating times
edx %>% count(userId) %>%
  ggplot(aes(n))+
  geom_histogram(bins=30, color="black")+
  scale_x_log10()+
  ggtitle("Users")

# Some users are likely to give ratings, while others may not. 
#This indicates there are users bias

#########4. Year bias############
# check the movie rating number by year
year_rating <- edx %>% group_by(year) %>%
  summarize(count = n(), avg_rating = mean (rating)) %>% 
  arrange(desc(count))
year_rating

ggplot(data=year_rating, aes(x=year, y=count))+geom_line()
# The count of rating increase when year increase and the maximum of rating counts is around 1992 to 1993

ggplot(data=year_rating, aes(x=year, y=avg_rating))+geom_line()
# the average rating for movie after 1978 are less than movies before 1978
ggplot(data=year_rating, aes(x=year, y=avg_rating))+
  geom_point()+geom_smooth()

#########5. Genres bias############


# movie ratings number by genre
genre_rating <- split_edx %>% group_by(genres) %>% 
  summarize(count = n()) %>% arrange(desc(count)) 
genre_rating

#calculate the average rating per genres
# movie average ratings by genre
genre_rating <- split_edx %>% group_by(genres) %>% 
  summarize(count = n(),avg_rating = mean(rating)) %>% arrange(desc(count)) 
genre_rating
ggplot(data=genre_rating, aes(x=genres, y=count))+geom_point()


##################year and genres###########
#Check the raing times for different genres and year
genres_popularity <- split_edx %>%
  select(movieId, year, genres) %>%
  mutate(genres = as.factor(genres)) %>%
  group_by(year, genres) %>%
  summarise(count=n())
head(genres_popularity$genres)
genres_popularity %>% filter(genres %in% c("Drama", "Comedy", "Thriller", "Romance","War","Sci-Fi", "Action")) %>%
  ggplot(aes(x=year, y=count))+
  geom_line(aes(color=genres))+
  scale_fill_brewer(palette="Paired")

genres_popularity
ggplot(data=year_rating, aes(x=count, y=avg_rating))+geom_line()


# Compare the most popular year of each type of movie
genres_popyear <- genres_popularity %>% group_by(genres) %>%
  filter(count==max(count))
genres_popyear
# We can see that most of genres of movie are popular around 1990 to 1999
#Documentary and IMAX start populat after 2000




#########################################
############Compare different model#######
#########################################
# Define the function of RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
rmse_compare <- data_frame()

###########1. Simple model########
### only include mu#########
#Calculate the average of mu
mu <- mean(edx$rating)
mu
simple_rmse <- RMSE(validation$rating, mu)
simple_rmse
rmse_compare <- tibble(method = "Just the average", RMSE = simple_rmse)
rmse_compare


#######2. Include the movie effects#########

# Calculate bi-represent average ranking for movie i
#bi is the average of Yu,i-mu for each movie i
movie_avgs <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom = "histogram", bins=20, data=., color=I("black"))
predicted_movie_rating <- mu + validation %>%
  left_join(movie_avgs, by = "movieId") %>%
  pull(b_i)
movie_rmse <- RMSE(validation$rating, predicted_movie_rating)
rmse_compare <- bind_rows(rmse_compare, tibble(method = "Movie effect model", RMSE = movie_rmse))
rmse_compare


#####################3. Movie and user effect model#############

# Different users are different in terms of how they rate movies
# Include the user bias b_u
# b_u can be calculated by Yu,i-mu-b_i

user_avgs <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
user_avgs %>% qplot(b_u, geom = "histogram", bins = 30, data=., color = I("black"))

predicted_user_movie_rating <- validation %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
user_movie_rmse <- RMSE(validation$rating, predicted_user_movie_rating)
rmse_compare <- bind_rows(rmse_compare, tibble(method = "Movie and user effect model", RMSE = user_movie_rmse))
rmse_compare


################Regularization based approach#########
######################################################
lambdas <- seq(0,10,0.25)
rmse <- sapply(lambdas, function(lam){
mu <- mean(edx$rating)

b_i<- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lam))


b_u <- edx %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i- mu)/(n()+lam))

predicated_rating <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
    pull(pred)


return(RMSE(validation$rating, predicated_rating))
})

qplot(lambdas, rmse)

lambda <- lambdas[which.min(rmse)]
lambda


############use the lambda############
#########compute the b_i#########
movie_avgs_reg <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())


######compute the b_u##########
user_avgs_reg <- edx %>%
  left_join(movie_avgs_reg, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda), n_u = n())

#############
predicted_rating_reg <- validation %>%
  left_join(movie_avgs_reg, by = "movieId") %>%
  left_join(user_avgs_reg, by = "userId") %>%
  mutate(pred = mu + b_i +b_u) %>%
  pull(pred)

reg_rmse <- RMSE(validation$rating, predicted_rating_reg)
rmse_compare <- bind_rows(rmse_compare, tibble(method = "Regularized Movie and user effect model", RMSE = reg_rmse))
rmse_compare


############Including movies, users, year and genres########
##b_y represent the year effect, it can be calculated by sum(rating-mu-b_i-b_u)/(n+lambda)
###b_g represent the genres effect, it can be calculated by sum(rating-mu-b_i-b_u-b_y)/(n+lambda)
########choose the lambda##########
lambdas <- seq(0,20,1)
rmse <- sapply(lambdas, function(lam){
  mu <- mean(edx$rating)
  
  b_i<- split_edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lam))
  
  
  b_u <- split_edx %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i- mu)/(n()+lam))
  
  
  b_y <- split_edx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+lam))
  

  b_g <- split_edx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "year") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - b_y - mu)/(n()+lam))  
    
  predicated_rating <- split_validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "year") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
    pull(pred)
  
  
  return(RMSE(split_validation$rating, predicated_rating)) 
})

qplot(lambdas, rmse)

lambda_yg <- lambdas[which.min(rmse)]

lambda_yg

############lambda is 14############

##################model##############

#########compute the b_i#########
movie_avgs_reg2 <- split_edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda_yg), n_i = n())


######compute the b_u##########
user_avgs_reg2 <- split_edx %>%
  left_join(movie_avgs_reg2, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda_yg), n_u = n())


######compute the b_y##########
year_avgs_reg2 <- split_edx %>%
  left_join(movie_avgs_reg2, by = "movieId") %>%
  left_join(user_avgs_reg2, by = "userId") %>%
  group_by(year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda_yg), n_y = n())

######compute the b_g##########
genres_avgs_reg2 <- split_edx %>%
  left_join(movie_avgs_reg2, by = "movieId") %>%
  left_join(user_avgs_reg2, by = "userId") %>%
  left_join(year_avgs_reg2, by = "year") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda_yg), n_g = n())

#############
predicted_rating_reg2 <- split_validation %>%
  left_join(movie_avgs_reg, by = "movieId") %>%
  left_join(user_avgs_reg, by = "userId") %>%
  left_join(year_avgs_reg2, by = "year") %>%
  left_join(genres_avgs_reg2, by = "genres") %>%
  mutate(pred = mu + b_i +b_u + b_y + b_g) %>%
  pull(pred)

reg_rmse2 <- RMSE(split_validation$rating, predicted_rating_reg2)
rmse_compare <- bind_rows(rmse_compare, tibble(method = "Regularized Movie,user, year and genres effect model", RMSE = reg_rmse2))
rmse_compare


