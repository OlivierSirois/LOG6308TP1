# Load libraries
library("readr")
library("dplyr")

# Open files
data <- read.csv("u.data", sep='|')
items <- read.csv("u.item.csv", sep='|')
users <- read.csv("u.user.csv", sep='|')

# Question 1
users_jobs <- users %>% select(id, job) %>% group_by(job)
ratings <- data %>% select(user.id, rating)

users_avg_ratings <- data %>% group_by(user.id) %>% summarize(avg_rating = mean(rating))

users_avg_ratings <- inner_join(users_avg_ratings, rename(users, user.id = id))

ratings_by_job <- users_avg_ratings %>% group_by(job) %>% summarize(avg_rating = mean(avg_rating))

ratings_by_job

ratings_by_age <- users_avg_ratings %>% group_by(age) %>% summarize(avg_rating = mean(avg_rating))

ratings_by_age

# Question 2
# ratings <- data %>% sparseMatrix(i = )
