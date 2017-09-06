# Load libraries
library("readr")
library("dplyr")
library("Matrix")

# Open files
data <- read.csv("u.data", sep='|')
items <- read.csv("u.item.csv", sep='|')
users <- read.csv("u.user.csv", sep='|')

# ----------------------------- Question 1 -------------------------------------
# users_jobs <- users %>% select(id, job) %>% group_by(job)
# ratings <- data %>% select(user.id, rating)
#
# users_avg_ratings <- data %>% group_by(user.id) %>% summarize(avg_rating = mean(rating))
#
# users_avg_ratings <- inner_join(users_avg_ratings, rename(users, user.id = id))
#
# ratings_by_job <- users_avg_ratings %>% group_by(job) %>% summarize(avg_rating = mean(avg_rating))
#
# ratings_by_job
#
# ratings_by_age <- users_avg_ratings %>% group_by(age) %>% summarize(avg_rating = mean(avg_rating))
#
# ratings_by_age

# ----------------------------- Question 2 -------------------------------------
ratings <- sparseMatrix(i = data[,1], j = data[,2], x = data[,3])

## Cosinus entre un vecteur v et chaque colonne dela matrice m
cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) }

# Trouve les indexes des premières 'n' valeurs maximales d'une matrice
max.nindex <- function(m, n=5) {
    i <- order(m, decreasing=TRUE)
    return(i[1:n])
}

ratings_cos <- cosinus.vm(ratings[,450], ratings)

closest_movies <- max.nindex(ratings_cos)

closest_movies
