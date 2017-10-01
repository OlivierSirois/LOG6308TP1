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
# ratings <- sparseMatrix(i = data[,1], j = data[,2], x = data[,3])
#
# ## Cosinus entre un vecteur v et chaque colonne dela matrice m
# cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) }
#
# # Trouve les indexes des premières 'n' valeurs maximales d'une matrice
# max.nindex <- function(m, n=10) {
#     i <- order(m, decreasing=TRUE)
#     return(i[1:n])
# }
#
# ratings_cos <- cosinus.vm(ratings[,450], ratings)
#
# closest_movies <- max.nindex(ratings_cos)
#
# closest_movies

# ----------------------------- Question 3 -------------------------------------
ratings <- sparseMatrix(i = data[,1], j = data[,2], x = data[,3])
rownames(ratings) <- paste('u', 1:nrow(ratings), sep='')
colnames(ratings) <- paste('i', 1:ncol(ratings), sep='')
m <- as.matrix(ratings)
m[m==0] <- NA

users.no.vote.450 = which(m[,450] %in% NA)
# users.no.vote.450

distance.450 <- sqrt(colSums(ratings[,450] - ratings)^2)

min.nindex <- function(m, n=5) {
    i <- order(m)
    return(i[1:n])
}

# Calcul des 20 voisins les plus proches
n.voisins <- 20 + 1
votes.communs <- colSums((ratings[,450] * ratings) > 0) # nombre de votes communs
#print(votes.communs)
i.distance.450 <- min.nindex(distance.450, n.voisins)
print(votes.communs[i.distance.450])
# votes.communs[i.distance.450]
i.distance.450 <- i.distance.450[i.distance.450 != 450]
# mean
i.mean.item <- matrix(colMeans(m[], na.rm=TRUE))
# i.mean.item

## Cosinus entre un vecteur v et chaque colonne dela matrice m
cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2, na.rm=TRUE)); (v %*% m)/(n * sqrt(sum(v^2))) }

similarite <- cosinus.vm(ratings[,450], ratings[])

no.vote.users.closest <- m[users.no.vote.450, i.distance.450]
#print(no.vote.users.closest)
dim.no.vote <- dim(no.vote.users.closest)
closest.means <- t(matrix(rep(i.mean.item[i.distance.450], dim.no.vote[1]), ncol=dim.no.vote[1]))

sim.closest <- t(matrix(rep(similarite[i.distance.450], dim.no.vote[1]), ncol=dim.no.vote[1]))
print(sim.closest)
num <- matrix(rowSums(sim.closest * (no.vote.users.closest - closest.means), na.rm=TRUE))
denom <- matrix(sum(abs(similarite[i.distance.450])), nrow=dim.no.vote[1], ncol=1)

res <- matrix(i.mean.item[450], nrow=dim.no.vote[1], ncol=1) + num / denom
#print(res)
