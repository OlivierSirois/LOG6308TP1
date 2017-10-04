# Load libraries
library("readr")
library("dplyr")
library("Matrix")

# Open files
data <- read.csv("u.data", sep='|')
items <- read.csv("u.item.csv", sep='|')
users <- read.csv("u.user.csv", sep='|')

# ----------------------------- Question 1 -------------------------------------

print("------------------- Question 1 -------------------")

users_jobs <- users %>% select(id, job) %>% group_by(job)
ratings <- data %>% select(user.id, rating)

# On regroupe les votes par utilisateur, puis on calcul la moyenne des votes
# par utilisateur
users_avg_ratings <- data %>% group_by(user.id) %>% summarize(avg_rating = mean(rating))

# Jointure pour avoir toutes les informations sur les utilisateurs, en plus de
# leur vote moyen
users_avg_ratings <- inner_join(users_avg_ratings, rename(users, user.id = id))

# On regroupe les votes moyens de chaque utilisateur par profession, puis on
# calcule le vote moyen par profession
ratings_by_job <- users_avg_ratings %>% group_by(job) %>% summarize(avg_rating = mean(avg_rating))

print(ratings_by_job)

# On regroupe les votes moyens de chaque utilisateur par âge, puis on calcule le
# vote moyen par âge
ratings_by_age <- users_avg_ratings %>% group_by(age) %>% summarize(avg_rating = mean(avg_rating))

print(ratings_by_age)

# ----------------------------- Question 2 -------------------------------------

print("------------------- Question 2 -------------------")

ratings <- sparseMatrix(i = data[,1], j = data[,2], x = data[,3])

## Cosinus entre un vecteur v et chaque colonne dela matrice m
cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) }

# Trouve les indexes des premières 'n' valeurs maximales d'une matrice
max.nindex <- function(m, n=10) {
    i <- order(m, decreasing=TRUE)
    return(i[1:n])
}

# On calcule la similarité (cosinus) entre Star Trek:The Final Frontier, et tous
# les autres films
ratings_cos <- cosinus.vm(ratings[,450], ratings)

# On sélectionne les 10 films les plus similaires
closest_movies <- max.nindex(ratings_cos, 11)
closest_movies <- closest_movies[closest_movies != 450]

print(closest_movies)

# ----------------------------- Question 3 -------------------------------------

print("------------------- Question 3 -------------------")

# Trouve les indexes des premières 'n' valeurs minimales d'une matrice
min.nindex <- function(m, n=5) {
    i <- order(m)
    return(i[1:n])
}

# Cosinus entre un vecteur v et chaque colonne dela matrice m
cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2, na.rm=TRUE)); (v %*% m)/(n * sqrt(sum(v^2))) }

# Fonction d'estimation de vote
predict <- function(ratings) {
    # Remplacement des zéros par NA
    m <- as.matrix(ratings)
    m[m==0] <- NA

    # Vecteur contenant les indices des utilisateurs n'ayant pas indiqué de vote
    # pour Star Trek
    users.no.vote.450 <- which(m[,450] %in% NA)

    # Vecteur contenant les distances entre Star Trek et les autres films
    distance.450 <- sqrt(colSums(ratings[,450] - ratings)^2)

    # Calcul des 20 voisins les plus proches
    n.voisins <- 20 + 1
    votes.communs <- colSums((ratings[,450] * ratings) > 0) # nombre de votes communs
    i.distance.450 <- min.nindex(distance.450, n.voisins)
    i.distance.450 <- i.distance.450[i.distance.450 != 450]

    # Moyenne des votes par film (sans les NA)
    i.mean.item <- matrix(colMeans(m[], na.rm=TRUE))

    # Calcul de la similarité (cosinus) entre Star Trek et les autres films
    similarite <- cosinus.vm(ratings[,450], ratings[])

    # Sélection des votes des utilisateurs n'ayant pas voté pour Star Trek,
    # pour chaque voisin de ce dernier
    no.vote.users.closest <- m[users.no.vote.450, i.distance.450]

    # Passage sous forme matricielle des moyennes de votes de chaque voisin de
    # Star Trek
    dim.no.vote <- dim(no.vote.users.closest)
    closest.means <- t(matrix(rep(i.mean.item[i.distance.450], dim.no.vote[1]), ncol=dim.no.vote[1]))

    # Passage sous forme matricielle de la similarité entre Star Trek et les
    # autres films
    sim.closest <- t(matrix(rep(similarite[i.distance.450], dim.no.vote[1]), ncol=dim.no.vote[1]))

    # Estimation des votes suivant la formule du cours sur les filtres
    # collaboratifs, page 19, transposée pour une approche item-item
    num <- matrix(rowSums(sim.closest * (no.vote.users.closest - closest.means), na.rm=T))
    denom <- matrix(sum(abs(similarite[i.distance.450])), nrow=dim.no.vote[1], ncol=1)
    res <- matrix(i.mean.item[450], nrow=dim.no.vote[1], ncol=1) + num / denom

    return(res)
}

ratings <- sparseMatrix(i = data[,1], j = data[,2], x = data[,3])
rownames(ratings) <- paste('u', 1:nrow(ratings), sep='')
colnames(ratings) <- paste('i', 1:ncol(ratings), sep='')

print(predict(ratings))

# ----------------------------- Question 4 -------------------------------------

# Principe : on retire temporairement de la matrice de votes, un utilisateur
# ayant voté pour Star Trek, on prédit ensuite son vote, et on se sert de la
# comparaison entre ces deux valeurs pour calculer la RMSE globale.

print("------------------- Question 4 -------------------")

# Cette fonction calcule le carré de la différence entre le vote prédit pour un
# utilisateur, et le vote réel
compute.square.difference <- function(u.id, ratings) {
    # On retire le vote de l'utilisateur étudié
    ratings.user.removed <- as.matrix(ratings)
    ratings.user.removed[u.id, 450] <- 0

    # On prédit (entre autres) le vote de cet utilisateur
    predictions <- predict(ratings.user.removed)

    # On calcule le carré de la différence entre la valeur prédite et la valeurs
    # réelle
    res <- (predictions[u.id] - ratings[u.id, 450]) ** 2

    return (res)
}

ratings <- sparseMatrix(i = data[,1], j = data[,2], x = data[,3])
rownames(ratings) <- paste('u', 1:nrow(ratings), sep='')
colnames(ratings) <- paste('i', 1:ncol(ratings), sep='')
m <- as.matrix(ratings)
m[m==0] <- NA

# On sélectionne tous les utilisateurs ayant donnée un vote à Star Trek
users.vote.450 <- which(! m[,450] %in% NA)

# Pour chacun de ces utilisateurs, on calcule le caré de la différence entre son
# vote prédit et son vote réel. On somme tous ces résultats
res <- sum(sapply(users.vote.450, function(x) compute.square.difference(x, ratings)), na.rm=T)

# On divise le résultat par le nombre de votants pour Star Trek, puis on prend la
# racine carrée de ce résultat, ce qui nous donne la RMSE
res <- sqrt(res / dim(matrix(users.vote.450))[1])

print(res)
