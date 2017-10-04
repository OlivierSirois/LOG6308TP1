# Load libraries
library("readr")
library("dplyr")
library("Matrix")

# Open files
data <- read.csv("u.data", sep='|')
items <- read.csv("u.item.csv", sep='|')
users <- read.csv("u.user.csv", sep='|')



# ----------------------------- Question 2 *Fonctions* -------------------------------------
ratings <- sparseMatrix(i = data[,1], j = data[,2], x = data[,3])
test1 <- matrix(c(5,4,4,1,1,1,2,4,3,1,NA,3,2,3,2,2),4)
test2 <- matrix(c(5,4,4,1), 1,4)
#

predict.vote <- function(m, useravg, item, user.avg, cor){ # pour utilisateur-utilisateur
  m.mod <- m[,item]
  user.avg.mod <- user.avg
  cor.mod <- cor
  correction.fact <- 1/sum(abs(cor[-which(is.na(m.mod))]))
  v1 <- m.mod[-which(is.na(m.mod))] - user.avg.mod[-which(is.na(m.mod))]
  predicted.voted <- useravg + correction.fact*  sum(cor.mod[-which(is.na(m.mod))]*v1)
  return(abs(predicted.voted))
}
## Cosinus entre un vecteur v et chaque colonne dela matrice m
cosinus.vm <- function(v,m) {
# On on met tous nos valeurs de NA a 0, sinon on va avoir des problemes de calculs avec des matrices sparse
  m[is.na(m)] <- 0
  v[is.na(v)] <- 0 ;
# On calcule le cosinus entre le vecteur V et les colonnes de la matrice m en utilisant la formule vu en classe
  (v %*% m)/(sqrt(colSums(m^2)) * sqrt(sum(v^2))) 
}


# Trouve les indexes des premières 'n' valeurs maximales d'une matrice
max.nindex <- function(m, n=5) {
    i <- order(m, decreasing=TRUE)
    return(i[1:n])
}
# Meme chose, mais pour les valeurs minimales
min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}

#Correlation entre la rangee v (v = index) et chaque colonne de la matrice m
corr.vm <- function(v,m) {
  # on centre les valeurs de la matrice m en fonction de la moyenne, on la renomme m.centre
  v.i <- rowMeans(m[,], na.rm=T)
  # on enleve les NA
  m[is.na(m)] <- 0
  m.centre <- m - v.i
  # on centre le vecteur v en fonction de sa moyenne
  v.index <- v
  v.index[is.na(v)] <- 0
  v.index <- v.index - mean(v, na.rm=T)
  # on retourne ensuite un vecteur correspondant entre le vecteur v et sa correlation avec chaque rangers de m
  return( (v.index%*%t(m.centre))/(sqrt(sum(v.index^2) * rowSums(m.centre^2))))
}


#-----------------Question 6
scores <- function(gender, job, age){
  # calcule des probabilites de votes positifs sachant le sexe
gend <- gender
jb <- job
ag <- age
# Nous commencons notre algorithme en fonction de la theorie bayesiennes. On score chaque films selon les chances posterieur d'un rating positif (c.a.d, plus grand ou egal a 4).
# On assume pour cette algorithme une independance des facteurs. Nous allons alors scorer nos films selon le ratio de chances avec nos caracteristiques (notre age, sexe et profession)
# la forme general va avoir la forme ci-dessous

#           P(Vote Positif sachant : Age, Sexe, Profession) = P(Vote Positifs) * P(Age sachant: Vote Positif) * P(Sexe sachant : Vote Positif ) * P(profession sachant : Vote positif)
#           _______________________________________________   ________________________________________________________________________________________________________________________
#           P(Vote Negatif sachant : Age, Sexe, Profession)   P(Vote Negatifs) * P(Age sachant: Vote Negatif) * P(Sexe sachant : Vote Negatif ) * P(profession sachant : Vote Negatif)

# Nous allons calculer individuellement chaque probabiliter en fonction de notre dataset et ensuite appliquer cette fonction sur toutes nos films. Nous prendre les films avec les 
# meilleurs score et nous les recommenderons

# on commence par charger nos données de vote dans le dataframe ratings.sel
r.sel <- data %>% select(user.id, rating, item.id) %>% inner_join(rename(users, user.id = id))

# Nous allons premierement commencer par separer les donnes par films, nous allons compter le nombre de vote par combinaison de vote positif et negatif et par sexe.
# Cela va nous donner pour chaque films: le nombre d'homme qui ont voter positif, negatif et le nombre de femme qui on voter positif et negatif 
gender.who.voted <- r.sel %>% select(user.id, rating, item.id, gender) %>% group_by(item.id) %>% count(gender, rating > 3)

# nous allons ensuite extraire les totaux de votes positif et de negatif dans deux autre differentes matrix. On fait sa parce que nos probabilités de sexes sont fait en sachant que
# les votes sont positifs. La même chose se fait pour les votes negatifs
gender.who.voted.pos <- merge(gender.who.voted[gender.who.voted[,3] == "TRUE" & gender.who.voted[,2] == gend,],gender.who.voted[gender.who.voted[,3] == "TRUE" & gender.who.voted[,2] != gend,], by = "item.id")
gender.who.voted.neg <- merge(gender.who.voted[gender.who.voted[,3] == "FALSE" & gender.who.voted[,2] == gend,],gender.who.voted[gender.who.voted[,3] == "FALSE" & gender.who.voted[,2] != gend,], by = "item.id")

# Maintenant que nous avons bien séparés nos données. Nous allons calculer les probabilités de sexe sachant en divisant le nombre de gens qui ont le sexe désiré par le nombre total de
# gens qui ont voter. Le calculs se fera pour les votes positifs et negatifs, ce qui nous donnera P(Sexe sachant: Vote positif) et P(Sexe sachant: Vote negatif), qui sont Psv et Psvi
Psv <- gender.who.voted.pos %>% select(item.id, n.x, n.y) %>% group_by(item.id) %>% summarize(Psv = n.x/(n.x+n.y))
Psvi <- gender.who.voted.neg %>% select(item.id, n.x, n.y) %>% group_by(item.id) %>% summarize(Psvi = n.x/(n.x+n.y))

# Avec le même trains d'idées. Nous allons effectuer  le même calcul pour les autres caractéristiques. 
profession.who.voted <- r.sel %>% select(user.id, rating, item.id, job) %>% group_by(item.id) %>% count(job, rating > 3)
# Dans le cas des profession, nous devons faire une additions de différentes valeurs étant donnée qu'il y a plusieurs profession. On regroupe toutes les professions qui ne sont pas 
# la profession désiré comme étant une 'otherprofs'
otherprofs.pos <- profession.who.voted[profession.who.voted[,3] == "TRUE" & profession.who.voted[,2] != jb,] %>% select(item.id, n) %>% group_by(item.id) %>% summarize(not_prof = sum(n))
otherprofs.neg <- profession.who.voted[profession.who.voted[,3] == "FALSE" & profession.who.voted[,2] != jb,] %>% select(item.id, n) %>% group_by(item.id) %>% summarize(not_prof = sum(n))
# on merge les données pour qu'ils se retrouvent dans une autre matrices pour facilité les calculs
profession.who.voted.pos <- merge(profession.who.voted[profession.who.voted[,3] == "TRUE" & profession.who.voted[,2] == jb,], otherprofs.pos, by = "item.id")
profession.who.voted.neg <- merge(profession.who.voted[profession.who.voted[,3] == "FALSE" & profession.who.voted[,2] == jb,], otherprofs.neg, by = "item.id")

# le même calculs que pour le sexe, sauf que c'est avec la profession
Ppv <- profession.who.voted.pos %>% select(item.id, n, not_prof) %>% group_by(item.id) %>% summarize(Ppv = n/(n+not_prof))
Ppvi <- profession.who.voted.neg %>% select(item.id, n, not_prof) %>% group_by(item.id) %>% summarize(Ppvi = n/(n+not_prof))

# On reprend le calcul, mais cette fois c'est avec l'age. Pour avoir une bonne rétention de films (C-à-d, ne pas perdre de film dans nos calculs en raison de manque de données),
# considérer des ages ayant des écart de quatre ans (Age-2 < Age < Age+2) comme étant un age similaire
age.who.voted <- r.sel %>% select(user.id, rating, item.id, age) %>% group_by(item.id) %>% count(abs(age - ag) < 2, rating > 3)
# On sépare en fonction de ceux qui ont voter positif et négatifs
age.who.voted.pos <- merge(age.who.voted[age.who.voted[,3] == "TRUE" & age.who.voted[,2] == "TRUE",], age.who.voted[age.who.voted[,3] == "TRUE" & age.who.voted[,2] == "FALSE",], by = "item.id")
age.who.voted.neg <- merge(age.who.voted[age.who.voted[,3] == "FALSE" & age.who.voted[,2] == "TRUE",], age.who.voted[age.who.voted[,3] == "FALSE" & age.who.voted[,2] == "FALSE",], by = "item.id")

# On reprend le calcul pour calculer les probabilité d'age selon un vote positif ou negatif
Pav <- age.who.voted.pos %>% select(item.id, n.x, n.y) %>% group_by(item.id) %>% summarize(Pav = n.x/(n.x+n.y))
Pavi <- age.who.voted.neg %>% select(item.id, n.x, n.y) %>% group_by(item.id) %>% summarize(Pavi = n.x/(n.x+n.y))

# On fait la dernière série de calculs, on veut seulement calculer les chances d'avoir un vote positif ou negatif pour chaque films sans prendre en considération
# les charactéristique voulues

# On sépare nos votes selon un vote positif ou négatif
who.voted <- r.sel %>% select(user.id, rating, item.id) %>% group_by(item.id) %>% count(rating > 3)
who.voted.pos <- who.voted[who.voted[,2] == "TRUE",]
who.voted.neg <- who.voted[who.voted[,2] == "FALSE",]
# on fait un merge de nos deux possibilités dans une autre matrice pour facilité le calcul
who.voted <- merge(who.voted.pos, who.voted.neg, by = "item.id")
# on fait le calcul en prenant le nombre de vote positif en divisant le nombre de vote total, on effectue la meme operation pour le vote inverse en utilisant les votes negatifs
Pv <- who.voted %>% select(item.id, n.x, n.y) %>% group_by(item.id) %>% summarize (Pv = n.x/(n.x+n.y), Pvi = n.y/(n.x+n.y))

# Maintenant que nous avons toutes nos probabilités, on va tous les joindres dans une seules matrices pour facilité grandement les calculs
all.p <- Reduce(function(x, y) merge(x, y, by = "item.id"), list(Pv, Pav, Pavi, Ppv, Ppvi, Psv, Psvi))

# Nous allons ensuite prendre les probabilités les utiliser dans la fonction que nous avons décrit au début de la fonction. 
# Cette fonction sera prise pour noter chaque films. 
all.scores <- all.p %>% select(item.id, Pv, Pvi, Pav, Pavi, Ppv, Ppvi, Psv, Psvi) %>% group_by(item.id) %>% summarize(score = Pv*Pav*Psv*Ppv/(Pvi*Pavi*Psvi*Ppvi)) 

# Nous allons maintenant joindre les titres de films dans notre matrices pour que sa soit 'user-friendly', la trier en fonction des notes attribuer par notre système. 
all.scores <- items %>% select(movie.id, movie.title) %>% rename(item.id = movie.id) %>% merge(all.scores, by = "item.id") %>% arrange(desc(score))
# Notre travail est maintenant terminer. On retourne notre dataframe de score 
return(all.scores)
}

# Un test de notre algorithme pour un programmeur agé de 30 ans
##print('Question 6')
##print("voici les recommandations selon un algorithme bayesien pour un homme programmeur de 30 ans")
##print.data.frame(head(scores("M", "programmer", 30)))

#-------------------------------Question 5-------------------
userA <- matrix(rep(NA, ncol(ratings)), 1, ncol(ratings))
# Pour la Question 5, nous devons trouver des recommandations pour un utilisateur (userA) en utilisant une approche collaborative.
# Nous devons trouver dix films pour cette utilisateur en se basant sur les vingts personnes les plus près de lui.

# Pour ce problème. Nous allons le résoudre en corrélant tous les utilisateurs sur notre utilisateur A. Nous allons ensuite prendre les vingts 
# personnes ayant le plus grand coéfficient de corrélation et prendre toutes les films ayant été côté 5 étoiles. Nous allons ensuite utiliser toutes
# les films qui ont été côté 5 étoiles au moins une fois par les 20 utilisateurs choisi. Nous allons ensuite choisir nos recommandations basé sur la fréquences 
# de votes 5 étoiles pour nos vingts utilisateurs les plus proches. Les films ayant le plus d'occurence de vote 5 étoiles de nos 20 utilisateurs seront pris pour recommandation

# Nous allons commencer par initialiser notre utilisateur comme étant un vecteur. Nous allons manuellement inscrire les votes pour
# chaques films  noté (Star Wars = 1 étoiles et Star Trek = 5 étoiles)
userA[172] = 1
userA[181] = 1
userA[222] = 5
userA[227] = 5
userA[228] = 5
userA[229] = 5
userA[230] = 5
userA[380] = 5
userA[449] = 5
userA[450] = 5
# Nous allons aussi initialiser une vecteur contenant toute les indices de films visionné par notre utilisateur pour éviter de les recommander à nouveau
movies.rated.by.user <- c(172,181,222,227,228,229,230,380,449,450)
# On effectue une corrélation sur tout nos utilisateurs
users <- corr.vm(userA, ratings)
# On prend les indices des vingts utilisateurs les plus proche
closest.users <- max.nindex(users, n=20)
# On prends tous les ratings de ces 20 utilisateurs pour chaque films
closest.ratings <- ratings[closest.users,]
# On extrait tous les ratings 5 étoiles pour chaques utilisateurs en prenant les indices de films
closest.movies <- which(closest.ratings == 5, arr.ind = T)
# on prend seulement en compte les indices de films, étant donné que la colonne représente les utilisateur 1-20, qui n'a pas vraiment d'importance significative
closest.movies <- closest.movies[,2]
# on extrait toutes les occurences de films ayant déjà été visionné par l'utilisateur
closest.movies <-  closest.movies[! closest.movies %in% movies.rated.by.user]
# on calcules les fréquences de votes 5 étoiles pour chaques films. et on trie la table en fonction des films ayant le plus d'occurence de votes 5 étoiles
closest.movies <- sort(table(closest.movies), decreasing = T)
# notre travail est terminer. On imprime les films ayant été voté le plus souvent
##print("Question 5")
##print("voici les 10 films recommander avec le nombre d'occurence de ratings 5 * dans les 20 utilisateurs les plus proches")
##print(closest.movies[1:10], row.names = FALSE)
#----------------------------------------------------------------




#        QUESTION 3
# Pour cette question nous devons faire des recommandations baser sur une approche item-item. Pour faire cela nous calculons les 20 items les plus pres de l'item 
# désiré à l'aide de la distance euclidienne. Ensuite nous prenons c'est 20 items, calculons le poid avec le cosinus et nous appliquons la formule pour calculer le vote.
# pour toute les différents utilisateurs
predict <- function(ratings) {
  
  # Remplacement des zéros par NA
  m <- as.matrix(ratings)
  m[m==0] <- NA
  
  # Vecteur contenant les indices des utilisateurs n'ayant pas indiqué de vote
  # pour Star Trek
  
  users.no.vote.450 = which(m[,450] %in% NA)
  
  # Vecteur contenant les distances entre Star Trek et les autres films
  distance.450 <- sqrt(colSums(ratings[,450] - ratings)^2)
  
  
  # Calcul des 20 voisins les plus proches
  n.voisins <- 20 + 1
  votes.communs <- colSums((ratings[,450] * ratings) > 0) # nombre de votes communs
  #print(which(votes.communs==0))
  i.distance.450 <- min.nindex(distance.450, n.voisins)
  # votes.communs[i.distance.450]
  
  i.distance.450 <- i.distance.450[i.distance.450 != 450]
  # Moyenne des votes par film (sans les NA)
  i.mean.item <- matrix(colMeans(m[], na.rm=TRUE))
  i.450.mean <- mean(m[,450], na.rm=T)

  # on sort nos predictions en utilisant la formule apprise dans le cours
  res <- sapply(unique(1:943), function(x) predict.vote(t(m[,i.distance.450]), i.450.mean, x, rowMeans(t(m[,i.distance.450]), na.rm = T), cosinus.vm(m[,450], m[,i.distance.450])))

  return(res)
}

# ----------------------------- Question 4 -------------------------------------

# Principe : on retire temporairement de la matrice de votes, un utilisateur
# ayant voté pour Star Trek, on prédit ensuite son vote, et on se sert de la
# comparaison entre ces deux valeurs pour calculer la RMSE globale.

print("------------------- Question 4 -------------------")

# Cette fonction calcule le carré de la différence entre le vote prédit pour un
# utilisateur, et le vote réel
compute.square.difference <- function(u.id, ratings, prediction) {
  # On retire le vote de l'utilisateur étudié
  ratings.user.removed <- as.matrix(ratings)
  ratings.user.removed[u.id, 450] <- 0
  
  # On prédit (entre autres) le vote de cet utilisateur
  predictions <- predict(ratings.user.removed)
  
  # On calcule le carré de la différence entre la valeur prédite et la valeur
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
#predictions <- predict(ratings)
res <- sum(sapply(users.vote.450, function(x) compute.square.difference(x, ratings)), na.rm=T)

# On divise le résultat par le nombre de votants pour Star Trek, puis on prend la
# racine carrée de ce résultat, ce qui nous donne la RMSE
res <- sqrt(res / dim(matrix(users.vote.450))[1])

print(res)


#--------------QUESTION 2
# Pour la question deux, nous devons simplement trouver les films les plus proche du film 450 avec les mesures de correlation pearson et du cosinus.
# Nous avons au début du fichier deux fonction que nous avons implémenter qui calcule les corrélations entre un vecteur et toutes les colonnes d'une matrice.
# Nous allons d'abord commencer par appliquer ces fonctions sur notre vecteur de notre film 450 et tous nos ratings 
ratings_cos <- cosinus.vm(ratings[,450], ratings)
ratings_cor <- corr.vm(ratings[,450], t(ratings))

#Nous allons ensuite trouver les 10 corrélations les plus élevés
closest_movies_cor <- max.nindex(ratings_cor, n=10)
closest_movies_cos <- max.nindex(ratings_cos, n=10)

#Et puis finalement, nous allons les imprimer
##print("Question 2")
##print('voici les films les plus proches du film 450 selon la mesure du cosinus')
##print(closest_movies_cos)
##print('voici les films les plus proches du film 450 selon la mesure de la correlation pearson')
##print(closest_movies_cor)
# Nous remarquons que les index de films sont les mêmes. Nous avons vérifier qu'il n'y avait pas d'erreur. Étant donné que les notes de la corrélation et du cosinus
# ne sont pas égaux. Alors c'est une addonance que les films recommandés sont exact dans les deux cas.

# ----------------------------- Question 1 -------------------------------------
# Pour la première question du TP, notre objectif est de trouver les moyennes de vote selon les professions et l'age des utilisateurs.
# Nous allons re-charger notre tableau, étant donné que nous l'avons modifié précédemment (Question 6). Nous allons ensuite faire un join de notre tableau de ratings avec les
# infos des utilisateurs. Avec sa, nous allons facilement être en mesure de pouvoir faire des moyennes selon chaque caractéristique de nos utilisateurs

# Nous allons commencer par charger notre tableau
users <- read.csv("u.user.csv", sep='|')
users_jobs <- as.data.frame(users) %>% select(id, job) %>% group_by(job)
# On extrait nos ratings de nos données
ratings <- data %>% select(user.id, rating)
# On fait une moyenne de nos ratings pour chaque utilisateurs dans notre matrice de votes
users_avg_ratings <- data %>% group_by(user.id) %>% summarize(avg_rating = mean(rating))
# On fait un inner_join de notre matrice de vote avec la matrice des utilisateurs, pour pouvoir avoir l'information de chaque utilisateurs avec la moyenne de ceux-ci
users_avg_ratings <- inner_join(users_avg_ratings, rename(users, user.id = id))

# Ayant ça, on peut faire une moyenne des moyennes de film par utilisateurs avec tous les utilisateurs ayant un certain age ou profession

# On commence par faire une moyenne des moyennes de votes pour les utilisateurs par professions
ratings_by_job <- users_avg_ratings %>% group_by(job) %>% summarize(avg_rating = mean(avg_rating))
# On l'imprime
##print('Average rating by job')
##print(ratings_by_job)
# On effectue le même calcule avec un regroupement par age
##ratings_by_age <- users_avg_ratings %>% group_by(age) %>% summarize(avg_rating = mean(avg_rating))
# on l'imprime
##print('Average rating by Age')
##print(ratings_by_age)



