
#############################################
##1. Chargement des Librairies et des Données
#############################################


# librairie utiliser 

library(ggplot2)
library(gridExtra)  # Pour organiser plusieurs graphiques
library(stringr)    # gérer les chaines de caractéres
library("RColorBrewer")  # Fourni une variéte de couleur pour les graphs
library(MASS) 
library(car)   # test et graphiques pour regression lineaire

library(cluster)  # pour utiliser des oprerations de clustering (CAH)
library(factoextra) # Proposes des graphiques pour visuliaser le nombres de clusters optimal 
library(fpc)      # Fourni des mesures pour évaluer les clusters



# chargement des données
data= read.csv("C:\\Users\\soso3\\Desktop\\Projet Licence\\DataScientist.csv\\DataScientist.csv", header=TRUE, sep=",",na.strings="-1")                           


## Compréhension des Données

str(data)  # affichage de la structure interne de l'objet




######################################################################################################################
######################################################################################################################
############################################ #####  Etape 1: Prétraitement 1  #######################################
######################################################################################################################
######################################################################################################################


## Transformation des Variables Chaînes de Caractères 


########################################################################################################
# 1. "Salary.Estimate": Supprimer les lignes qui ne contiennent pas d'information sur le salaire annuel.
########################################################################################################

## à quoi ressemble ces lignes 
cat( str_subset(data$ Salary.Estimate,"Per Hour") )                

# Nous indique sous forme de booleen si une ligne contient "Per Hour" ou  pas
pattern <- str_detect(data$ Salary.Estimate,"Per Hour")    

# Suppression de ces lignes dans le dataframe data. On a conservé tous les TRUE en faisant la négation de pattern
data=data[!pattern,]                                       


cat("verification des lignes supprimer: ", nrow(data))   # initialement 3909


#########################################################################
#  "Salary.Estimate": Éliminer la mention "(Glassdoor est.) et $ et k".
#########################################################################

data$Salary.Estimate <- gsub("\\(Glassdoor est.\\)|\\$|K", "", data$Salary.Estimate)

# verification que "(Glassdoor est.)" et "$" et "k" sont bien supprimer 
head(data$Salary.Estimate,3)


#####################################################################################
# "Salary.Estimate"Créer de nouvelles variables "Salaire_Min" et "Salaire_Max"
###############################################################################


# Divise la colonne 'Salary.Estimate' du dataframe 'data' en deux parties (min et max)
r=str_split(data$Salary.Estimate, "-", simplify = TRUE)

# Affiche les trois premières lignes de la matrice 'r' pour un échantillon des données
head(r,3)

# permet d'extraire que les valeurs numérique de 0 à 9 de la premiere colonne de 'r'
min=as.numeric(str_extract(r[,1], "[0-9]+"))      
max=as.numeric(str_extract(r[,2], "[0-9]+"))

cat("On verifie les premières valeurs de la variable 'min': ", head(min), "\n" )
cat("On verifie les premières valeurs de la variable 'min' :", head(max) )


data$estima_min=min        # nouvelle variable min dans le data.frame data
data$estima_max=max        # nouvelle variable max dans le data.frame data


# Cela crée une nouvelle colonne "Salaire_Aggrégé" contenant la moyenne de "Salaire_Min" et "Salaire_Max" pour chaque observation.
data$Salaire_Aggrégé <- rowMeans(data[c("estima_min", "estima_max")], na.rm = TRUE)



###########################################################
# 2. Diviser la variable "Location" en "Ville" et "Région"
#########################################################

 # délimateur par une virgule
b <- str_split(data$Location, ",", simplify = TRUE)

# Crée la variable "lieu_ville" à partir de la première colonne de la matrice b
data$lieu_ville <- b[, 1]

# Crée la variable "lieu_region" à partir de la deuxième colonne de la matrice b
data$lieu_region <- b[, 2]



########################## 
# la variable Company.Name
##########################
 
#  Supprimer la partie "\n..." de la variable "Company.Name" en utilisant gsub
data$Company.Name <- gsub("[\n][0-9]+[.][0-9]+", "", data$Company.Name)



########################## 
# la variable Easy.Apply
##########################

# Remplacer "NA" par "Unkn" et "True" par "Easy" dans la variable "Easy.Apply" en utilisant str_replace_all
data$Easy.Apply <- str_replace_all(data$Easy.Apply, c( "True" = "Easy"))



# Vérifier les dimensions actuelles du dataframe "data" (nombre de lignes et de colonnes)

cat(names(data), "nom des colonnes", "\n")
cat(nrow(data), "nbres de lignes")



###################################
## Élimination des Colonnes inutiles
####################################

data[ c("X","index" ,"Salary.Estimate", "Job.Description", "Company.Name", "Location", "Headquarters", "Competitors", "Founded", "Industry", 
         "estima_min", "estima_max","lieu_ville" ) ]= NULL

cat("dimension du nouveau data: ", dim(data), "\n\n" )

colnames(data)


##################################################
## Transformation des Variables Catégorielles
################################################


# Sélectionner les noms des colonnes contenant des données de type caractère (chaînes de caractères)
chr_column_names <- colnames(data)[sapply(data, is.character)]


# Conversion des colonnes caractère en facteurs
for (var in chr_column_names) {
  data[[var]] <- as.factor(data[[var]])
}

# Définir l'ordre des niveaux pour les variables Size et Revenue
data$Size <- factor(data$Size, levels = c("1 to 50 employees", "51 to 200 employees", "201 to 500 employees",
                                          "501 to 1000 employees", "1001 to 5000 employees", "5001 to 10000 employees",
                                          "10000+ employees", "Unknown"), ordered = TRUE)

data$Revenue <- factor(data$Revenue, levels = c("Less than $1 million (USD)", "$1 to $5 million (USD)",
                                                "$5 to $10 million (USD)", "$10 to $25 million (USD)",
                                                "$25 to $50 million (USD)", "$50 to $100 million (USD)",
                                                "$100 to $500 million (USD)", "$500 million to $1 billion (USD)",
                                                "$1 to $2 billion (USD)", "$2 to $5 billion (USD)",
                                                "$5 to $10 billion (USD)", "$10+ billion (USD)",
                                                "Unknown / Non-Applicable"), ordered = TRUE)


# On change les noms des poste

colnames(data)= c("nom_poste", "eval_entrep", "taille_entrep", "Type_propriete", "secteur", "revenue_entrep","Easy.Apply",  "Salaire_Aggrégé", "lieu_region" )

# On verifie qu'on a bien changé les classes en facteur et changé les nom des postes
str(data)



#########################
## Nettoyage des Données
##########################


## Gestion des données manquantes
  
# Remplacement des valeurs -1 par NA dans la variable "Rating"
data[data[, 2] == -1, 2] = NA

# Nombre de lignes complètes sans NA
complete_cases_count <- sum(complete.cases(data))
cat("Nombre de lignes complètes sans données manquantes : ", complete_cases_count, "\n\n")

# Nombre de lignes contenant des NA
na_cases_count <- sum(!complete.cases(data))
cat("Nombre de lignes contenant des données manquantes : ", na_cases_count, "\n\n")

# Proportion des données complètes dans le jeu de données (en pourcentage)
complete_cases_percent <- (complete_cases_count / nrow(data)) * 100
cat("Proportion des données complètes dans le jeu de données : ", complete_cases_percent, "%\n\n")

# Proportion des données manquantes dans le jeu de données (en pourcentage)
na_cases_percent <- (na_cases_count / nrow(data)) * 100
cat("Proportion des données manquantes dans le jeu de données : ", na_cases_percent, "%\n\n")

# Nombre de valeurs manquantes pour chaque variable
apply(data, MARGIN = 2, function(x) sum(is.na(x)))


library(VIM)
summary(aggr(data, sortVar = TRUE))


######################################
## Suppression des Valeurs Manquantes
#####################################


# Suppression de la variable "Easy.Apply"
data$Easy.Apply <- NULL

# on supprime les valeurs manquantes en fonction de la variable secteur et de eval_entrep
data <- subset(data, !is.na(data$secteur))    
data <- subset(data, !is.na(data$eval_entrep)) 

## verification du nombre de valeurs manquantes (NA) dans chaque colonne du dataframe
sapply(data,function(x) sum(is.na(x)))

## nouvelle structure du data
str(data)


################################################
## Identifications et Suppressions des doublons
################################################

doublonstest<-which(duplicated(data))   ##  affichent les lignes admettant des doublons.
print(doublonstest) 

# suppression des doublons
data= data[-doublonstest, ]



##################################################################### 
################### Préparation des Données #########################
##################################################################### 

#  L'objectif ici est de préparer nos données pour la modélisation en posant des questions sur chaque variable du jeu de données. 
#  Étant donné que la plupart de nos variables sont qualitatives, nous avons élaboré une fonction appelée plot_meilleurs_variable.
#  Cette fonction génère des graphiques en camembert (pie charts) pour montrer les catégories les plus fréquentes des variables qualitatives.


plot_meilleurs_variable <- function(data, variable_quali, nbre) {
  # Cette fonction crée un pie chart pour visualiser les effectifs des catégories d'une variable qualitative.

# Identifier les effectifs de chaque catégorie de la variable qualitative
resume <- aggregate(data[[variable_quali]], by = list(data[[variable_quali]]), FUN = length)
colnames(resume) <- c(variable_quali, "nombre")

# Trier les catégories par nombre d'effectifs décroissant
resume <- resume[order(resume$nombre, decreasing = TRUE), ]

# Extraire les "nbre" catégories les plus fréquentes
top <- head(resume, nbre)

# Créer un pie chart avec les "nbre" catégories les plus fréquentes et leurs effectifs
labels_with_space <- paste(top[[variable_quali]], "\n Nombre : ", top$nombre)
title <- paste("Catégories les plus fréquentes de", variable_quali)
pie(top$nombre, labels = labels_with_space, col = brewer.pal(n = nbre, name = "RdBu"), main = title)
}


# 1.1- Combien contient de modalités la variable nom_poste?
  
nlevels(data$nom_poste)


# 1.2- Quelles sont les 6 emplois les plus demandéés?
  
plot_meilleurs_variable(data, "nom_poste", 6)



# 1.bis- Quelles sont les 8 emplois les mieux évalués

# Identifier les emplois les mieux évalués
emplois_mieux_evalues <- aggregate(data$eval_entrep, by = list(data$nom_poste), FUN = mean)
colnames(emplois_mieux_evalues) <- c("nom_poste", "eval_moyenne")
emplois_mieux_evalues <- emplois_mieux_evalues[order(emplois_mieux_evalues$eval_moyenne, decreasing = TRUE), ]

# Extraire les 10 emplois les mieux évalués
top_10_mieux_evalues <- head(emplois_mieux_evalues, 8)

# Créer un pie chart avec les 10 emplois les mieux évalués et leurs moyennes
labels_with_space <- paste(top_10_mieux_evalues$nom_poste, "\n Moyenne : ", round(top_10_mieux_evalues$eval_moyenne, 2))
pie(top_10_mieux_evalues$eval_moyenne, labels = labels_with_space, col = brewer.pal(n = 10, name = "RdBu"), main = "Emplois les mieux évalués")



# 2.1- Combien contient de modalités la variable secteurs d'activité?

nlevels(data$secteur)

# 2.2- Quelles sont les 6 secteurs d'activités les plus rechercher?
  
plot_meilleurs_variable(data, "secteur", 6)


# 3.1- Combien contient de modalités la variable revenue_entrep?
  
nlevels(data$revenue_entrep)


# 3.2- Quelles sont les 5 plus gros revenue des entreprises?
  
plot_meilleurs_variable(data, "revenue_entrep", 5)


# 4.1- Combien contient de modalités la variable lieu_region?
  
nlevels(data$lieu_region)


# 4.2- Quelles sont les 6 régions qui contient le plus d'offre d'emplois ?
  
plot_meilleurs_variable(data, "lieu_region", 6)



# 5.1- Combien contient de modalité la variable Type_propriete?
  
nlevels(data$Type_propriete)

# 5.2- Quelle type de propriété offre le plus d'emplois?

plot_meilleurs_variable(data, "Type_propriete", 4)


# 6.1- Combien contient de modalités la variable taille_entrep?

nlevels(data$taille_entrep)

# 6.2- Quelles sont les tailles d'entreprise qui recrutent le plus?

plot_meilleurs_variable(data, "taille_entrep", 6)



#####################################
## Gestion des Variables Qualitatives
#####################################

 
# Pour résoudre ces problèmes, nous envisageons d'utiliser deux approches principales :
  
# 1. Agrégation Manuelle :
# 2. Classification Ascendante Hiérarchique (CAH) :



## Voici les étapes de gestion des variables qualitatives :


## 1 Fonction de Table de Fréquence pour Variable Qualitative qui permet d'identifier des problèmes potentiels, 
##   tels que des modalités rares, un grand nombre de modalités ou des différences de fréquence significatives.


generate_frequency_table <- function(data, qualitative_variable) {
  
  # table() pour compter les occurrences des modalités
  table_result <- table(data[[qualitative_variable]])
  
  # prop.table() pour obtenir les fréquences relatives
  freq_relatives <- round(prop.table(table_result), 3)
  
  # Créer un dataframe avec les effectifs et les fréquences relatives
  result_dataframe <- data.frame(
    Modalite = names(table_result),
    Effectif = as.vector(table_result),
    Frequence = as.vector(freq_relatives)
  )
  
  # Trier le dataframe par fréquences relatives décroissantes
  result_dataframe <- result_dataframe[order(result_dataframe$Frequence, decreasing = TRUE), ]
  
  return(result_dataframe)
}




# 2 Fonction de Nettoyage de Variable Qualitative : Si la fonction `generate_frequency_table` révèle des problèmes, nous utilisons 
# la fonction `nettoyer_variable` pour nettoyer la variable. Cela comprend la suppression de niveaux indésirables et des valeurs manquantes.


nettoyer_variable <- function(data, variable, niveaux_a_supprimer = NULL) {
  # Supprimer les niveaux indésirables
  if (!is.null(niveaux_a_supprimer)) {
    data[[variable]] <- droplevels(data[[variable]], exclude = niveaux_a_supprimer)
  }
  
  # Supprimer les lignes avec des valeurs manquantes dans la variable
  data <- data[!is.na(data[[variable]]), ]
  
  return(data)
}


## 3. Pour une agrégation automatique nous allons faire un CAH.


# Fonction pour calculer la matrice de dissimilarité basée sur l'indice de Dice

calculate_dice_similarity_matrix <- function(data, variable_name) {
  # Étape 1 : Sélectionne la variable catégorielle à partir des données
  variable <- data[[variable_name]]
  
  # Étape 2 : Transforme la variable en encodage one-hot
  encoded_data <- model.matrix(~ variable - 1, data = data)
  
  # Étape 3 : Calcule la matrice de dissimilarité (indice de Dice)
  # on crée une matrice vide de la même taille que les données encodées
  similarity_matrix <- matrix(0, ncol(encoded_data), ncol(encoded_data))
  
  # Nom des lignes et colonnes de la matrice avec les noms des variables
  rownames(similarity_matrix) <- colnames(similarity_matrix) <- colnames(encoded_data)
  
  # on parcour toutes les paires de variables encodées
  for (i in 1:(ncol(encoded_data) - 1)) {
    for (j in (i + 1):ncol(encoded_data)) {
      # Calculez l'indice de Dice entre les deux variables
      similarity_matrix[i, j] <- similarity_matrix[j, i] <-
        0.5 * sum((encoded_data[, i] - encoded_data[, j])^2)
    }
  }
  
  # Étape 4 : Transforme la matrice de similarité en matrice de distance
  distance_matrix <- as.dist(sqrt(similarity_matrix))  # Ajout de la parenthèse autour de sqrt
  
  
  # Étape 5 : Renvoie la matrice de distance
  return(distance_matrix)
}




# Fonction choix du nombre de cluster
  

# Fonction pour afficher le graphique d'inertie
afficher_graphique_inertie <- function(data, variable) {
  distance_matrix <- calculate_dice_similarity_matrix(data, variable)
  arbre.moda <- hclust(distance_matrix, method = "average")
  
  # Sous-graphique 1 : Inertie
  inertie <- sort(arbre.moda$height, decreasing = TRUE)
  plot(inertie[1:length(inertie)], type = "s", xlab = "Nombre de classes", ylab = "Inertie", main = "Inertie")
}



# Fonction pour organiser les graphiques
# Fonction pour organiser les graphiques
organiser_graphiques <- function(data, variable) {
  distance_matrix <- calculate_dice_similarity_matrix(data, variable)
  matrice_sim <- as.matrix(distance_matrix)
  
  # le nombre de niveaux de la variable spécifiée
  k_max <- nlevels(data[[variable]]) - 1
  
  # Sous-graphique 2 : Méthode Silhouette
  optimal_clusters_silhouette <- fviz_nbclust(matrice_sim, FUNcluster = hcut, method = "silhouette", hc_method = "average",
                                              k.max = k_max)
  plot2 <- optimal_clusters_silhouette + labs(subtitle = "Silhouette method")
  
  # Sous-graphique 3 : Méthode Elbow
  optimal_clusters_elbow <- fviz_nbclust(matrice_sim, FUNcluster = hcut, method = "wss", hc_method = "average",
                                         k.max = k_max)
  plot3 <- optimal_clusters_elbow + geom_vline(xintercept = 4, linetype = 2) + labs(subtitle = "Elbow method")
  
  # Sous-graphique 4 : Méthode Gap Statistic
  optimal_clusters_gap_stat <- fviz_nbclust(matrice_sim, FUNcluster = hcut, method = "gap_stat", nboot = 600, 
                                            hc_method = "average", k.max = k_max)
  plot4 <- optimal_clusters_gap_stat + labs(subtitle = "Gap statistic method")
  
  # les sous-graphiques en une seule disposition
  grid.arrange(plot2, plot3, plot4, ncol = 1)
}



## Validation interne
 
generer_resultats_clustering <- function(data, variable, nbre_choix) {
  # Calcul la matrice de similarité
  distance_matrix <- calculate_dice_similarity_matrix(data, variable)
  
  # Créer une liste vide pour stocker les résultats
  result_list <- list()
  
  # Créer une deuxième liste vide pour stocker uniquement les stats avec des noms
  stats_list <- list()
  
  # Liste pour stocker les attributs de clustering
  cluster_list <- list()  
  
  
  for (ap in nbre_choix) {
    # Créer une partition en clusters de test
    arbre.moda <- hclust(distance_matrix, method = "average")
    clustering <- cutree(arbre.moda, k = ap)
    
    # les statistiques de validation de clustering
    stats <- cluster.stats(distance_matrix, clustering)
    
    # Créez une sous-liste avec des noms
    sub_list <- list(nbre_choix = ap, stats = stats, clustering = clustering)  # Inclure les attributs de clustering
    
    # Ajout la sous-liste aux listes respectives
    result_list[[length(result_list) + 1]] <- sub_list
    stats_list[[length(stats_list) + 1]] <- sub_list$stats
    cluster_list[[length(cluster_list) + 1]] <- sub_list$clustering
  }
  
  # Créez une matrice vide pour stocker les résultats
  elements <- c( "average.within", "dunn", "dunn2", "entropy")
  results_matrix <- matrix(nrow = length(elements), ncol = length(stats_list))
  
  # Définir les noms des lignes de la matrice
  rownames(results_matrix) <- elements
  
  # Définir les noms de colonnes en utilisant les valeurs de nbre_choix avec "cluster_"
  colnames(results_matrix) <- paste("cluster_", as.character(nbre_choix), sep = " ")
  
  for (j in 1:length(stats_list)) {
    stats <- stats_list[[j]]  # Obtenez les statistiques de la liste
    for (i in 1:length(elements)) {
      result <- stats[[elements[i]]]
      results_matrix[i, j] <- result
    }
  }
  
  return(list(result_list = result_list, stats_list = stats_list, cluster_list = cluster_list, results_matrix = results_matrix))
}




## Mettre en application

################################
### Pour la variable "lieu_region" 
###################################

frequency_table <- generate_frequency_table(data, "lieu_region")
print(frequency_table)


# Utilisation de la fonction pour nettoyer la variable taille_entrep
data <- nettoyer_variable(data, "lieu_region", niveaux_a_supprimer = " United Kingdom")


# Verification 
cat( "le nombre de region maintenant :", nlevels(data$lieu_region) )    # 3345 obs initial
cat( "le nombre de ligne maintenant :", nrow(data) )


# Changez les modalités 
levels(data$lieu_region)[levels(data$lieu_region) %in% c(" AZ", " TX")] <- "sud_Ouest"
levels(data$lieu_region)[levels(data$lieu_region) %in% c(" CA")]       <- "cote_Ouest"
levels(data$lieu_region)[levels(data$lieu_region) %in% c(" DE", " FL", " NJ"," NY", " PA")] <- "cote_est"
levels(data$lieu_region)[levels(data$lieu_region) %in% c(" IL", " OH")] <- "Sud_Ouest"


# Vérifiez les niveaux mis à jour
cat("On verifie les nouvelles modalites :")
cat(levels(data$lieu_region))


cat("dimension du data :", dim(data) )



###############################
### Pour la variable "taille_entrep"
#################################

frequency_table <- generate_frequency_table(data, "taille_entrep")
print(frequency_table)

# Utilisation de la fonction pour nettoyer la variable taille_entrep
data <- nettoyer_variable(data, "taille_entrep", niveaux_a_supprimer = "Unknown")


# Verification 
cat( "le nombre de taille_entrep maintenant :", nlevels(data$taille_entrep) )    # on passe de 8 modalité à 7 
cat( "le nombre de ligne maintenant :", nrow(data) )                             # 3341 obs apres lieu_region



# Utilisation de la première fonction pour afficher le graphique d'inertie
afficher_graphique_inertie(data, "taille_entrep")

# Utilisation de la deuxième fonction pour organiser les graphiques
organiser_graphiques(data, "taille_entrep")

# Utilisation de la fonction pour générer les résultats
nbre_choix <- c(2,4)
resultats <- generer_resultats_clustering(data, "taille_entrep", nbre_choix)

# Afficher la matrice finale
print(resultats$results_matrix)
print(resultats$cluster_list)


# Changez les modalités 
levels(data$taille_entrep)[levels(data$taille_entrep) %in% c("1 to 50 employees" , "51 to 200 employees")] <- "petite"
levels(data$taille_entrep)[levels(data$taille_entrep) %in% c("201 to 500 employees", "501 to 1000 employees" )] <- "moyenne"
levels(data$taille_entrep)[levels(data$taille_entrep) %in% c("1001 to 5000 employees", "5001 to 10000 employees")] <- "grande"
levels(data$taille_entrep)[levels(data$taille_entrep) %in% c("10000+ employees")] <- "tres_grande"

# Vérifiez les niveaux mis à jour
print(levels(data$taille_entrep))



######################################  
## Pour la variable "Type_propriete"
####################################

frequency_table <- generate_frequency_table(data, "Type_propriete")
print(frequency_table)


# Utilisation de la première fonction pour afficher le graphique d'inertie
afficher_graphique_inertie(data, "Type_propriete")

# Utilisation de la deuxième fonction pour organiser les graphiques
organiser_graphiques(data, "Type_propriete")


# Utilisation de la fonction pour générer les résultats
nbre_choix <- c(2,3,4,10)
resultats <- generer_resultats_clustering(data, "Type_propriete", nbre_choix)

# Afficher la matrice finale
print(resultats$results_matrix)
print(resultats$cluster_list)


# Changez les modalités 
levels(data$Type_propriete)[levels(data$Type_propriete) %in% "Company - Private" ] <- "privee"
levels(data$Type_propriete)[levels(data$Type_propriete) %in% "Company - Public" ] <- "publique"

# restes des variables
levels(data$Type_propriete)[c(1, 4:14)] <- "autres"

# Vérifiez les niveaux mis à jour
print(levels(data$Type_propriete))



######################################
###Pour la variable "revenue_entrep"
##################################

frequency_table <- generate_frequency_table(data, "revenue_entrep")
print(frequency_table)

# Changez les modalités 
levels(data$revenue_entrep)[levels(data$revenue_entrep) %in% "Unknown / Non-Applicable"] <- "inconnue"
levels(data$revenue_entrep)[levels(data$revenue_entrep) %in% "$10+ billion (USD)"] <- "+10B"

levels(data$revenue_entrep)[c(8:11)] <- "500M à 10B"
levels(data$revenue_entrep)[c(6:7)] <- "50M à 500M"
levels(data$revenue_entrep)[c(1:5)] <- "-1M à 50M"

# Vérifiez les niveaux mis à jour
print(levels(data$revenue_entrep))



##############################
### Pour la variable "secteur 
##############################


frequency_table <- generate_frequency_table(data, "secteur")
print(frequency_table)

# Changez les modalités

levels(data$secteur)[levels(data$secteur) %in% "Information Technology"] <- "technologie_infor"
levels(data$secteur)[levels(data$secteur) %in% "Business Services"] <- "services"
levels(data$secteur)[levels(data$secteur) %in% c("Biotech & Pharmaceuticals", "Health Care")] <- "Sciences_Santé"
levels(data$secteur)[levels(data$secteur) %in% c("Finance", "Insurance" )] <- "finance-assurance"


# Liste des catégories à conserver
conserver <- c("Sciences_Santé" , "finance-assurance" , "services" , "technologie_infor")

# Récupérez les niveaux de la variable depuis votre dataframe (je suppose que vous avez déjà 'data' défini ailleurs).
nom_moda <- levels(data$secteur)


# Obtenez les indices des catégories qui ne sont pas dans 'conserver'
indices_non_conserver <- which(!nom_moda %in% conserver)

# Changez les modalités
levels(data$secteur)[indices_non_conserver]= "autres"


cat("les nouvelles modalites:", levels(data$secteur) )



###################################
### Pour la variable "nom_poste
##################################

frequency_table <- generate_frequency_table(data, "nom_poste")
print( head(frequency_table,10) )

# Convertir le facteur en caractère
data$nom_poste <- as.character(data$nom_poste)

# Remplacer les modalités contenant "analyst" par "data_analyst"
data$nom_poste  <- gsub(".*analyst.*", "data_analyst", data$nom_poste , ignore.case = TRUE)

# Remplacer les modalités contenant "scientist" par "data_scientist"
data$nom_poste  <- gsub(".*scientist.*", "data_scientist", data$nom_poste , ignore.case = TRUE)

# Remplacer les modalités contenant "Engineer" par "data_engineer"
data$nom_poste  <- gsub(".*engineer.*", "data_engineer", data$nom_poste , ignore.case = TRUE)


# Convertir la variable 'nom_poste' en facteur
data$nom_poste <- as.factor(data$nom_poste)

# afficher le nombre de niveaux (modalités) de la variable 'nom_poste'
cat("le nombre de modalités apres modification : ", nlevels(data$nom_poste), "\n\n")


# Obtenir la liste des modalités de 'nom_poste'
nom_moda = levels(data$nom_poste)

# Créer un vecteur logique pour les modalités à conserver
modalité_garder <- nom_moda == "data_scientist" | nom_moda == "data_analyst" | nom_moda == "data_engineer"

# Remplacer les modalités qui ne sont pas à conserver par "autres"
levels(data$nom_poste)[!modalité_garder] <- "autres"

# verifier

cat("Verification finale :",levels(data$nom_poste))





##########################################################################################################################################
#########################################################################################################################################
#############################################  Etape 2: Analyse exploratoire des données ################################################
#########################################################################################################################################
#########################################################################################################################################


####################################################
### Analyse Univariée pour les variables qualitatives
####################################################

# nouveau dataframe gardant que les variables qualitatives
data_quali= data[,c("nom_poste", "taille_entrep", "Type_propriete",  "secteur", "revenue_entrep", "lieu_region")]

#  graphiques en barres horizontales des 6 variables 
par(mfrow = c(2, 3))

for(i in 1:6) {
eff= table(data_quali[,i])
frequence= round(prop.table(eff)*100,2)
barplot( frequence, main=names(data_quali)[i], horiz=T, col=brewer.pal( n = nlevels(data_quali[,i]) , name = "Spectral") ) 

              }


#############################################################################
#### Analyse Bivariée et Tests Statistiques pour Deux Variables Qualitatives** 
##############################################################################


# Fonction pour effectuer le test du khi2 et calculer le coefficient de Cramer
test_chi2 <- function(data) {
  # Créer un data frame pour stocker les résultats
  resultats <- data.frame(Variable1 = character(0), Variable2 = character(0), Statistique_de_test = numeric(0),
                          p_valeur = numeric(0), ddl = numeric(0), Effectifs_attendus_sup_5 = logical(0),
                          p_valeur_fisher = numeric(0), V_de_Cramer = numeric(0))

  # On sélectionne les colonnes contenant des variables qualitatives
  qual_cols <- names(data)[sapply(data, is.factor)]

  # Itérer sur toutes les paires de variables qualitatives
  for (i in 1:(length(qual_cols) - 1)) {
    for (j in (i + 1):length(qual_cols)) {
      var1 <- qual_cols[i]
      var2 <- qual_cols[j]

      # On effectue le test du khi2
      contingency_table <- table(data[[var1]], data[[var2]])
      chi2_result <- chisq.test(contingency_table)

      # Calcule le coefficient de V de Cramer
      n <- sum(contingency_table)
      v_cramer <- sqrt(chi2_result$statistic / (n * (min(dim(contingency_table)) - 1)))
      
      #  le test de Fisher
      fisher_result <- fisher.test(contingency_table, simulate.p.value = TRUE)

      # Stocker les résultats dans le data frame
      resultats <- rbind(resultats, data.frame(Variable1 = var1, Variable2 = var2,
                                               Statistique_de_test = round(chi2_result$statistic, 2),
                                               p_valeur_khi2 = round(chi2_result$p.value, 3),
                                               ddl = chi2_result$parameter,
                                               Effectifs_attendus_sup_5 = all(chi2_result$expected > 5),
                                               p_valeur_fisher = round(fisher_result$p.value, 3),
                                               V_de_Cramer = round(v_cramer, 3)))
    }
  }

  return(resultats)
}

# Utilisation de la fonction
resultats_khi2 <- test_chi2(data_quali)  # Assurez-vous de remplacer "data_quali" par le nom de votre jeu de données



# Question 1.1: concernant les variables "types de Postes", "Régions"**

## Analyse des Distributions Conditionnelles des types de Postes en fonction des régions :** 

# - Quel est le type de poste le plus fréquent dans chaque région ? 

# - Y a-t-il des variations significatives dans la répartition des types de postes entre les régions ? 

# - Existe-t-il des régions où un certain type de poste prédomine par rapport aux autres ? 
 

##Comparaison des Catégories de Postes entre les Régions :** 

# - La répartition de certains postes varie-t-elle considérablement d'une région à l'autre ? 

# - Quels types de postes sont plus fréquents dans les régions cote_Ouest par rapport aux régions cote_Est ? 

# - Existe-t-il des différences significatives dans la répartition des postes data analyste entre les régions ? 


# Question 1.2:  Le type de poste recherché est-il indépendant de la région ?** 



# Création d'un graphique à barres empilées avec ggplot2
bar <- ggplot(data, aes(x = lieu_region, fill = nom_poste)) +
  geom_bar(position = "dodge") +  
  labs(title = "Diagramme en tuyaux d'orgue du type de poste en fonction de la region",  # Titre du graphique
       x = "Région",  # Légende de l'axe des x
y = "Nombre de postes")  # Légende de l'axe des y

# Afficher le graphique
print(bar)



# question 2:  

print( subset(resultats_khi2, Variable1=="nom_poste" & Variable2=="lieu_region") )





  
# Question 2.1: Concernant les variables Secteurs d'Activités et Régions**
# 2.2- Le type de industrie cherché est il indépendant de la région ?**
  



# Création d'un graphique à barres empilées avec ggplot2
bar <- ggplot(data, aes(x = lieu_region, fill = secteur)) +
  geom_bar(position = "dodge") +  
  labs(title = "Diagramme en tuyaux d'orgue secteur d'activité en fonction de la region",  # Titre du graphique
       x = "Région",  # Légende de l'axe des x
       y = "Secteur")  # Légende de l'axe des y

# Afficher le graphique
print(bar)



# question 2:  

print( subset(resultats_khi2, Variable1=="secteur" & Variable2=="lieu_region") ) 



###########################################################
#### **Analyse Univariée pour les variables quantitatives**
###########################################################

# on garde que les variables quantitatives
data_quanti= data[,c("Salaire_Aggrégé","eval_entrep")]

# graphiques histo et densité des variables quanti
par(mfrow=c(2,2))
for (i in 1:length(data_quanti)) {
  hist(data_quanti[,i], main=names(data_quanti[i]) )
  plot(density(data_quanti[,i]), main=names(data_quanti)[i])
  }


# statistique de base
summary(data_quanti)

# calcul les écarts types 
sd_salaire= sd(data_quanti$Salaire_Aggrégé)
sd_eval= sd(data_quanti$eval_entrep)

# comparaison relatives
relative_comparaison = sd_eval/sd_salaire

# Calcul du coefficient de variation
cv_sal= sd_salaire/mean(data_quanti$Salaire_Aggrégé)
cv_eval= sd_eval/mean(data_quanti$eval_entrep)


# affichage des resultats
print(paste("Ecart type salaire",sd_salaire) )
print(paste("Ecart type eval", sd_eval))
print(paste("CV sal", cv_sal))
print(paste("CV eval", cv_eval))
print(paste("comapraison_relative",relative_comparaison))



###################################################################################
#### ** Analyse Bivariée et Tests Statistiques pour deux Variables quantitatives**
##################################################################################


# Test de Corrélation de Pearson
cor_pear= cor.test(data_quanti$eval_entrep, data_quanti$Salaire_Aggrégé, method = "pearson")

# Test de Corrélation de Spearman
cor_spear= cor.test(data_quanti$eval_entrep, data_quanti$Salaire_Aggrégé, method = "spearman")

# Test de Corrélation de Kendall
cor_ken= cor.test(data_quanti$eval_entrep, data_quanti$Salaire_Aggrégé, method = "kendall")

#print("l'intensité est:", cor_pear)
print(cor_pear)
print(cor_spear)
print(cor_ken)




#################################################################################################################################
##################################################################################################################################
##################################################### Etape 3: Modélisation statistique #########################################
#################################################################################################################################
###################################################################################################################################



########################################
###  Évaluation Visuelle de la Linéarité 
########################################


# permet de faire hypothèse que la forme de la relation entre les  variables est linéaire

scatterplot( Salaire_Aggrégé~eval_entrep, data=data_quanti)  

# la ligne en trait plein est la droite de régression linéaire (définie par la méthode des moindres carrés) entre les deux variables
# la ligne centrale en pointillé est la courbe de régression locale de type lowess. Elle indique la tendance globale entre les deux variables.
# Les deux lignes extérieures représentent un intervalle de confiance de la courbe lowess.



#######################################################
### Réalisation d'un modéle régression lineaire simple**
#######################################################


# Pour déterminer la droite de régression, on ajuste un modèle linéaire simple aux données, à l'aide de la fonction "lm"
reg= lm(Salaire_Aggrégé ~ eval_entrep, data= data_quanti)   



#############################
## Analyse géneral des résidus
################################


# les résidus studentisés des oberservations
plot(rstudent(reg), pch = ".",ylab = "Résidus studentisés par VC")
abline(h = c(-2,2))
lines(lowess(rstudent(reg)))


## compter le nombre observation qui est mal expliquée par le modèle 
e = rstandard(reg)
# e[abs(e) > 2]
length(e[abs(e) > 2])/nrow(data) *100     # 3.061



###################################################################
#### **Vérification des Points Aberrants, Levier et Influents :** 
################################################################


par(mfrow=c(1,2))

# points de levier

p <- length(reg$coef)
n <- nrow(data)

infl.reg <- influence.measures(reg)
plot(infl.reg$infmat[,"hat"],type="h",ylab="hii", main="Point levier")
seuil1 <- 3*p/n ; abline(h=seuil1,col=1,lty=2)
seuil2 <- 2*p/n ; abline(h=seuil2,col=1,lty=3)

seuil1
seuil2

length(which(infl.reg$infmat[,"hat"]>seuil1))
length(which(infl.reg$infmat[,"hat"]>seuil2))


# points influents

plot(cooks.distance(reg),type="h",ylab="Distance de Cook", main="distance de cook")
seuil1 <- qf(0.1,p,n-p) ; abline(h=seuil1)


qf(0.1,p,n-p) ##  Une distance de Cook en-dessous de fp,n???p(0.1) est considéré comme souhaitable

qf(0.5,p,n-p) ##   seuil critique pour la distance de Cook à partir duquel on considère que l'observation est trop influente est le                    quantile fp,n???p(0.5)



#########################################################################################################
#################################### **Vérification des hypothéses**  ###################################
#########################################################################################################



########################################################
### **1- Hypohthése Adequation (Linearité) et Normalité**
########################################################


par(mfrow=c(1,2))

# Graphique de l'hypohthése Adequation (Linearité) 

plot( data_quanti$eval_entrep , residuals(reg), pch=16, xlab="xi", ylab="residus", main="variable explicatives vs résidus", cex.main=1)
lines(lowess(rstudent(reg)), col="red" )

# alternatif plot(reg,1)

## on peut aussi vérifier de la maniere suivantes. Dans une régression avec constante la moyenne des résidus doit etre nulle.
print(" on verifie que la moyenne est nulle", mean(residuals(reg)))




# Graphique de l'hypohthése de normalité 

plot(reg,2)


### tests statistiques

# Test d'adequation de Rainbow 
library(lmtest)
print( raintest(reg) ) 


library(nortest) 
# Test d'Anderson-Darling 
print( ad.test(e) )



#########################################
#### **2- Hypothèse d'Homoscédasticité **
#########################################  
  

par(mfrow=c(1,2))

prediction= fitted(reg)
plot( prediction, residuals(reg), pch=16, xlab="prediction", ylab="residus", main="Graph predictions vs résidus", cex.main=1)
lines(lowess(rstudent(reg)), col="red" )

# Ce graphique à pour meme but que le premier mais on utilise des résidus standardisé pour les mettres à la meme échelle.
plot(reg,3)

# test Breusch-Pagan
library(car)
print( ncvTest(reg) )



#######################################  
### **3- Independance des residus**
#####################################  

  
par(mfrow=c(1,3))

plot(rstudent(reg),  pch=20, col="blue", type="b", xlab="i", ylab="ei", main="Graph des résidus")
abline(h=0, col="red", lwd=2)

acf(residuals(reg), main="Graph autocorrélation") 
pacf(residuals(reg), main="Graph autocorrélation partielle")


# test de Durbin-Watson
library(lmtest)
print(durbinWatsonTest (reg) )



#######################
## Résumer du modéle lm
#######################

summary(reg)





  