##################################################################################
# 1 - Chargement des librairies 
##################################################################################
library(data.table)
# gestion des dates
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")};library(lubridate)
# Functions to Accompany J. Fox and S. Weisberg, An R Companion to Applied Regression, Third Edition, Sage, 2019.
if("car" %in% rownames(installed.packages()) == FALSE) {install.packages("car")};library(car)
# Several functions are available for calculating the most widely used effect sizes (ES),
# along with their variances, confidence intervals and p-values
if("compute.es" %in% rownames(installed.packages()) == FALSE) {install.packages("compute.es")};library(compute.es)
# Graphical and tabular effect displays, e.g., of interactions, for various statistical models with linear predictors.
if("effects" %in% rownames(installed.packages()) == FALSE) {install.packages("effects")};library(effects)
# Create Elegant Data Visualisations Using the Grammar of Graphics
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")};library(ggplot2)
# Simultaneous Inference in General Parametric Models
if("multcomp" %in% rownames(installed.packages()) == FALSE) {install.packages("multcomp")};library(multcomp)
# Package for Analysis of Space-Time Ecological Series
if("pastecs" %in% rownames(installed.packages()) == FALSE) {install.packages("pastecs")};library(pastecs)
# A Collection of Robust Statistical Methods
if("WRS2" %in% rownames(installed.packages()) == FALSE) {install.packages("WRS2")};library(WRS2)
# set of packages that work in harmony because they share common data representations and 'API' design.
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)
# Regression subset selection, including exhaustive search.
if("leaps" %in% rownames(installed.packages()) == FALSE) {install.packages("leaps")};library(leaps)

##################################################################################
# 2 - Import des données 
##################################################################################

# (5144 matchs)
data <- read.csv("data/data.csv", sep = ",")

# Vérification des types de champs et des valeurs nulles
summary(data)
str(data)

# conversion du champ date en type date
data$date<-as.Date(data$date)

# Retrait des matchs antérieur à l'an 2000 (4895 matchs)
data<-data[year(data$date)>2000,]

##################################################################################
# 3 - Création d'une table de combattant unique
##################################################################################

# Etape 1 : faire un dataframe avec toutes les variables concernant le joueur bleu + Date + Gagnant + Catégorie de poids + Nombre de round
# Liste de tous les noms de colonnes
nom_colonne<-colnames(data)
# Liste des noms de colonnes des combattants rouges
col_rouge<-nom_colonne[nom_colonne %like% "^R_"]
# Liste des noms de colonnes des combattants bleus
col_bleu<-nom_colonne[nom_colonne %like% "^B_"]

# Liste des matchs avec que les champs des combattants rouges (4895 matchs)
data_rouge<-data[ ,c("date","Winner","title_bout","weight_class","no_of_rounds", col_rouge) ]
colnames(data_rouge)<-c("date","Winner","title_bout","weight_class","no_of_rounds", substr(col_rouge, 3, 1000) )

# Liste des matchs avec que les champs des combattants bleus (4895 matchs)
data_bleu<-data[ ,c("date","Winner","title_bout","weight_class","no_of_rounds", col_bleu) ]
colnames(data_bleu)<-c("date","Winner","title_bout","weight_class","no_of_rounds", substr(col_bleu, 3, 1000) )

# Liste des matchs en double avec info des combattant bleus et rouges (9790 lignes)
base_joueur_match<-rbind(data_rouge,data_bleu)

# liste des dates de match la plus récente par combattant (1742 combattants)
date_max<-aggregate(date ~ fighter , data = base_joueur_match , max )

# liste des combattants avec leur date de dernier match (1742 combattants)
joueur_unique <- merge(base_joueur_match, date_max, 
                       by.x = c("date","fighter") ,by.y = c("date","fighter"),
                       all.x = F , all.y = F)

# Recherche de doublons ?
#doublon<-data.frame(table(joueur_unique$fighter))
#doublon[doublon$Freq>0,]

# Etape 2 : faire un dataframe avec toutes les variables concernant le joueur rouge + Date + Gagnant + Catégorie de poids + Nombre de round
# Etape 3 : Renommer les variables pour que les noms de colonnes soit identiques entre les deux dataframe créés ci-dessus 
# Etape 4 : Concatener les deux dataframes en un seul
# Etape 5 : Sélectionner seulement la ligne correspondant au dernier combat par combattant

#######################################################################################################################################
# 3 - Calculer la régression linéaire simple entre le poids et la taille 
# QUANTITATIF/QUANTITATIF
#######################################################################################################################################

# The database is attached to the R search path.
# This means that the database is searched by R when evaluating a variable,
# so objects in the database can be accessed by simply giving their names.
#attach(joueur_unique)

# Liste des couples tailles/poids (de 1739 combattants) après suppression des valeurs nulles sur Weight_lbs et Height_cms
data_taille_poids <- na.omit(joueur_unique[,c("Weight_lbs","Height_cms")])
summary(data_taille_poids)

# A - Analyse graphique
# nuage de points du rapport taille/poids (de 1739 combattants)
plot(Weight_lbs, Height_cms, data = data_taille_poids)

# Contrôle des valeurs de taille (de 1739 combattants)
table(data_taille_poids$Height_cms)

# B - Construction du modèle de régression linéaire simple
# dans le but de prédire le poids en fonction de la taille
str(data_taille_poids)
reg.lm <- lm( Weight_lbs~Height_cms, data = data_taille_poids)
?lm
names(reg.lm)
# R² ajusté de 60%
summary(reg.lm)

# droite de régression & nuage de points du rapport taille/poids (de 1739 combattants)
plot(Weight_lbs~Height_cms, data = data_taille_poids, pch=16)
abline(reg.lm, col="red", lwd=2)

# B1 - Estimation des paramètres (méthode des moindres carrés)
# B2 - Test global du modèle (test F) / tests de nullité des coefficients
# B3 - Qualité du modèle (coefficient R²)


# https://theeasierproject.wordpress.com/les-tests-dhypothese-nulle-principes-essentiels-des-statistiques/
# https://bookdown.org/ndphillips/YaRrr/linear-regression-with-lm.html
# Un test statistique permet d’estimer la probabilité de croiser une personnes aussi intelligente ou plus intelligente qu’Albert Einstein
# en considérant que ce niveau intellectuel ne se distingue pas de celui de la population générale.
# La probabilité associée à un test statistique pour les tests d’hypothèses nulles est donc la probabilité d’une absence de différence.
# Test de student
# probabilités d’une absence de différence pour les 1739 poids
rstud = rstudent(reg.lm)
# valeurs
ecarts<-cbind(data_taille_poids, reg.lm$fitted.values, reg.lm$residuals, reg.lm$effects, rstud)
head(ecarts)
# Comparaison des valeurs d'origines et des valeurs prédites de la variable Y dépendante (weight_lbs)
data_taille_poids$weight_value.lm <- reg.lm$fitted.values
head(data_taille_poids)

# C - Vérification des hypothèses
# C1 - Valeurs ajustées / résidus studentisés (indépendance, structure de variance, points aberrants)
plot(rstud, pch=20, ylab="Résidus studentisés", ylim=c(-3,3))
abline(h=c(0), col="grey", lty=1, lwd=2)
abline(h=c(-2,2), col="grey", lty=2, lwd=2)
# Rapport entre taille échantillon avec probabilité -1.95 > p > 1.95 (119) et taille échantillon total (1739)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents sur le modèle de régression linéaire)
reg.cook=cooks.distance(model=reg.lm)
plot(reg.cook, type="h", ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5, col="gray", lty=2)

# C3 - Droite de Henry (normalité)
reg.qq=qqnorm(rstud, pch=20, ylim=c(-3,7), xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

# D - Prédiction
# Quelle serait la taille d'une personne pesant 135 lbs ?
hist(data_taille_poids$Height_cms)
# Classes des tailles
sequences_taille <- data.frame(Height_cms = seq(160, 200, 5))
# prédictions de valeurs par classe de taille en se basant sur le modèle
predictions <- cbind(sequences_taille, predict(reg.lm, sequences_taille, interval="prediction"))
plot(predictions$Height_cms~predictions$fit)

#######################################################################################################################################
# 4- Calculer la régression linéaire multiple entre le ratio de victoire et 
# Le nombre de coups à la tête / le nombre de coups au corps / le nombre de coups au sol
#
# QUANTITATIF/QUANTITATIF+QUANTITATIF+QUANTITATIF...
#######################################################################################################################################

# A - Analyse graphique
# Ajout d'une variable ratio_victoire (nb_victoire / total nb_victoire+nb_defaite)
joueur_unique$ratio_victoire <- joueur_unique$wins/(joueur_unique$wins + joueur_unique$losses)
# histogramme ratio_victoire : beaucoup de valeurs à 0 qui peuvent déséquilibrer le modèle à définir
hist(joueur_unique$ratio_victoire)

# du coup On garde seulement les combattants de poids mi-moyen avec plus de 3 matchs
base_reg<-joueur_unique[((joueur_unique$wins + joueur_unique$losses)>3) & joueur_unique$weight_class == "Welterweight", c("fighter","weight_class","ratio_victoire","avg_BODY_att","avg_HEAD_att","avg_GROUND_att") ]
hist(base_reg$ratio_victoire)
table(joueur_unique$wins + joueur_unique$losses)
summary(base_reg)
str(base_reg)

# Analyse graphique
plot(base_reg)
# on regarde les covariances, corrélation de pearson par défaut
# méthode moins spécifique que la régression linéaire simple, puisqu’elle n’évalue pas la linéarité mais seulement la monotonie.
# entre -1 et 1
# Lorsque le coefficient de corrélation est positif, cela signifie que les deux variables varient dans le mêmes sens.
# Au contraire, lorsque le coefficient est négatif cela signifie que les variables varient en sens opposé.
# Plus la valeur absolue du coefficient est proche de 1, plus la relation monotone entre les deux variables est forte,
# c’est à dire plus la relation monotone entre les deux variables est parfaite.
# méthode non paramétrique, qui est basée sur les rangs des observations ne permet pas de représentation graphique
cor(base_reg$ratio_victoire, base_reg$avg_BODY_att)
cor(base_reg$ratio_victoire, base_reg$avg_HEAD_att)
cor(base_reg$ratio_victoire, base_reg$avg_GROUND_att)
cor(base_reg$avg_GROUND_att, base_reg$avg_HEAD_att)

# B  - Construction du modèle 
# B1 - Estimation des paramètres (méthode des moindres carrés)
# B2 - Test global du modèle (test F) / tests de nullité des coefficients
# B3 - Qualité du modèle (coefficient R²)
reg.lm <- lm(ratio_victoire~avg_GROUND_att+avg_HEAD_att+avg_BODY_att, data= base_reg)
summary(reg.lm)

plot(ratio_victoire~avg_GROUND_att+avg_HEAD_att+avg_BODY_att, data = base_reg, pch=16)
abline(reg.lm, col="red", lwd=2)

# C - Vérification des hypothèses
# C1 - Valeurs ajustées / résidus studentisés (indépendance, structure de variance, points aberrants)
rstud = rstudent(reg.lm)
plot(rstud, pch=20, ylab="Résidus studentisés", ylim=c(-3,3))
abline(h=c(0), col="grey", lty=1, lwd=2)
abline(h=c(-2,2), col="grey", lty=2, lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
reg.cook=cooks.distance(model=reg.lm)
plot(reg.cook, type="h", ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray", lty=2)

# C3 - Droite de Henry (normalité)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7), xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

#######################################################################################################################################
# ANOVA à 1 facteur entre nombre de coup tenté et catégorie de poids
#
# QUANTITATIF/QUALITATIF
#######################################################################################################################################

# Analyse de la variable weight_class
data.frame(table(joueur_unique$weight_class))
# moyenne de coups portés par classe de poids
aggregate(avg_TOTAL_STR_att~weight_class, data = joueur_unique, mean)
# histogramme du nombre de coups porté (assez loin d'une loi normale)
hist(joueur_unique$avg_TOTAL_STR_att)
# boîte à moustache du nombre de coups porté par classe de poids
boxplot(formula=avg_TOTAL_STR_att~weight_class, data=joueur_unique, boxwex=0.3, col="lightblue", pch=10, xlab="Catégorie de poids")

# un Premier modèle sans recodage des modalités (R² 8%)
mod.lm=lm(formula=avg_TOTAL_STR_att~weight_class, data=joueur_unique)
anova(mod.lm)
summary(mod.lm)

# cette fois On garde seulement les combattants avec plus de 3 matchs et le nombre de coups total port renseigné
# avec le nombre de coups portés aussi à la tête en complément
data_anova <- joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) &!is.na(joueur_unique$avg_TOTAL_STR_att), c("fighter","avg_TOTAL_STR_att","weight_class","avg_HEAD_att")]

# Regroupement des classes de poids dans 6 nouvelles classes
# poid_plume_homme, poid_leger_homme, poid_moyen_homme, poid_lourd_homme, poid_plumme_femme, poid_moyen_femme
data_anova[which(data_anova$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_anova[which(data_anova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"
# histogramme du nombre de coups porté : cette fois plus près d'une loi normale
hist(data_anova$avg_TOTAL_STR_att)

# Analyse graphique : boît à moustache avec nb coups portés par classe de poids révisée (les 6 nouvelles)
boxplot(formula=avg_TOTAL_STR_att~categorie_poids2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Catégorie de poids")

# meilleur modèle (R² 14%)
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2, data=data_anova)
anova(mod.lm)
summary(mod.lm)

# C - Vérification des hypothèses
# C1 - Valeurs ajustées / résidus studentisés (indépendance, structure de variance, points aberrants)
rstud = rstudent(mod.lm)
plot(rstud, pch=20, ylab="Résidus studentisés", ylim=c(-3,3))
abline(h=c(0), col="grey", lty=1, lwd=2)
abline(h=c(-2,2), col="grey", lty=2, lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
reg.cook=cooks.distance(model=mod.lm)
plot(reg.cook, type="h", ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5, col="gray", lty=2)

# C3 - Droite de Henry (normalité)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7), xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

#######################################################################################################################################
# ANOVA à deux facteurs entre nombre de coup tenté et catégorie de poids + Stance
#
# QUANTITATIF/MIXTE QUANTITATIF & QUALITATIF
#######################################################################################################################################

# Analyse stance
table(joueur_unique$Stance)
aggregate(avg_TOTAL_STR_att~Stance, data = joueur_unique, mean)
boxplot(formula=avg_TOTAL_STR_att~Stance, data=joueur_unique, boxwex=0.3, col="lightblue", pch=10, xlab="Catégorie de poids")

# Analyse weight_class
aggregate(avg_TOTAL_STR_att~weight_class, data = joueur_unique, mean)
hist(joueur_unique$avg_TOTAL_STR_att)
boxplot(formula=avg_TOTAL_STR_att~weight_class, data=joueur_unique, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")

# cette fois On garde seulement les combattants avec plus de 3 matchs et le nombre de coups total port renseigné
# avec le nombre de coups portés aussi à la tête et le style de combat en complément
data_anova<-joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) &!is.na(joueur_unique$avg_TOTAL_STR_att),c("fighter","avg_TOTAL_STR_att","weight_class","avg_HEAD_att","Stance")]
# Regroupement des classes de poids dans 6 nouvelles classes
# poid_plume_homme, poid_leger_homme, poid_moyen_homme, poid_lourd_homme, poid_plumme_femme, poid_moyen_femme
data_anova[which(data_anova$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_anova[which(data_anova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"
# Regroupement des styles de combat dans 3 nouvelles classes
# autres, Orthodox, Southpaw
data_anova[which(data_anova$Stance %in% c("Open Stance","Sideways","Switch","")),"Stance2"]<-"autres"
data_anova[which(data_anova$Stance %in% c("Orthodox")),"Stance2"]<-"Orthodox"
data_anova[which(data_anova$Stance %in% c("Southpaw")),"Stance2"]<-"Southpaw"

# Analyse stance reclassifiée
table(data_anova$Stance2)
aggregate(avg_TOTAL_STR_att~Stance2, data = data_anova, mean)
boxplot(formula=avg_TOTAL_STR_att~Stance2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Catégorie de poids")

# Analyse graphique
boxplot(formula=avg_TOTAL_STR_att~categorie_poids2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Par catégorie de poids", ylab="Nombre de coups portés")
boxplot(formula=avg_TOTAL_STR_att~Stance, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Par style de combat", ylab="Nombre de coups portés")

# lm à besoin de types "factor" vs chr
str(data_anova)
data_anova$categorie_poids2<-as.factor(data_anova$categorie_poids2)
#levels(data_anova$categorie_poids2)<-c("")
levels.default(data_anova$categorie_poids2)

# Via lm
# Modèle sans interaction
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2+Stance2, data=data_anova)
summary(mod.lm)
anova(mod.lm)

# Modèle avec interaction
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2*Stance2, data=data_anova)
summary(mod.lm)
anova(mod.lm)

# Modèle seulement avec interaction 
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2:Stance2, data=data_anova)
summary(mod.lm)
anova(mod.lm)

model1 <- aov(avg_TOTAL_STR_att~categorie_poids2+altitude+canopy+height)

############################################################################
# Comparaison ancova VS reg linéaire en fonction des catégorie de poids 
############################################################################

data_ancova<-na.omit(joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) &!is.na(joueur_unique$avg_TOTAL_STR_att),c("fighter","age","ratio_victoire","avg_TOTAL_STR_att","weight_class","avg_HEAD_att","Stance")])
data_ancova[which(data_ancova$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_ancova[which(data_ancova$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_ancova[which(data_ancova$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_ancova[which(data_ancova$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_ancova[which(data_ancova$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_ancova[which(data_ancova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"

data_ancova[which(data_ancova$Stance %in% c("Open Stance","Sideways","Switch","")),"Stance2"]<-"autres"
data_ancova[which(data_ancova$Stance %in% c("Orthodox")),"Stance2"]<-"Orthodox"
data_ancova[which(data_ancova$Stance %in% c("Southpaw")),"Stance2"]<-"Southpaw"

data_ancova[data_ancova$age < 25 , "Age2"]<-"-25ans"
data_ancova[data_ancova$age >= 25 & data_ancova$age < 30, "Age2"]<-"25-30ans"
data_ancova[data_ancova$age >= 30 & data_ancova$age < 35, "Age2"]<-"30-35ans"
data_ancova[data_ancova$age >= 35 , "Age2"]<-"+35ans"

summary(data_ancova)

#Analyse graphique
boxplot(formula=ratio_victoire~categorie_poids2, data=data_ancova, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")
plot(data_ancova$ratio_victoire,data_ancova$avg_TOTAL_STR_att)
#Cr�ation du mod�le

#Mod�le avec coefficient B par modalit� 
mod.ancova1=lm(formula=ratio_victoire~Age2+avg_TOTAL_STR_att,data=data_ancova)
mod.ancova2=lm(formula=ratio_victoire~Age2+avg_TOTAL_STR_att:Age2,data=data_ancova)

summary(mod.ancova1)
anova(mod.ancova1)


anova(mod.ancova1,mod.ancova2)



############################################################################
#############S�lection du meilleurs mod�le
############################################################################
colnames(joueur_unique)
data_select<-na.omit(joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) ,
                                   c("fighter","age","ratio_victoire","weight_class","Stance","avg_HEAD_att","avg_TOTAL_STR_att","avg_BODY_att","avg_CLINCH_att","avg_DISTANCE_att","avg_GROUND_att","avg_KD","avg_SUB_ATT","Height_cms")])

data_select[which(data_select$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_select[which(data_select$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_select[which(data_select$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_select[which(data_select$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_select[which(data_select$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_select[which(data_select$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"
data_select$categorie_poids2<-as.factor(data_select$categorie_poids2)

data_select[which(data_select$Stance %in% c("Open Stance","Sideways","Switch","")),"Stance2"]<-"autres"
data_select[which(data_select$Stance %in% c("Orthodox")),"Stance2"]<-"Orthodox"
data_select[which(data_select$Stance %in% c("Southpaw")),"Stance2"]<-"Southpaw"
data_select$Stance2<-as.factor(data_select$Stance2)

data_select[data_select$age < 25 , "Age2"]<-"-25ans"
data_select[data_select$age >= 25 & data_select$age < 30, "Age2"]<-"25-30ans"
data_select[data_select$age >= 30 & data_select$age < 35, "Age2"]<-"30-35ans"
data_select[data_select$age >= 35 , "Age2"]<-"+35ans"
data_select$Age2<-as.factor(data_select$Age2)

data_select<-na.omit(data_select[,!colnames(data_select) %in% c("fighter","age","Stance","weight_class")])
summary(data_select)
str(data_select)

full.model <- lm(ratio_victoire ~., data = data_select)
summary(full.model)

simple.model <- lm(ratio_victoire ~1, data = data_select)
summary(simple.model)

backward <- stepAIC(full.model, direction = "backward")
#ratio_victoire ~ avg_HEAD_att + avg_DISTANCE_att + avg_GROUND_att + 
#  avg_KD + avg_SUB_ATT + Height_cms

forward <- stepAIC(simple.model, direction="forward", scope=list(lower=simply.model, upper=full.model))
#ratio_victoire ~ avg_GROUND_att + avg_KD + avg_DISTANCE_att + 
#  avg_SUB_ATT + avg_HEAD_att + Height_cms

stepwise_aic <- stepAIC(simple.model, direction="both", scope=list(lower=simply.model, upper=full.model))
#ratio_victoire ~ avg_GROUND_att + avg_KD + avg_DISTANCE_att + 
#  avg_SUB_ATT + avg_HEAD_att + Height_cms
summary(stepwise_aic)


n = dim(data_select)[1]
stepwise_bic <- stepAIC(simple.model, direction="both", scope=list(lower=simply.model, upper=full.model),k=log(n))
#ratio_victoire ~ avg_GROUND_att + avg_KD + avg_DISTANCE_att + avg_SUB_ATT
summary(stepwise_bic)


############################################################################
# Regression logistique - Issue du match en fonction du style
############################################################################

# Pensez à bien vérifier le format de vos champs !!!
# Pensez à regarder si des erreurs de saisie / valeurs aberrantes sont présentes
# Sélection des variables & année
table(data$Winner)

# Filtrage sur colonnes "Winner","B_Stance","R_Stance","B_avg_DISTANCE_att","R_avg_DISTANCE_att"
# uniquement les winner blue et red sur les matches supérieurs à 2010
# 3579 matchs
data<-data[year(data$date)>2010 & data$Winner %in% c("Blue","Red"), c("Winner","B_Stance","R_Stance","B_avg_DISTANCE_att","R_avg_DISTANCE_att")]
# Retrait des matchs avec joueurs sans style de combat
# 3346 matchs
data<-data[-which(data$B_Stance=="" |data$R_Stance=="") ,]
# Retrait des matchs avec joueurs sans moyenne de coups portés à distance (na)
# 2641 matchs
data<-data[-which(is.na(data$B_avg_DISTANCE_att)),]
# 2545 matchs
data<-data[-which(is.na(data$R_avg_DISTANCE_att)),]

# Echantillonnage à 50/50 sur la variable à prédire
# 1471 matchs gagnés par des rouges
red<-data[data$Winner=="Red",]
# 1074 matchs gagnés par des bleus
blue<-data[data$Winner=="Blue",]
# Echantillon de 1000 matchs gagnés par des rouges
sample_red<-sample(1:dim(red)[1],1000)
# Echantillon de 1000 matchs gagnés par des bleus
sample_blue<-sample(1:dim(blue)[1],1000)
# base de 2000 matchs gagnés par des rouges ou des bleus
data_reg<-rbind(red[sample_red,],blue[sample_blue,])
table(data_reg$Winner)

# Création des variables explicatives 
# Ajout d'une variable explicative diff_dist_att écart distance moyenne de coups portés
data_reg$diff_dist_att <- data_reg$B_avg_DISTANCE_att-data_reg$R_avg_DISTANCE_att
# Ajout d'une variable explicative diff_win écart nombre de matchs gagnés
data_reg$diff_win <- data_reg$B_wins-data_reg$R_wins
table(data_reg$Winner)

# Mise en classe diff_dist_att
hist(data_reg$diff_dist_att, breaks = 100)
summary(data_reg$diff_dist_att)

ggplot(data_reg, aes(x = diff_dist_att)) +
    geom_histogram(aes(color = Winner, fill = Winner), 
                   position = "identity", bins = 30, alpha = 0.4) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    scale_fill_manual(values = c("#00AFBB", "#E7B800"))

data_reg[data_reg$diff_dist_att< -50,"diff_dist_att_class"]<-"++Rouge"
data_reg[data_reg$diff_dist_att< -15 & data_reg$diff_dist_att>= -50 ,"diff_dist_att_class"]<-"+Rouge"
data_reg[data_reg$diff_dist_att< 15 & data_reg$diff_dist_att>= -15 ,"diff_dist_att_class"]<-"egal"
data_reg[data_reg$diff_dist_att< 50 & data_reg$diff_dist_att>= 15 ,"diff_dist_att_class"]<-"+Bleu"
data_reg[ data_reg$diff_dist_att>= 50 ,"diff_dist_att_class"]<-"++Bleu"
table(data_reg$diff_dist_att_class)

# Calcul des indicateurs de style
data_reg[data_reg$B_Stance==data_reg$R_Stance,"style_similaire"]<-"oui"
data_reg[data_reg$B_Stance!=data_reg$R_Stance,"style_similaire"]<-"non"

# Mise au format factor et gestion des levels 
str(data_reg)
summary(data_reg)

data_reg$diff_dist_att_class<-as.factor(data_reg$diff_dist_att_class)
data_reg$style_similaire<-as.factor(data_reg$style_similaire)
data_reg$Winner <- as.factor(data_reg$Winner)

levels(data_reg$Winner)
levels(data_reg$diff_dist_att_class)
levels(data_reg$style_similaire)

data_reg$diff_dist_att_class<-factor(data_reg$diff_dist_att_class, levels=c("egal","++Bleu","++Rouge","+Bleu","+Rouge"))
data_reg$Winner<-factor(data_reg$Winner, levels=c("Red","Blue"))

# Construction du modèle
table(data_reg$Winner)
model_quali<-glm(Winner~diff_dist_att_class,data=data_reg,family= binomial(logit))

# Interprétation
model_quali
summary(model_quali)
exp(coef(model_quali))

# Matrice de confusion
appren.p <- cbind(data_reg, predict(model_quali, newdata = data_reg, type = "link", 
                                    se = TRUE))
appren.p <- within(appren.p, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedProb > 0.5, 1, 0)))
colnames(appren.p)
appren.p<-appren.p[,c("Winner","diff_dist_att","diff_dist_att_class","fit","PredictedProb","pred.chd")]
(m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$Winner)))

# Taux de bien classé
(m.confusion[1,1]+m.confusion[2,2]) / sum(m.confusion)

# Sensibilité
(m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])

# Sensibilité
(m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])

# Spécificité 
(m.confusion[1,1]) / (m.confusion[1,1]+m.confusion[2,1])


# ODs ratio
exp(cbind(coef(model_quali), confint(model_quali)))
library(questionr)
odds.ratio(model_quali)
install.packages("GGally")
library(GGally)
library(broom.helpers)
ggcoef_model(model_quali, exponentiate = TRUE)

############################################################################
# Regression logistique - La variable à prédire est « Winner »
# 1. Importer le fichier « Data.csv »
#   1.Vérifier le format des champs et le bon import des données
# 2. La variable à prédire est « Winner »
#   1.Construire le meilleur modèle possible
############################################################################

colnames(joueur_unique)
data_select<-na.omit(joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) ,
                                   c("fighter","age","ratio_victoire","weight_class","Stance","avg_HEAD_att","avg_TOTAL_STR_att","avg_BODY_att","avg_CLINCH_att","avg_DISTANCE_att","avg_GROUND_att","avg_KD","avg_SUB_ATT","Height_cms")])

data_select[which(data_select$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_select[which(data_select$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_select[which(data_select$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_select[which(data_select$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_select[which(data_select$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_select[which(data_select$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"
data_select$categorie_poids2<-as.factor(data_select$categorie_poids2)

data_select[which(data_select$Stance %in% c("Open Stance","Sideways","Switch","")),"Stance2"]<-"autres"
data_select[which(data_select$Stance %in% c("Orthodox")),"Stance2"]<-"Orthodox"
data_select[which(data_select$Stance %in% c("Southpaw")),"Stance2"]<-"Southpaw"
data_select$Stance2<-as.factor(data_select$Stance2)

data_select[data_select$age < 25 , "Age2"]<-"-25ans"
data_select[data_select$age >= 25 & data_select$age < 30, "Age2"]<-"25-30ans"
data_select[data_select$age >= 30 & data_select$age < 35, "Age2"]<-"30-35ans"
data_select[data_select$age >= 35 , "Age2"]<-"+35ans"
data_select$Age2<-as.factor(data_select$Age2)

data_select<-na.omit(data_select[,!colnames(data_select) %in% c("fighter","age","Stance","weight_class")])
summary(data_select)
str(data_select)

full.model <- lm(ratio_victoire ~., data = data_select)
summary(full.model)

simple.model <- lm(ratio_victoire ~1, data = data_select)
summary(simple.model)

backward <- stepAIC(full.model, direction = "backward")
#ratio_victoire ~ avg_HEAD_att + avg_DISTANCE_att + avg_GROUND_att + 
#  avg_KD + avg_SUB_ATT + Height_cms

forward <- stepAIC(simple.model, direction="forward", scope=list(lower=simply.model, upper=full.model))
#ratio_victoire ~ avg_GROUND_att + avg_KD + avg_DISTANCE_att + 
#  avg_SUB_ATT + avg_HEAD_att + Height_cms

stepwise_aic <- stepAIC(simple.model, direction="both", scope=list(lower=simply.model, upper=full.model))
#ratio_victoire ~ avg_GROUND_att + avg_KD + avg_DISTANCE_att + 
#  avg_SUB_ATT + avg_HEAD_att + Height_cms
summary(stepwise_aic)


n = dim(data_select)[1]
stepwise_bic <- stepAIC(simple.model, direction="both", scope=list(lower=simply.model, upper=full.model),k=log(n))
#ratio_victoire ~ avg_GROUND_att + avg_KD + avg_DISTANCE_att + avg_SUB_ATT
summary(stepwise_bic)

