##################################################################################
# 1 - Import des données
##################################################################################
library(data.table)
library(lubridate)

setwd("C:/Transway/BI_Data/scripts_R/regression_lineaire/stat2_joueurs_UFC")

# Importer la base de données
data <- read.csv("data/data.csv", sep = ",")

# vérification du format des champs
# Recherche des erreurs de saisie / valeurs aberrantes
summary(data)
str(data)

# Correction champ date
data$date<-as.Date(data$date)
data <- data[year(data$date)>2000,]

##################################################################################
# 2 - Création d'une table de combattant unique
##################################################################################


# Etape 1 : faire un dataframe avec toutes les variables concernant le joueur bleu + Date + Gagnant + Catégorie de poids + Nombre de round
nom_colonne <- colnames(data)
col_bleu <- nom_colonne[nom_colonne %like% "^B_"]
data_bleu <- data[ ,c("date","Winner","title_bout","weight_class","no_of_rounds", col_bleu) ]

# Etape 2 : faire un dataframe avec toutes les variables concernant le joueur rouge + Date + Gagnant + Catégorie de poids + Nombre de round
col_rouge <- nom_colonne[nom_colonne %like% "^R_"]
data_rouge <- data[ ,c("date","Winner","title_bout","weight_class","no_of_rounds", col_rouge) ]

# Etape 3 : Renommer les variables pour que les noms de colonnes soit identiques entre les deux dataframe créés ci-dessus
colnames(data_bleu) <- c("date","Winner","title_bout","weight_class","no_of_rounds", substr(col_bleu, 3, 1000))
colnames(data_rouge) <- c("date","Winner","title_bout","weight_class","no_of_rounds", substr(col_rouge, 3, 1000))

# Etape 4 : Concaténer les deux dataframes en un seul
base_joueur_match <- rbind(data_rouge, data_bleu)

# Etape 5 : Sélectionner seulement la ligne correspondant au dernier combat par combattant
date_max <- aggregate(date ~ fighter, data = base_joueur_match, max )

joueur_unique <- merge(base_joueur_match, date_max, 
                       by.x = c("date","fighter"), by.y = c("date","fighter"),
                       all.x = F, all.y = F)

doublon <- data.frame(table(joueur_unique$fighter))
doublon[doublon$Freq>1,]

##################################################################################
# 3 - Calculer la régression linéaire simple entre le poids et la taille 
##################################################################################
attach(joueur_unique)

data_taille_poids <- na.omit(joueur_unique[fighter, c("Weight_lbs", "Height_cms")])
summary(data_taille_poids)

# A - Analyse graphique
plot(data_taille_poids$Weight_lbs, data_taille_poids$Height_cms)
table(data_taille_poids$Height_cms)

# B - Construction du modèle
str(data_taille_poids)
res.lm<-lm( Weight_lbs~Height_cms, data = data_taille_poids)
?lm
summary(res.lm)

plot(Weight_lbs~Height_cms, data = data_taille_poids, pch = 16)
abline(res.lm, col="red", lwd=2)

# B1 - Estimation des paramètres (méthode des moindres carrés)
# B2 - Test global du modèle (test F) / tests de nullité des coefficients
# B3 - Qualité du modèle (coeffficient R²)
# C - Vérification des hypothèses
# C1 - Valeurs ajustées/ résidus studentisées indépendance, structure de variance, points aberrants)
rstud = rstudent(res.lm)
ecart<-cbind(data_taille_poids, res.lm$fitted.values, res.lm$residuals, rstud)
plot(rstud, pch=20, ylab="Résidus studentisés", ylim=c(-3,3))
abline(h=c(0), col="grey", lty=1, lwd=2)
abline(h=c(-2,2), col="grey", lty=2, lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=res.lm)
plot(res.cook, type="h", ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5, col="gray", lty=2)

# C3 - Droite de Henry (normalité)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

# D - Prédiction 
# Quelle serait la taille d'une personne pesant 135 lbs
hist(data_taille_poids$Height_cms)
new <- data.frame(Height_cms = seq(160, 200, 5))

yy <- cbind(new, predict(res.lm, new, interval="prediction"))
yy
##################################################################################
# 4- Calculer la régression linéaire multiple entre le ratio de victoire (nb victoire / nb match) et 
# Le nombre de coups à la tête / le nombre de coup au corps / le nombre de coup au sol
##################################################################################
# A - Analyse graphique

joueur_unique$ratio_victoire<-joueur_unique$wins/(joueur_unique$wins + joueur_unique$losses)
hist(joueur_unique$ratio_victoire)

# On garde seulement les combattants avec plus de 3 matchs
base_reg <- joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) & joueur_unique$weight_class == "Welterweight", c("fighter","weight_class","ratio_victoire","avg_BODY_att","avg_HEAD_att","avg_GROUND_att") ]
hist(base_reg$ratio_victoire)
table(joueur_unique$wins + joueur_unique$losses)
summary(base_reg)
str(base_reg)

# Analyse graphique
plot(base_reg)
cor(base_reg$ratio_victoire, base_reg$avg_BODY_att)
cor(base_reg$ratio_victoire, base_reg$avg_HEAD_att)
cor(base_reg$ratio_victoire, base_reg$avg_GROUND_att)
cor(base_reg$avg_GROUND_att, base_reg$avg_HEAD_att)

# B  - Construction du modèle
# B1 - Estimation des paramètres (méthodes des moindres carrés)
# B2 - Test global du modèle (test F) / tests de nullité des coefficients
# B3 - Qualité du modèle (coefficient R²)
res.lm <- lm(ratio_victoire~avg_GROUND_att+avg_HEAD_att+avg_BODY_att, data = base_reg)
summary(res.lm)

plot(Weight_lbs~Height_cms, data = data_taille_poids, pch=16)
abline(res.lm, col="red", lwd=2)

# B1 - Estimation des paramètres (méthode des moindres carrés)
# B2 - Test global du modèle (test F) / tests de nullité des coefficients
# B3 - Qualité du modèle (coeffficient R²)

# C - Vérification des hypothèses
# C1 - Valeurs ajustées/ résidus studentisées indépendance, structure de variance, points aberrants)
rstud = rstudent(res.lm)
plot(rstud, pch=20, ylab="Résidus studentisés", ylim=c(-3,3))
abline(h=c(0), col="grey", lty=1, lwd=2)
abline(h=c(-2,2), col="grey", lty=2, lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=res.lm)
plot(res.cook, type="h", ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5, col="gray", lty=2)

# C3 - Droite de Henry (normalité)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7), xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

############################################################################
# ANOVA entre nombre de coup tentés et catégorie de poids retravaillé
############################################################################

# Analyse de la variable weight_class
data.frame(table(joueur_unique$weight_class))
aggregate(avg_TOTAL_STR_att~weight_class, data = joueur_unique, mean)
hist(joueur_unique$avg_TOTAL_STR_att)
boxplot(formula=avg_TOTAL_STR_att~weight_class, data=joueur_unique, boxwex=0.3, col="lightblue", pch=10, xlab="Catégorie de poids")

# Premier modèle sans recodage des modalités
mod.lm=lm(formula=avg_TOTAL_STR_att~weight_class, data=joueur_unique)
anova(mod.lm)
summary(mod.lm)

# Regroupement des classes de poids
base_reg<-joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) & joueur_unique$weight_class == "Welterweight", c("fighter","weight_class","ratio_victoire","avg_BODY_att","avg_HEAD_att","avg_GROUND_att") ]
data_anova<-joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) &!is.na(joueur_unique$avg_TOTAL_STR_att),c("fighter","avg_TOTAL_STR_att","weight_class","avg_HEAD_att")]
data_anova[which(data_anova$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_anova[which(data_anova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"
hist(data_anova$avg_TOTAL_STR_att)

# Analyse graphique
boxplot(formula=avg_TOTAL_STR_att~categorie_poids2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Catégorie de poids")

# Création du modèle
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2,data=data_anova)
anova(mod.lm)
summary(mod.lm)

# C - Vérification des hypothèses
# C1 - Valeurs ajustées / résidus studentisées indépendance, structure de variance, points aberrants)
rstud = rstudent(mod.lm)
plot(rstud, pch=20, ylab="Résidus studentisés", ylim=c(-3,3))
abline(h=c(0), col="grey", lty=1, lwd=2)
abline(h=c(-2,2), col="grey", lty=2, lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=mod.lm)
plot(res.cook, type="h", ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5, col="gray", lty=2)

# C3 - Droite de Henry (normalité)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7), xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)


##################################################################################
# 5- Calculer une ANOVA multi
#
# Prédire le nombre de coup tenté en fonction :
# 
# 1. De la catégorie de poids 
# 2. Du style de combat <- Stance
# 
# Tester les modèles :
# 1. Sans interaction
# 2. Avec interaction
# 3. Hiérarchique (style dépend du poids)
#
# landed = coup tenté, Att = coup réussi
##################################################################################

str(joueur_unique)

# Sans interaction
anova.lm <- lm(avg_TOTAL_STR_att ~ categorie_poids2 + Stance, data=data_anova)
anova(anova.lm)

# Avec interaction
anova_i.lm<-lm(avg_TOTAL_STR_att ~ categorie_poids2 + Stance + categorie_poids2*Stance, data=data_anova)
anova(anova_i.lm)

# Analyse graphique
plot(base_anova)

# correlations
cor(base_anova)

# C - Vérification des hypothèses
# C1 - Valeurs ajustées / résidus studentisés (indépendance, structure de variance, points aberrants)
rstud = rstudent(anova_i.lm)
plot(rstud,pch=20,ylab="Résidus studentisés",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=anova_i.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalité)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7), xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

ecart<-cbind(data_taille_poids, res.lm$fitted.values, res.lm$residuals,rstud)

# Hiérarchique (style dépend du poids)
anova_h.lm<-lm(avg_TOTAL_STR_att ~ categorie_poids2 + Stance + categorie_poids2:Stance, data=data_anova)
anova(anova_h.lm)

##################################################################################
# 6 - Calculer une ANCOVA
#
# Prédire le nombre de victoire / nombre de match en fonction :
# 1. De la catégorie de poids
# 2. Du nombre de coup total
# 
# Calculer un modèle de régression linéaire simple par catégorie
# de poids entre pour prédire le nombre de victoire / nombre de match en fonction :
# 1. Du nombre de coup total
# 
# Quelle solution vous semble la meilleure ? 
#
# "code pablo"
##################################################################################

# Regroupement des classes de poids
data_anova<- na.omit(joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) &!is.na(joueur_unique$avg_TOTAL_STR_att) ,c("fighter","avg_TOTAL_STR_att","weight_class","avg_HEAD_att", "Stance", "ratio_victoire")])

data_anova[which(data_anova$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_anova[which(data_anova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"

levels(data_anova$categorie_poids2) <- c("")
levels.default(data_anova$categorie_poids2)

#### ancova ####
ancova.lm <- lm(formula = ratio_victoire ~ categorie_poids2+avg_TOTAL_STR_att:categorie_poids2, data=data_anova) # :
summary(ancova.lm)
anova(ancova.lm)

#### ancova LM 2####
ancovalm2.lm <- lm(formula = ratio_victoire ~ categorie_poids2+avg_TOTAL_STR_att, data=data_anova)
summary(ancovalm2.lm)
anova(ancova.lm, ancovalm2.lm)

#### ancova LM 3- ANOVA ####
ancovalm3.lm <- lm(formula = ratio_victoire ~ categorie_poids2, data=data_anova)
summary(ancovalm3.lm)
anova(ancovalm2.lm, ancovalm3.lm)

#### Régression linéaire  ####
res.lm<-lm(ratio_victoire ~ avg_TOTAL_STR_att, data = data_anova)
summary(res.lm)
# anova(ancovalm2.lm, res.lm)

plot(ratio_victoire~avg_TOTAL_STR_att , data = base_reg ,pch=16)
abline(res.lm,col="red",lwd=2)

#analyse graphique
plot(res.lm, which = 1:4)

##################################################################################
# 7 - Faire une régression logistique
# 1. Importer le fichier « Data.csv »
#   1.Vérifier le format des champs et le bon import des données
# 2. La variable à prédire est « Winner »
#   1.Prédire le gagnant du match en fonction de
#       R_Stance
#       B_Stance
#       Créer un nouvelle indicateur pour savoir si les deux combattants ont le même style de combat
##################################################################################


?glm





