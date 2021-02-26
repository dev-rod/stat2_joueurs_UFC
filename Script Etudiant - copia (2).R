##################################################################################
# 1 - Import des donn�es 
##################################################################################
#### import librairie ####
if("" %in% rownames(installed.packages()) == FALSE) {install.packages("")};library()

library(data.table)
library(lubridate)

library(tidyverse)
library(dplyr)

# Importer la base de donn�es

# library(here)
# here('Data')
# data.csv
data <- read.csv("E:/MEDAS CNAM/MASTER_MEDAS/STATISTIQUES II 2021/Pour étudiant/Pour étudiant/UFC_preditions/Data/data.csv",sep = ",")

# Pensez � bien v�rifier le format de vos champs !!!
# Pensez � regarder si des erreurs de saisie / valeurs aberrantes sont pr�sentes
summary(data)
str(data)

data$date<-as.Date(data$date)
data<-data[year(data$date)>2000,]
##################################################################################
# 2 - Cr�ation d'une table de combattant unique
##################################################################################

# Etape 1 : faire un dataframe avec toutes les variables concernant le joueur bleu + Date + Gagnant + Cat�gorie de poids + Nombre de round
nom_colonne<-colnames(data)
col_rouge<-nom_colonne[nom_colonne %like% "^R_"]
col_bleu<-nom_colonne[nom_colonne %like% "^B_"]

data_rouge<-data[ ,c("date","Winner","title_bout","weight_class","no_of_rounds", col_rouge) ]
data_bleu<-data[ ,c("date","Winner","title_bout","weight_class","no_of_rounds", col_bleu) ]

colnames(data_rouge)<-c("date","Winner","title_bout","weight_class","no_of_rounds",substr(col_rouge,3,1000) )
colnames(data_bleu)<-c("date","Winner","title_bout","weight_class","no_of_rounds",substr(col_bleu,3,1000) )

base_joueur_match <- rbind(data_rouge,data_bleu)

date_max <- aggregate(date ~ fighter , data = base_joueur_match , max )

joueur_unique <- merge(base_joueur_match,date_max, 
                       by.x = c("date","fighter") ,by.y = c("date","fighter"),
                       all.x = F , all.y = F)


doublon<-data.frame(table(joueur_unique$fighter))
doublon[doublon$Freq>1,]

# Etape 2 : faire un dataframe avec toutes les variables concernant le joueur rouge + Date + Gagnant + Cat�gorie de poids + Nombre de round
# Etape 3 : Renommer les variables pour que les noms de colonnes soit identiques entre les deux dataframe cr��s ci-dessus 
# Etape 4 : Concatener les deux dataframes en un seul
# Etape 5 : S�lectionner seulement la ligne correspondant au dernier combat par combattant

##################################################################################
# 3 - Calculer la r�gression lin�aire simple entre le poids et la taille 
##################################################################################
attach(joueur_unique)

data_taille_poids <- na.omit(joueur_unique[, c("Weight_lbs","Height_cms")])
summary(data_taille_poids)

# A - Analyse graphique
plot(data_taille_poids$Weight_lbs,data_taille_poids$Height_cms)
table(data_taille_poids$Height_cms)

# B - Construction du mod�le 
str(data_taille_poids)
res.lm<-lm( Weight_lbs~Height_cms  , data = data_taille_poids)
?lm
summary(res.lm)

plot(Weight_lbs~Height_cms , data = data_taille_poids ,pch=16)
abline(res.lm,col="red",lwd=2)

# B1 - Estimation des param�tres (m�thode des moindres carr�s)
# B2 - Test global du mod�le (test F) / tests de nullit� descoefficients
# B3 - Qualit� du mod�le (coefficient R�)

# C - V�rification des hypoth�ses
# C1 - Valeurs ajust�es / r�sidus studentis�s (ind�pendance, structure de variance, points aberrants)
rstud = rstudent(res.lm)
plot(rstud,pch=20,ylab="R�sidus studentis�s",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=res.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalit�)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

ecart<-cbind(data_taille_poids, res.lm$fitted.values,res.lm$residuals,rstud)
ecart

# D - Pr�diction 
# Quel serait la taille d'une personne pesant 135 lbs
hist(data_taille_poids$Height_cms)
new <- data.frame(Height_cms = seq(160, 200, 5))

yy <- cbind(new,predict(res.lm, new, interval="prediction"))

##################################################################################
# 4- Calculer la r�gression lin�aire multiple entre le ratio de victoire et 
# Le nombre de coup � la t�te /  "avg_HEAD_att
# le nombre de coup au corp / avg_BODY_att
# le nombre de coup au sol / avg_GROUND_att

##################################################################################
# A - Analyse graphique

str(joueur_unique)

joueur_unique$ratio_victoire <- joueur_unique$wins/(joueur_unique$wins+joueur_unique$losses)

# unique weight_class
unique(joueur_unique$weight_class)

base_reg <- joueur_unique[joueur_unique$wins + joueur_unique$losses>3 & joueur_unique$weight_class == 'Welterweight' , c("avg_HEAD_att","avg_BODY_att","avg_GROUND_att","ratio_victoire")]

hist(base_reg$ratio_victoire)

summary(base_reg)
str(base_reg)

# voir NAs, datatype, etc. 

# analyse graphique
plot(base_reg)

cor(base_reg$avg_HEAD_att, base_reg$avg_BODY_att)
cor(base_reg$avg_HEAD_att, base_reg$avg_GROUND_att) # pas trop de correlation
cor(base_reg)

# B - Construction du mod�le 

# Voir exemple :
# http://www.sthda.com/english/articles/40-regression-analysis/168-multiple-linear-regression-in-r/

res.lm<-lm(ratio_victoire ~ avg_BODY_att + avg_GROUND_att, data = base_reg)
summary(res.lm)

plot(ratio_victoire ~ avg_HEAD_att , data = base_reg ,pch=16)
abline(res.lm,col="red",lwd=2)

# B1 - Estimation des param�tres (m�thode des moindres carr�s)
# B2 - Test global du mod�le (test F) / tests de nullit� descoefficients
# B3 - Qualit� du mod�le (coefficient R�)

# C - V�rification des hypoth�ses
# C1 - Valeurs ajust�es / r�sidus studentis�s (ind�pendance, structure de variance, points aberrants)
rstud = rstudent(res.lm)
plot(rstud,pch=20,ylab="R�sidus studentis�s",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=res.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalit�)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

ecart<-cbind(data_taille_poids, res.lm$fitted.values,res.lm$residuals,rstud)

##################################################################################
# 4- Calculer une ANOVA

# Prédire le nombre de coup tenté en fonction de la catégorie
# de poids.

# Prédire le nombre de coup à la tête reçu en fonction de la
# catégorie de poids. Que concluez-vous ? 
##################################################################################

# landed = coup tenté, Att = coup réussi

data.frame(table(joueur_unique$weight_class))
aggregate(avg_TOTAL_STR_att~weight_class, data = joueur_unique, mean)

str(joueur_unique)

base_anova <- na.omit(joueur_unique[, c("avg_HEAD_landed","avg_TOTAL_STR_att","weight_class")])

# FACTORISATION 
# base_anova$weight_factor <- as.factor(base_anova$weight_class)

# analyse graphique
boxplot(formula=avg_TOTAL_STR_att ~ weight_class,  data = base_anova, boxwex=0.3, col='lightblue', pch=10, xlab="weight_class")

hist(base_anova$avg_TOTAL_STR_att)

plot(base_anova)

#Premier mod�le sans recodage des modalit�s
mod.lm<-lm(avg_TOTAL_STR_att ~ weight_class, data = base_anova)
summary(mod.lm)
anova(mod.lm)

# C - V�rification des hypoth�ses
# C1 - Valeurs ajust�es / r�sidus studentis�s (ind�pendance, structure de variance, points aberrants)
rstud = rstudent(mod.lm)
plot(rstud,pch=20,ylab="R�sidus studentis�s",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=mod.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalit�)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

ecart<-cbind(data_taille_poids, res.lm$fitted.values,res.lm$residuals,rstud)

### Factor conversion, which keeps value attributes ####
unique(joueur_unique$weight_class)

# create parially labelled vector | del mas ligero al mas pesado
joueur_unique$weight_factor <- set_labels(
  df_maxdate$weight_class,
  labels = c(
    `1` = "Flyweight",
    `4` = "Bantamweight",
    `9` = "Featherweight",
    `9` = "Lightweight",
    `9` = "",
    `9` = "",
    `9` = "",
    `9` = "",
    `9` = "",
  ))

#Signification et choix des classes
#Regroupement des classes de poids
data_anova<- na.omit(joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) &!is.na(joueur_unique$avg_TOTAL_STR_att), c("fighter","avg_TOTAL_STR_att","weight_class","avg_HEAD_att", "Stance", "ratio_victoire")])

data_anova[which(data_anova$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_anova[which(data_anova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"

# unique(data_anova$Stance)
data_anova[which(data_anova$Stance %in% c("Orthodox", "Switch","Southpaw", "Open Stance")),"Stance2"]<-"autres"
data_anova[which(data_anova$Stance %in% c("Orthodox")),"Stance2"]<-"Orthodox"
data_anova[which(data_anova$Stance %in% c("Southpaw")),"Stance2"]<-"Southpaw"

hist(data_anova$avg_TOTAL_STR_att)

#Cr�ation du mod�le
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2,data=data_anova)
anova(mod.lm)


##################################################################################
# 5- Calculer une ANOVA multi

# Prédire le nombre de coup tenté en fonction :
# 
# 1. De la catégorie de poids 
# 2. Du style de combat <- Stance
# 
# Tester les modèles :
# 1. Sans interaction
# 2. Avec interaction
# 3. Hiérarchique (style dépend du poids)

# Notes:
# str(joueur_unique)
# landed = coup tenté, Att = coup réussi

##################################################################################

table(joueur_unique$Stance)
aggregate(avg_TOTAL_STR_att~Stance, data = joueur_unique, mean)
boxplot(formula=avg_TOTAL_STR_att ~ Stance,  data = joueur_unique , boxwex=0.3, col='lightblue', pch=10)

levels(data_anova$categorie_poids2) <- c("")
levels.default(data_anova$categorie_poids2)

# Sans interaction
anova.lm <- lm(formula = avg_TOTAL_STR_att ~ categorie_poids2+Stance, data=data_anova)
summary(anova.lm)
anova(anova.lm)

# Avec interaction
anova_i.lm<-lm(formula = avg_TOTAL_STR_att ~ categorie_poids2*Stance, data=data_anova)
summary(anova_i.lm)
anova(anova_i.lm)

# Analyse graphique
plot(base_anova)

# C - V�rification des hypoth�ses
# C1 - Valeurs ajust�es / r�sidus studentis�s (ind�pendance, structure de variance, points aberrants)
rstud = rstudent(anova_i.lm)
plot(rstud,pch=20,ylab="R�sidus studentis�s",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=anova_i.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalit�)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

ecart<-cbind(data_taille_poids, res.lm$fitted.values,res.lm$residuals,rstud)


# Hiérarchique (style dépend du poids)
anova_h.lm<-lm(formula = avg_TOTAL_STR_att ~ categorie_poids2:Stance, data=data_anova)
summary(anova_h.lm)
anova(anova_h.lm)

##################################################################################
# 6 - Calculer une ANCOVA 

# Prédire le nombre de victoire / nombre de match en fonction :
# 1. De la catégorie de poids
# 2. Du nombre de coup total
# 
# Calculer un modèle de régression linéaire simple par catégorie
# de poids entre pour prédire le nombre de victoire / nombre de match en fonction :
# 1. Du nombre de coup total
# 
# Quelle solution vous semble la meilleure ? 

##################################################################################

#Regroupement des classes de poids
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
ancova.lm <- lm(formula = ratio_victoire ~ categorie_poids2+avg_TOTAL_STR_att:categorie_poids2, data=data_anova)
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

plot(res.lm, which = 1:4)

plot(ratio_victoire~avg_TOTAL_STR_att , data = data_anova ,pch=16)
abline(res.lm,col="red",lwd=2)

#######################################################
# S�lection du meilleurs mod�le

# Trouver le meilleur modèle pour prédire le
# nombre de victoire / nombre de match

# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/

#######################################################

# colnames(joueur_unique)

# factorisation des varibales
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

# library(caret)
# library(leaps)
library(MASS)

# data_select <- na.omit(data_select)

# re-selection des variables 
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

forward <- stepAIC(simple.model, direction="forward", scope=list(lower=simple.model, upper=full.model))
#ratio_victoire ~ avg_GROUND_att + avg_KD + avg_DISTANCE_att + 
#  avg_SUB_ATT + avg_HEAD_att + Height_cms

stepwise_aic <- stepAIC(simple.model, direction="both", scope=list(lower=simple.model, upper=full.model))
#ratio_victoire ~ avg_GROUND_att + avg_KD + avg_DISTANCE_att + 
#  avg_SUB_ATT + avg_HEAD_att + Height_cms
summary(stepwise_aic)

n = dim(data_select)[1]
stepwise_bic <- stepAIC(simple.model, direction="both", scope=list(lower=simple.model, upper=full.model),k=log(n))
#ratio_victoire ~ avg_GROUND_att + avg_KD + avg_DISTANCE_att + avg_SUB_ATT
summary(stepwise_bic)


# # ?
models <- regsubsets(ratio_victoire~., data = data_select, nvmax = 5,
                     method = "seqrep")
summary(models)

#######################################################
# Regression logistique 

# Prédire le gagnant du match en fonction de :
# 1
# B_avg_body_landel - R_avg_body_landed (mise en classe)

# 2
# • R_Stance
# • B_Stance
# • Créer un nouvelle indicateur pour savoir si les deux combattants ont le même
# style de combat

#######################################################

# avant il y a que des gagnes rouges
data<-data[year(data$date)>2010 & data$Winner %in% c("Blue","Red"),c("Winner","B_Stance","R_Stance","B_avg_DISTANCE_att","R_avg_DISTANCE_att")]
data<-data[-which(data$B_Stance=="" |data$R_Stance=="") ,]
data<-data[-which(is.na(data$B_avg_DISTANCE_att)),]

data<-data[-which(is.na(data$R_avg_DISTANCE_att)),]

#Echantillonnage � 50/50 sur la variable � pr�dire
red<-data[data$Winner=="Red",]
blue<-data[data$Winner=="Blue",]
sample_red<-sample(1:dim(red)[1],1000)
sample_blue<-sample(1:dim(blue)[1],1000)
data_reg<-rbind(red[sample_red,],blue[sample_blue,])
table(data_reg$Winner)

#Cr�ation des variables explicatives 
data_reg$diff_dist_att <- data_reg$B_avg_DISTANCE_att-data_reg$R_avg_DISTANCE_att
data_reg$diff_win <- data_reg$B_wins-data_reg$R_wins
table(data_reg$Winner)

#Mise en classe diff_dist_att
hist(data_reg$diff_dist_att,breaks = 100)
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

#Mise au format factor et gestion des levels 
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

# Construction du mod�le
table(data_reg$Winner)
model_quali<-glm(Winner~diff_dist_att_class,data=data_reg,family= binomial(logit))
#interpr�tation
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

#Taux de bien class�
(m.confusion[1,1]+m.confusion[2,2]) / sum(m.confusion)

#Sensibilit� 
(m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])

#Sensibilit� 
(m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])

#Sp�cificit� 
(m.confusion[1,1]) / (m.confusion[1,1]+m.confusion[2,1])


#ODs ratio
exp(cbind(coef(model_quali), confint(model_quali)))
library(questionr)
odds.ratio(model_quali)
install.packages("GGally")
library(GGally)
library(broom.helpers)
ggcoef_model(model_quali, exponentiate = TRUE)

###############################################################
# La variable à prédire est « Winner » =  quanli = r logistique
# 
# Construire le meilleur modèle possible

###############################################################

# factorisation des varibales
data_select<-na.omit(base_joueur_match[(base_joueur_match$wins + base_joueur_match$losses>3 & base_joueur_match$Winner %in% c("Blue","Red")) ,
                                   c("Winner","age","weight_class","Stance","avg_HEAD_att","avg_TOTAL_STR_att","avg_BODY_att","avg_CLINCH_att","avg_DISTANCE_att","avg_GROUND_att","avg_KD","avg_SUB_ATT","Height_cms")])

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

data_select$Winner <-as.factor(data_select$Winner) # exclude=NULL
# unique(data_select$Winner)

library(MASS)

summary(data_select)
str(data_select)

# colnames(data_select) %in% c("age","weight_class","Stance")
data_select <- data_select[, !colnames(data_select) %in% c("age","weight_class","Stance")]

data_select <- na.omit(data_select)

simple.model <- glm(Winner ~1, data = data_select, family = binomial)
summary(simple.model)

full.model <- glm(Winner ~., data = data_select, family = binomial)
summary(full.model)

stepwise_aic <- stepAIC(simple.model, direction="both", scope=list(lower=simple.model, upper=full.model))

# best model
# Winner ~ avg_DISTANCE_att + avg_GROUND_att + Age2 + categorie_poids2 + Height_cms + avg_SUB_ATT
summary(stepwise_aic)

n = dim(data_select)[1]
stepwise_bic <- stepAIC(simple.model, direction="both", scope=list(lower=simple.model, upper=full.model),k=log(n))
summary(stepwise_bic)

# model 
model_quali<-glm( Winner ~ avg_DISTANCE_att + avg_GROUND_att + Age2 + categorie_poids2 + Height_cms + avg_SUB_ATT, data=data_select, family = binomial) # binomial
model_quali
summary(model_quali)
exp(coef(model_quali))

# model_quali$

appren.p <- cbind(data_select, predict(model_quali, newdata = data_select, type = "link",
                                    se = TRUE))

appren.p <- within(appren.p, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedProb > 0.5, 1, 0)))
colnames(appren.p)
appren.p<-appren.p[,c("Winner","avg_DISTANCE_att","avg_GROUND_att","fit","PredictedProb","pred.chd")]
(m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$Winner)))

#Taux de bien class�
(m.confusion[1,1]+m.confusion[2,2]) / sum(m.confusion)

#Sensibilit� 
(m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])

#Sensibilit� 
(m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])

#Sp�cificit� 
(m.confusion[1,1]) / (m.confusion[1,1]+m.confusion[2,1])


#ODs ratio
exp(cbind(coef(model_quali), confint(model_quali)))

library(questionr)
odds.ratio(model_quali)
install.packages("GGally")
library(GGally)
library(broom.helpers)
ggcoef_model(model_quali, exponentiate = TRUE)
