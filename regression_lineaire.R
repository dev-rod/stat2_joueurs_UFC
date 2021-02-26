##################################################################################
# 1 - Import des données 
##################################################################################
library(data.table)
library(lubridate)
library(car)
library(compute.es)
library(effects)
library(ggplot2)
library(multcomp)
library(pastecs)
library(WRS)
library(tidyverse)
library(leaps)
# Importer la base de données 
# data.csv
data <- read.csv("data/data.csv", sep = ",")

# Pensez à bien vérifier le format de vos champs !!!
# Pensez à regarder si des erreurs de saisie / valeurs aberrantes sont pr�sentes
summary(data)
str(data)

data$date<-as.Date(data$date)
data<-data[year(data$date)>2000,]
##################################################################################
# 2 - Création d'une table de combattant unique
##################################################################################

# Etape 1 : faire un dataframe avec toutes les variables concernant le joueur bleu + Date + Gagnant + Catégorie de poids + Nombre de round
nom_colonne<-colnames(data)
col_rouge<-nom_colonne[nom_colonne %like% "^R_"]
col_bleu<-nom_colonne[nom_colonne %like% "^B_"]

data_rouge<-data[ ,c("date","Winner","title_bout","weight_class","no_of_rounds", col_rouge) ]
data_bleu<-data[ ,c("date","Winner","title_bout","weight_class","no_of_rounds", col_bleu) ]



colnames(data_rouge)<-c("date","Winner","title_bout","weight_class","no_of_rounds",substr(col_rouge,3,1000) )
colnames(data_bleu)<-c("date","Winner","title_bout","weight_class","no_of_rounds",substr(col_bleu,3,1000) )


base_joueur_match<-rbind(data_rouge,data_bleu)

date_max<-aggregate(date ~ fighter , data = base_joueur_match , max )

joueur_unique <- merge(base_joueur_match,date_max, 
                       by.x = c("date","fighter") ,by.y = c("date","fighter"),
                       all.x = F , all.y = F)


doublon<-data.frame(table(joueur_unique$fighter))
doublon[doublon$Freq>1,]

# Etape 2 : faire un dataframe avec toutes les variables concernant le joueur rouge + Date + Gagnant + Catégorie de poids + Nombre de round
# Etape 3 : Renommer les variables pour que les noms de colonnes soit identiques entre les deux dataframe créés ci-dessus 
# Etape 4 : Concatener les deux dataframes en un seul
# Etape 5 : Sélectionner seulement la ligne correspondant au dernier combat par combattant

##################################################################################
# 3 - Calculer la régression linéaire simple entre le poids et la taille 
##################################################################################
attach(joueur_unique)

data_taille_poids <- na.omit(joueur_unique[,c("Weight_lbs","Height_cms")])
summary(data_taille_poids)
# A - Analyse graphique
plot(data_taille_poids$Weight_lbs,data_taille_poids$Height_cms)
table(data_taille_poids$Height_cms)
# B - Construction du mod�le 
str(data_taille_poids)
res.lm<-lm( Weight_lbs~Height_cms  , data = data_taille_poids)
?lm
summary(res.lm)

plot(Weight_lbs~Height_cms , data =data_taille_poids ,pch=16)
abline(res.lm,col="red",lwd=2)

# B1 - Estimation des param�tres (m�thode des moindres carr�s)
# B2 - Test global du mod�le (test F) / tests de nullit� descoefficients
# B3 - Qualit� du mod�le (coefficient R�)
ecart<-cbind(data_taille_poids,res.lm$fitted.values,res.lm$residuals,rstud)
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


# D - Pr�diction 
# Quel serait la taille d'une personne pesant 135 lbs
hist(taille_poid$Height_cms)
new <- data.frame(Height_cms = seq(160, 200, 5))

yy <- cbind(new,predict(res.lm, new, interval="prediction"))

##################################################################################
# 4- Calculer la r�gression lin�aire multiple entre le ratio de victoire et 
# Le nombre de coup � la t�te / le nombre de coup au corp / le nombre de coup au sol 
##################################################################################
# A - Analyse graphique

joueur_unique$ratio_victoire<-joueur_unique$wins/(joueur_unique$wins + joueur_unique$losses)
hist(joueur_unique$ratio_victoire)

#On garde seulement les combattants avec plus de 3 matchs
base_reg<-joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) & joueur_unique$weight_class == "Welterweight", c("fighter","weight_class","ratio_victoire","avg_BODY_att","avg_HEAD_att","avg_GROUND_att") ]
hist(base_reg$ratio_victoire)
table(joueur_unique$wins + joueur_unique$losses)
summary(base_reg)
str(base_reg)

#Analyse graphique
plot(base_reg)
cor(base_reg$ratio_victoire , base_reg$avg_BODY_att)
cor(base_reg$ratio_victoire , base_reg$avg_HEAD_att)
cor(base_reg$ratio_victoire , base_reg$avg_GROUND_att)
cor(base_reg$avg_GROUND_att , base_reg$avg_HEAD_att)

# B - Construction du mod�le 
# B1 - Estimation des param�tres (m�thode des moindres carr�s)
# B2 - Test global du mod�le (test F) / tests de nullit� descoefficients
# B3 - Qualit� du mod�le (coefficient R�)
res.lm <- lm(ratio_victoire~avg_GROUND_att+avg_HEAD_att+avg_BODY_att, data= base_reg)
summary(res.lm)

plot(Weight_lbs~Height_cms , data =data_taille_poids ,pch=16)
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


############################################################################
#############ANOVA entre nombre de coup tent� et cat�gorie de poids retravailler
############################################################################

#Analyse de la variable weight_class
data.frame(table(joueur_unique$weight_class))
aggregate(avg_TOTAL_STR_att~weight_class, data = joueur_unique, mean)
hist(joueur_unique$avg_TOTAL_STR_att)
boxplot(formula=avg_TOTAL_STR_att~weight_class, data=joueur_unique, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")


#Premier mod�le sans recodage des modalit�s
mod.lm=lm(formula=avg_TOTAL_STR_att~weight_class,data=joueur_unique)
anova(mod.lm)
summary(mod.lm)

#Regroupement des classes de poids
data_anova<-joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) &!is.na(joueur_unique$avg_TOTAL_STR_att),c("fighter","avg_TOTAL_STR_att","weight_class","avg_HEAD_att")]
data_anova[which(data_anova$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_anova[which(data_anova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"
hist(data_anova$avg_TOTAL_STR_att)


#Analyse graphique
boxplot(formula=avg_TOTAL_STR_att~categorie_poids2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")

#Cr�ation du mod�le
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2,data=data_anova)
anova(mod.lm)
summary(mod.lm)

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


############################################################################
#############ANOVA � deux facteurs entre nombre de coup tent� et cat�gorie de poids retravailler + Stance
############################################################################
#Analyse stance
table(joueur_unique$Stance)
aggregate(avg_TOTAL_STR_att~Stance, data = joueur_unique, mean)
boxplot(formula=avg_TOTAL_STR_att~Stance, data=joueur_unique, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")
#Analyse weight_class
aggregate(avg_TOTAL_STR_att~weight_class, data = joueur_unique, mean)
hist(joueur_unique$avg_TOTAL_STR_att)
boxplot(formula=avg_TOTAL_STR_att~weight_class, data=joueur_unique, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")


data_anova<-joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) &!is.na(joueur_unique$avg_TOTAL_STR_att),c("fighter","avg_TOTAL_STR_att","weight_class","avg_HEAD_att","Stance")]
data_anova[which(data_anova$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_anova[which(data_anova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"

data_anova[which(data_anova$Stance %in% c("Open Stance","Sideways","Switch","")),"Stance2"]<-"autres"
data_anova[which(data_anova$Stance %in% c("Orthodox")),"Stance2"]<-"Orthodox"
data_anova[which(data_anova$Stance %in% c("Southpaw")),"Stance2"]<-"Southpaw"

table(data_anova$Stance2)
aggregate(avg_TOTAL_STR_att~Stance2, data = data_anova, mean)
boxplot(formula=avg_TOTAL_STR_att~Stance2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")


#Analyse graphique
boxplot(formula=avg_TOTAL_STR_att~categorie_poids2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")
boxplot(formula=avg_TOTAL_STR_att~Stance, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")

#Chnagement de levels
str(data_anova)
data_anova$categorie_poids2<-as.factor(data_anova$categorie_poids2)
levels(data_anova$categorie_poids2)<-c("")
levels.default(data_anova$categorie_poids2)

# Via lm
#Mod�le sans int�raction
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2+Stance2,data=data_anova)
summary(mod.lm)
anova(mod.lm)
#Mod�le avec int�raction
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2*Stance2,data=data_anova)
summary(mod.lm)
anova(mod.lm)

# Mod�le seulement avec int�raction 
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2:Stance2,data=data_anova)
summary(mod.lm)
anova(mod.lm)

model1 <- aov(avg_TOTAL_STR_att~categorie_poids2+altitude+canopy+height)

############################################################################
#############Comparaison ancova VS reg lin�aire en fonction des cat�gorie de poids 
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
############# Regression logistique - Issue du match en fonction du style
############################################################################

data <- read.csv("C:/Users/ejosse/OneDrive - Business & Decision/Enseignement/Master MEDAS/2ieme ann�e/2020-2021/Pour �tudiant/Data/data.csv",sep = ",")
data$B_wins
# Pensez � bien v�rifier le format de vos champs !!!
# Pensez � regarder si des erreurs de saisie / valeurs aberrantes sont pr�sentes
#Selection des variables & ann�e
table(data$Winner)
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
