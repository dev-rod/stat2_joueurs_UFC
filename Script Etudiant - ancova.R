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