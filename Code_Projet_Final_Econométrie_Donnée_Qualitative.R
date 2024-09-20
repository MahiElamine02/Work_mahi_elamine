setwd("C:/Users/lysah/OneDrive/Desktop")


gc()
rm(list=ls())


library(readr)
library(haven)
library(questionr)
library(PerformanceAnalytics)
library(gridExtra)
library(gtable)

data_etud_sup <- read_dta("3_Etudes_sup.dta")
View(data_etud_sup)

#Variables :
#Ensemble de données hypothétiques comportant l’accès aux études supérieures :
#Apply : avec des niveaux 0=«improbable», 1=«peu probable» et 2=«très probable»
######Pared : qui est une variable 0/1 indiquant si au moins un parent a un diplôme d'études supérieures
######Public : qui est une variable 0/1 où 1 indique que l'établissement de premier cycle est public et 0 privé
#gpa : qui est la moyenne pondérée de l'étudiant.

nb_ligne = nrow(data_etud_sup)
# 400 lignes
nb_colonne = ncol(data_etud_sup)
# 4 colonnes (variables)
name_colonne = names(data_etud_sup)

summary(data_etud_sup)
#Pared : 15.75% des étudiants, un de leur parent a un diplôme d'études supérieurs
#Public : 14.25% des étudiants fréquentent des établissements de premier cycle public
#gpa : la moyenne des moyennes pondérée des étudiant = 2.999

attach(data_etud_sup)
questionr::freq(apply)
# 55% des étudiants (n = 200), il est improbable qui postulent à des études supérieures 
# 35% des étudiants (n = 140), il est peu probable qui postulent à des études supérieures
# 10% des étudiants (n = 40), il est trés probable qui postulent à des études supérieures

questionr::freq(pared)
# 84.2% des étudiants (n = 337), leurs parents n'ont pas fait d'études supérieurs
# 15.8% des étudiants (n = 63), au moins un de leurs parents a fait des études supérieurs

questionr::freq(public)
# 85.8% des étudiants (n = 343), fréquentent des établissements de premier cycle privé
# 14.2% des étudiants (n = 57), fréquentent des établissements de premier cycle public

#Relation entre les candidatures et le niveau d'étude des parents
rel_apply_pared = table(apply, pared)
rel_apply_pared
# 200 étudiants improbable qui postulent et que leurs parents n'ont pas fait des études supérieures
# 20 étudiants improbable qui postulent et un de leurs parents a fait des études supérieures (au moins)
# 110 étudiants peu probable qui postulent, leurs parents n'ont pas fait d'étude
# 30 étudiants peu probable qui postulent, au moins un de leurs parents a fait des études 
# 27 étudiants trés probable qui postulent, leurs parents n'ont pas fait d'études supérieures #########
# 13 étudiants trés probable qui postulent, au moins un de leurs parents a fait des études supérieures

rel_apply_public = table(apply, public)
rel_apply_public
# 189 étudiants improbable qui postulent pour des études supérieures, ils fréquentent des établissements privés
# 31 étudiants improbable qui postulent pour des études supérieures, ils fréquentent des établissements publics
# 124 étudiants peu probable qui postulent pour des études supérieures, ils fréquentent des établissements privés
# 16 étudiants peu probable qui postulent pour des études supérieures, ils fréquentent des établissements publics
# 30 étudiants trés probable qui postulent pour des études supérieures, ils fréquentent des établissements privés
# 10 étudiants trés probable qui postulent pour des études suérieures, ils fréquentent des établissements publics

cor(apply, pared)
cor(apply, public)

#################################           
##########################
#################
##########                                        A AJOUTER
data_etud_sup$parent_non_etudesup <- 1
data_etud_sup$parent_non_etudesup[data_etud_sup$pared==1] <- 0
attach(data_etud_sup)

data_etud_sup$etudiant_etab_prive <- 1
data_etud_sup$etudiant_etab_prive[data_etud_sup$public==1] <- 0




####
x = sum(parent_non_etudesup)
x
y = sum(pared)
y
## 337 étudiants leurs parents non pas fait d'études supérieures
## 63 étudiants que au min un de leurs parent a fait des études sup

# min gpa = 1.9 / max gpa = 4 / delta = 0.35

data_etud_sup$classe_gpa <- 1
data_etud_sup$classe_gpa[data_etud_sup$gpa > 2.25] <- 2
data_etud_sup$classe_gpa[data_etud_sup$gpa > 2.6] <- 3
data_etud_sup$classe_gpa[data_etud_sup$gpa > 2.95] <- 4
data_etud_sup$classe_gpa[data_etud_sup$gpa > 3.3] <- 5
data_etud_sup$classe_gpa[data_etud_sup$gpa > 3.65] <- 6

#################################################################################################################
#### N3: Nombre d'étudiant dont un des parent minimum a fait des études sup selon les classe moyenne
N3 = tapply(X=data_etud_sup$etudiant_etab_prive, INDEX = data_etud_sup$classe_gpa, FUN = sum)
#### N33 : Nombre d'étudiant selon les classe moyenne 
N33 = tapply(X=data_etud_sup$etudiant_etab_prive, INDEX = data_etud_sup$classe_gpa, FUN = length)
#### P3 : proportion des étudiants dont un des parent minimum a fait des études sup  selon les classes 
centre = c(2.075,2.425,2.775,3.125,3.475,3.825)

N33
P3 = N3/N33
P3
#Affichage du tableau
toto = cbind(centre,N3,N33,P3)
View(toto)
#Plot
plot(P3~centre,main="Prop. d'étudiants dont l'établissement du premier cycle est privé selon la classe gpa",
     xlab="gpa", ylab=expression(italic(~Pi)~(bold(x))))
cor(data_etud_sup$etudiant_etab_prive, data_etud_sup$classe_gpa, method = "pearson")
#################################################################################################################
#################################################################################################################
#### N4: Nombre d'étudiant dont parent pas études sup selon les classe moyenne
N4 = tapply(X=data_etud_sup$parent_non_etudesup, INDEX = data_etud_sup$classe_gpa, FUN = sum)
#### N33 : Nombre d'étudiant selon les classe moyenne 
N33 = tapply(X=data_etud_sup$parent_non_etudesup, INDEX = data_etud_sup$classe_gpa, FUN = length)
#### P4 : proportion des étudiants dont parent PAS études sup  selon les classes 

N33
P4 = N4/N33
P4
#Affichage du tableau
toto = cbind(centre,N4,N33,P4)
View(toto)
#Plot
plot(P4~centre,main="Prop. d'étudiants dont aucun des parents n'a fait des études sup selon la classe gpa",
     xlab="gpa", ylab=expression(italic(~Pi)~(bold(x))))
cor(data_etud_sup$parent_non_etudesup, data_etud_sup$classe_gpa, method = "pearson")

# Logit hiérarchiser
# Soit on regrouppe le 1:peu probable et 2:trés probable dans 1 et on laisse O
# Soit on regrouppe 1 et 0 et on laisse 2

print(data_etud_sup)
# Avant d'appliqué la fct logistique sur nos variables
# on a structuré la variable apply comme suit : 
# 0 --> unlikely
# 1 --> likely (qui regroupe les observations de "somewhat likely" et "very likely")


#Vérifier les valeurs actuelles de 'apply'
table(data_etud_sup$apply)

# Recode 'apply' en utilisant des opérations de base
data_etud_sup$apply <- ifelse(data_etud_sup$apply == 0, 0, 1)

# Vérifier le résultat de la transformation
table(data_etud_sup$apply)

# Afficher le résultat pour vérifier
questionr::freq(data_etud_sup$apply)
attach(data_etud_sup)


###################################################################
######## Modele 01 : ################### MODELE COMPLET sans interactions
############## s?lection du mod?le

Reg.logit_1 <- glm(apply~parent_non_etudesup+etudiant_etab_prive,family=binomial(link="logit"))
summary(Reg.logit_1)

anova(Reg.logit_1, test="Chisq")

### 2 - La qualit? du mod?le

# Calculs de taux d'erreurs et matrice de confusion

pred.proba.reg1 <- predict(Reg.logit_1,type="response",newdata = data_etud_sup)
print(pred.proba.reg1)
summary(pred.proba.reg1)

# table(pred.proba.reg2 > 0.21, apply)
pred.moda.reg1 <- factor(ifelse(pred.proba.reg1 > 0.43, 1,0))
print(pred.moda.reg1)

mc.reg1 <- table(apply, pred.moda.reg1)
class(mc.reg1)
print(mc.reg1)

#taux d'erreur
err.reg1 <- (mc.reg1[2,1]+mc.reg1[1,2])/sum(mc.reg1)
print(err.reg1)


#######################
### 4 - Odds ratios
#######################

summary(Reg.logit_1)

###################
#  Avec le package questionr
#install.packages("questionr")

library(questionr)
odds.ratio(Reg.logit_1)

#### 5 - Repr?sentation graphiques des Odds ratios
#################################################

Reg1.graph <- broom::tidy(Reg.logit_1, conf.int = TRUE, exponentiate = TRUE)
str(Reg1.graph)

# tracer les odds-ratios avec la fonciton ggplot de la library ggplot2

#install.packages("ggplot2")
library("ggplot2")
ggplot(Reg1.graph) + aes(x = estimate, y= term, xmin = conf.low, 
                         xmax = conf.high) + geom_vline(xintercept = 1) + 
  geom_errorbarh() + geom_point() + scale_x_log10()


#### 6 - Estimations des effets marginaux avec le package margins
#### 2 méthodes possibles 
#################################################################
library(reshape2)
library(margins)
Reg1.select.b <- glm(formula = apply~parent_non_etudesup+etudiant_etab_prive, family = binomial(link = "logit"), data = data_etud_sup)
(Reg1.marginal <- margins(Reg1.select.b))
summary(Reg1.marginal)

#ou 

library(mfx)
logitmfx(formula = apply~parent_non_etudesup+etudiant_etab_prive , data=data_etud_sup)

#visualisation
#install.packages("ggeffects")
library(ggeffects)
plot(ggeffect(Reg1.select.b))


###################################################################
######## Modele 02 : ################### MODELE COMPLET en integrant gpa interactions
############## s?lection du mod?le

#le modèle 
Reg.logit_2 <- glm(apply~parent_non_etudesup+etudiant_etab_prive+gpa,family=binomial(link="logit"))
summary(Reg.logit_2)

#table des déviances 
anova(Reg.logit_2, test="Chisq")

### La qualit? du mod?le

# Calculs de taux d'erreurs et matrice de confusion

pred.proba.reg2 <- predict(Reg.logit_2,type="response",newdata = data_etud_sup)
print(pred.proba.reg2)
summary(pred.proba.reg2)

# matrice de confusion

pred.moda.reg2 <- factor(ifelse(pred.proba.reg2 > 0.49, 1,0))
print(pred.moda.reg2)

mc.reg2 <- table(apply, pred.moda.reg2)
class(mc.reg2)
print(mc.reg2)

#taux d'erreur
err.reg2 <- (mc.reg2[2,1]+mc.reg2[1,2])/sum(mc.reg2)
print(err.reg2)


#######################
### 4 - Odds ratios
#######################
library(questionr)
odds.ratio(Reg.logit_2)

#### 5 - Représentation graphiques des Odds ratios
#################################################

Reg2.graph <- broom::tidy(Reg.logit_2, conf.int = TRUE, exponentiate = TRUE)
str(Reg2.graph)

# tracer les odds ratios avec la fonction ggplot de la library ggplot2

library("ggplot2")
ggplot(Reg2.graph) + aes(x = estimate, y= term, xmin = conf.low, 
                         xmax = conf.high) + geom_vline(xintercept = 1) + 
  geom_errorbarh() + geom_point() + scale_x_log10()


#### 6 - Estimations des effets marginaux avec le package margins
#################################################################
library(reshape2)
library(margins)
Reg2.select.b <- glm(formula = apply~parent_non_etudesup+etudiant_etab_prive, family = binomial(link = "logit"), data = data_etud_sup)
(Reg2.marginal <- margins(Reg2.select.b))
summary(Reg2.marginal)
#ou
#install.packages("mfx")
library(mfx)
logitmfx(formula = apply~parent_non_etudesup+etudiant_etab_prive+gpa , data=data_etud_sup)

#visualisation
#install.packages("ggeffects") afin de visualiser les effets marginaux 
library(ggeffects)
plot(ggeffect(Reg2.select.b))

##################################################################################

################### II - MODELE COMPLET AVEC INTERACTIONS : des VA crois?es

##################################################################################

Reg3.logit <- glm(apply~etudiant_etab_prive+(parent_non_etudesup*gpa),family = binomial(link = "logit"))
summary(Reg3.logit)

# pour exporter le tableau de r?sultats

library("broom")
Reg3.export <- tidy(Reg3.logit, conf.int = TRUE, exponentiate = TRUE)
print(Reg3.export)

file_path_2 <- "C:/Users/lysah/OneDrive/Desktop/Reg3_export.csv"

write.csv2(Reg3.export,file_path_2)

#qualit? du mod?le

anova(Reg3.logit, test="Chisq")

anova(Reg.logit_2,Reg3.logit, test="Chisq")

pred.proba.reg3 <- predict(Reg3.logit,type="response",newdata = data_etud_sup)
print(pred.proba.reg3)
summary(pred.proba.reg3)

pred.moda.reg3 <- factor(ifelse(pred.proba.reg3 > 0.49, 1,0))
print(pred.moda.reg3)

#matrice de confusion
mc.reg3 <- table(apply,pred.moda.reg3)
class(mc.reg3)
print(mc.reg3)

#taux d'erreur
err.reg3 <- (mc.reg3[2,1]+mc.reg3[1,2])/sum(mc.reg3)
print(err.reg3)

#comparaison des 2 mod?les sans et avec variables crois?es
anova(Reg.logit_2,Reg3.logit, test="Chisq")



