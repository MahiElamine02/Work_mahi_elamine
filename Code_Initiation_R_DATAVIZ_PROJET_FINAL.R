######################Initiation à R & Dataviz
#Groupe 6 : DEKKAR Massyle & GUILBERT--ARRUGA Leo & GUENDOUZ Mahi Elamine
#
#
#####Exercice 1: Estimation du Modèle MEDAF sur plusieurs périodes
#
#Directory
setwd("C:/Users/DELL/OneDrive/Bureau/S1/INTRO A R ET DATAVIZ/projet_final")
#
#package
#
library(readr)
library(zoo)
library(ggplot2)
library(dplyr)
library(lmtest)
library(car)
#
#import
#
FF_6portfolios <- read.csv2("FF_6portfolios.csv", sep = ",")
FF_factors <- read.csv2("FF_factors.csv", sep = ",")
data <- merge(FF_6portfolios, FF_factors, by = "X") #fusion un seul df par date (col X : AAAAMM)
col <- c("SMALL.LoBM", "SMALL.HiBM", "BIG.LoBM", #colonne à convertir en numeric
         "BIG.HiBM", "ME1.BM2", "ME2.BM2",
         "Mkt.RF", "SMB", "HML", "RF") 
data[col] <- lapply(data[col], as.numeric) #conversion numeric
str(data) #verifier qu'on a que du numeric
summary(data) #stats descriptives de toutes les données
#
#Mkt-RF : le rendement du portefeuille de marché moins le rendement sans risque
#RF : le rendement sans risque
#Choix portefeuille ME1 BM2 
#
summary(data$ME1.BM2)
str(data$ME1.BM2)
rend_pf_ME1_BM2 <- data$ME1.BM2
#
#Rendement du portefeuille de marché = Mkt-RF + RF 
#
rend_pf_marche <- data$Mkt.RF + data$RF
RF <- data$RF
#
#Conversion dates mois
#
dates <- seq(as.Date("1963-07-01"),
             length.out = length(rend_pf_ME1_BM2),
             by = "1 month")
#
#Df final avec date, rendement de portefeuille et rendement du portefeuille du marché
#
FF_dataframe <- data.frame(Date = format(dates, "%m/%Y"),
                           rend_pf_ME1_BM2 = rend_pf_ME1_BM2, 
                           rend_pf_marche = rend_pf_marche,
                           RF = RF )
#
###Période 1 : juillet 1963 - décembre 2005
#période 1
#
FF_dataframe$Date <- as.yearmon(FF_dataframe$Date,
                                format = "%m/%Y")
donnees_periode_1 <- FF_dataframe[FF_dataframe$Date >= as.yearmon("07/1963", format = "%m/%Y") &
                                    FF_dataframe$Date <= as.yearmon("12/2005", format = "%m/%Y"), ]
summary(donnees_periode_1)
str(donnees_periode_1)
#
#graphiques rendements des portefeuilles et du marché periode 1
#
ggplot(donnees_periode_1, aes(x = Date)) +
  geom_line(aes(y = rend_pf_ME1_BM2, color = "Portefeuille")) +
  geom_line(aes(y = rend_pf_marche, color = "Marché")) +
  labs(title = "Rendements mensuels des portefeuilles et du marché",
       y = "Rendement",
       x = "Date") +
  scale_color_manual(values = c("Portefeuille" = "lightblue", "Marché" = "pink"))
#
ggplot(donnees_periode_1, aes(x = rend_pf_ME1_BM2, fill = "Portefeuille")) +
  geom_histogram(binwidth = 0.5, position = "identity", alpha = 0.7) +
  geom_histogram(data = donnees_periode_1, aes(x = rend_pf_marche, fill = "Marché"),
                 binwidth = 0.5, position = "identity", alpha = 0.7) +
  labs(title = "Distribution des rendements des portefeuilles et du marché",
       y = "Effectif",
       x = "Rendement") +
  scale_fill_manual(values = c("Portefeuille" = "lightblue", "Marché" = "pink"))
#
#Modele MEDAF periode 1
#
modele_medaf_1 <- lm(rend_pf_ME1_BM2 ~ rend_pf_marche + RF, data = donnees_periode_1)
summary(modele_medaf_1)
#
###Période 2 : juillet 1963 - septembre 2022 
#période 2
donnees_periode_2 <- FF_dataframe[FF_dataframe$Date >= as.yearmon("07/1963", format = "%m/%Y") &
                                    FF_dataframe$Date <= as.yearmon("09/2022", format = "%m/%Y"), ]
summary(donnees_periode_2)
str(donnees_periode_2)
#
#graphiques rendements des portefeuilles et du marché periode 2
#
ggplot(donnees_periode_2, aes(x = Date)) +
  geom_line(aes(y = rend_pf_ME1_BM2, color = "Portefeuille")) +
  geom_line(aes(y = rend_pf_marche, color = "Marché")) +
  labs(title = "Rendements mensuels des portefeuilles et du marché",
       y = "Rendement",
       x = "Date") +
  scale_color_manual(values = c("Portefeuille" = "lightblue", "Marché" = "pink"))
#
ggplot(donnees_periode_2, aes(x = rend_pf_ME1_BM2, fill = "Portefeuille")) +
  geom_histogram(binwidth = 0.5, position = "identity", alpha = 0.7) +
  geom_histogram(data = donnees_periode_2, aes(x = rend_pf_marche, fill = "Marché"),
                 binwidth = 0.5, position = "identity", alpha = 0.7) +
  labs(title = "Distribution des rendements des portefeuilles et du marché",
       y = "Effectif",
       x = "Rendement") +
  scale_fill_manual(values = c("Portefeuille" = "lightblue", "Marché" = "pink"))
#
#Modele MEDAF periode 2
#
modele_medaf_2 <- lm(rend_pf_ME1_BM2 ~ rend_pf_marche + RF, data = donnees_periode_2)
summary(modele_medaf_2)
#
###Période 3 : janvier 2006 - septembre 2022
#période 3
donnees_periode_3 <- FF_dataframe[FF_dataframe$Date >= as.yearmon("01/2006", format = "%m/%Y") &
                                    FF_dataframe$Date <= as.yearmon("09/2022", format = "%m/%Y"), ]
summary(donnees_periode_3)
str(donnees_periode_3)
#
#graphiques rendements des portefeuilles et du marché periode 3
#
ggplot(donnees_periode_3, aes(x = Date)) +
  geom_line(aes(y = rend_pf_ME1_BM2, color = "Portefeuille")) +
  geom_line(aes(y = rend_pf_marche, color = "Marché")) +
  labs(title = "Rendements mensuels des portefeuilles et du marché",
       y = "Rendement",
       x = "Date") +
  scale_color_manual(values = c("Portefeuille" = "lightblue", "Marché" = "pink"))
#
ggplot(donnees_periode_3, aes(x = rend_pf_ME1_BM2, fill = "Portefeuille")) +
  geom_histogram(binwidth = 0.5, position = "identity", alpha = 0.7) +
  geom_histogram(data = donnees_periode_3, aes(x = rend_pf_marche, fill = "Marché"),
                 binwidth = 0.5, position = "identity", alpha = 0.7) +
  labs(title = "Distribution des rendements des portefeuilles et du marché",
       y = "Effectif",
       x = "Rendement") +
  scale_fill_manual(values = c("Portefeuille" = "lightblue", "Marché" = "pink"))
#
#Modele MEDAF periode 3
#
modele_medaf_3 <- lm(rend_pf_ME1_BM2 ~ rend_pf_marche + RF, data = donnees_periode_3)
summary(modele_medaf_3)
#
#
#
#####Exercice 2:  Estimation du modèle Fama-French à 3 facteurs
#
#etape 1
#
reg1 <- lapply(col[2:7], function(pf) { #fonction sur 6 portefeuilles, col 2 a 7 de data
  lm(data[[pf]] ~ data$Mkt.RF + data$SMB + data$HML) #reg lineaire avec 3 facteurs
})
mat_coef <- matrix(nrow = length(col[2:7]), #creation matrice resultats
                   ncol = 4) #4 colonnes
for (i in 1:length(col[2:7])) { #boucle avec 6 portfeuilles
  mat_coef[i, ] <- coefficients(reg1[[i]]) #coef reg1 pour chaque i
}
mat_coef #matrice etape1 avec 6 portefeuilles, intercept et 3facteurs Mkt,SMB,HML
#
#etape 2
#
moy_rend <- colMeans(data[, col[2:7]]) #calcul rend moyen par portefeuilles
reg2 <- lm(moy_rend ~ mat_coef[, 2:4]) #reg lineaire avec moyennes et Mkt,SMB,HML
final <- as.numeric(coef(reg2))#coef numeric reg2 final
mat_esti <- matrix(final, ncol = 1, #nbr colonne
                   dimnames = list(c("Intercept", "Mkt.RF", #noms lignes, colonnes
                                     "SMB", "HML"),"Coefficients"))
mat_esti #matrice avec intercept et les trois estimateurs finaux
#
#
#
#####Exercice 3: Tabagisme et âge
#
#import
#
data_tabac <- read.csv2("2_tabac.csv", sep = ",") #import df
str(data_tabac) #structure df
length(data_tabac$ID) #nbr individu, 100 ici
which(is.na(data_tabac)) #donne indice des NA, aucun ici
#
#convertir var quali factor
#
data_tabac$sexe <- factor(data_tabac$sexe)
data_tabac$situation <- factor(data_tabac$situation)
data_tabac$tabagisme_passif <- factor(data_tabac$tabagisme_passif)
data_tabac$probleme_pulmonaire <- factor(data_tabac$probleme_pulmonaire)
#
###Question 1
#
#stat global sans ID
#
summary(data_tabac[,-1])
#
#graph genre
#
ggplot(data_tabac, aes(x = sexe, fill = sexe)) + #var, remplir avec var
  geom_bar() + #histogramme barre
  scale_fill_manual(values = c("homme" = "lightblue", "femme" = "pink")) + #couleur genre
  geom_text(stat = "count", #effectif
            aes(label = ..count..), #afficher effcetif dans la barre
            position = position_stack(vjust = 0.5)) + #position milieu barre
  labs(title = "Répartition Homme/Femme", #titre
       x = "Genre", y = "Effectif") #noms axes
#
#graph situation conjugale
#
ggplot(data_tabac, aes(x = situation)) +
  geom_bar(fill = "orange") +
  geom_text(stat = "count",
            aes(label = ..count..),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition des situations conjugales",
       x = "Situation", y = "Effectif")
#
#graph tabagisme passif
#
ggplot(data_tabac, aes(x = tabagisme_passif)) +
  geom_bar(fill = "orange") +
  geom_text(stat = "count",
            aes(label = ..count..),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Exposition au Tabagisme Passif",
       x = "Exposition", y = "Effectif")
#
#graph problèmes pulmonaires
#
ggplot(data_tabac, aes(x = probleme_pulmonaire)) +
  geom_bar(fill = "orange") +
  geom_text(stat = "count",
            aes(label = ..count..),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Présence de problèmes pulmonaires",
       x = "Problèmes pulmonaires", y = "Effectif")
#
#graph age et genre
#
ggplot(data_tabac, aes(x = sexe, y = age, fill = sexe)) +
  geom_boxplot() +
  scale_fill_manual(values = c("homme" = "lightblue", "femme" = "pink")) +
  labs(title = "Répartition de l'âge selon le genre",
       x = "Genre", y = "Âge")
#
#graph tabac et genre
#
ggplot(data_tabac, aes(x = tabac, fill = sexe)) +
  geom_bar(position = position_dodge(width = 0.5), #barre cote a cote,width espace entre barre
           color = "black") +  #couleur contour barre
  scale_fill_manual(values = c("homme" = "lightblue", "femme" = "pink")) +
  labs(title = "Répartition du niveau de tabagisme selon le genre", 
       x = "Niveau de Tabagisme", y = "Effectif")
#
###Question 2
#
#creation modele lineaire
#variable dischotomique pour illustrer les variable qualitative ( donc =  0 ou 1 qui rpz 2 modalité ) et poyltomqiue
#mod1
mod1 <- lm( data_tabac$tabac ~ data_tabac$age +  data_tabac$probleme_pulmonaire +
              data_tabac$tabagisme_passif + data_tabac$sexe +
              data_tabac$situation )
summary (mod1)
vif(mod1)
#mod2
mod2 <- lm( data_tabac$tabac ~ data_tabac$age +
              data_tabac$probleme_pulmonaire +
              data_tabac$tabagisme_passif )
summary(mod2)
vif(mod2)









