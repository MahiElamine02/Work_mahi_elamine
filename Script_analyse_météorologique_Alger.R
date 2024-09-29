library(foreign)
library(lmtest)
library(readxl)
library(sandwich)
library(dynlm) 
library(lubridate) 
library(TSA) 
library(tidyverse)

#imporation des données 

TEMP_ALGER <- read_excel("C:/Users/DELL/OneDrive/Bureau/Projet_final_économetrie/ALGER.xlsx")

summary(TEMP_ALGER)

str(TEMP_ALGER)

# On remarque que il y a une colonne qui contient aucune information, on la supprime 

TEMP_ALGER <- TEMP_ALGER[,-5]

# On remplace les valeurs manquantes de chaque colonne par la moyenne 

### calcule des moyennes 

mean_TAVG <- mean(na.omit(TEMP_ALGER$`TAVG (Degrees Fahrenheit)`))
mean_TMAX <- mean(na.omit(TEMP_ALGER$`TMAX (Degrees Fahrenheit)`))
mean_TMIN <- mean(na.omit(TEMP_ALGER$`TMIN (Degrees Fahrenheit)`))

######### maintenant on va remplacer les valeurs manquantes par la moyenne 

TEMP_ALGER$`TAVG (Degrees Fahrenheit)`[is.na(TEMP_ALGER$`TAVG (Degrees Fahrenheit)`)] <- mean_TAVG
TEMP_ALGER$`TMAX (Degrees Fahrenheit)`[is.na(TEMP_ALGER$`TMAX (Degrees Fahrenheit)`)] <- mean_TMAX
TEMP_ALGER$`TMIN (Degrees Fahrenheit)`[is.na(TEMP_ALGER$`TMIN (Degrees Fahrenheit)`)] <- mean_TMIN

summary(TEMP_ALGER)


# Creation de "TDTR"

TDTR <- TEMP_ALGER$`TMAX (Degrees Fahrenheit)` - TEMP_ALGER$`TMIN (Degrees Fahrenheit)`

TEMP_ALGER <- TEMP_ALGER %>% mutate(TDTR)

summary(TEMP_ALGER)

# Assurez-vous que la colonne 'Date' est de type Date

TEMP_ALGER$Date <- as.Date(TEMP_ALGER$Date, format="%m-%d-%Y")

# Création de la série temporelle

inds <- seq(as.Date("1980-02-01"), as.Date("2023-08-11"), by = "day")

######### On crée notre série temporel

ALGER_TAVG <- ts(TEMP_ALGER$`TAVG (Degrees Fahrenheit)`, start = c(1980, as.numeric(format(inds[1], "%j"))), frequency = 365)

summary(ALGER_TAVG)

plot(ALGER_TAVG)

# Calculez et affichez la densité

d1 <- density(ALGER_TAVG)

plot(d1, col = "blue", main = "", sub = "Alger", xlab = "", ylab = "")

abline(h = 0, lwd = 2)

# Tendance linéaire

time.pts <- c(1:length(ALGER_TAVG))

plot(time.pts)

######################################################################

# Modèle de régression linéaire pour estimer la tendance

mod1 <- lm(ALGER_TAVG ~ time.pts)

summary(mod1)

coeftest(mod1, vcov = NeweyWest(mod1, lag = 18, prewhite = FALSE))

# Données estimées par le modèle linéaire
  
donnees_est <- fitted(mod1)
  
plot(donnees_est)

summary(donnees_est)

# Séries temporelle des données estimées

temp.fit.lm1 <- ts(donnees_est,
                   start = c(1980, as.numeric(format(inds[1], "%j"))),
                   frequency = 365)

plot(temp.fit.lm1)
summary(temp.fit.lm1)

plot(ALGER_TAVG, ylab = "", col = "red", sub = "Alger", main = "")
lines(temp.fit.lm1, lwd = 2, col = "blue")

# Decomposition de la série temporelle en tendance, saisonnalité et résidus

fit <- stl(ALGER_TAVG, s.window = "periodic")
plot(fit$time.series[, 2])
summary(fit$time.series[, 2])
dat <- decompose(ALGER_TAVG)
plot(dat)

detrended <- ALGER_TAVG - fit$time.series[, 2]
plot(fit$time.series[, 2])
plot(detrended)
summary(detrended)
# Assurez-vous que toutes les colonnes sont des séries temporelles

detrended <- ts(detrended, start = start(ALGER_TAVG), frequency = frequency(ALGER_TAVG))
TAVG <- ts(TEMP_ALGER$`TAVG (Degrees Fahrenheit)`, start(ALGER_TAVG), frequency = frequency(ALGER_TAVG))
TMAX <- ts(TEMP_ALGER$`TMAX (Degrees Fahrenheit)`, start(ALGER_TAVG), frequency = frequency(ALGER_TAVG))
TMIN <- ts(TEMP_ALGER$`TMIN (Degrees Fahrenheit)`, start(ALGER_TAVG), frequency = frequency(ALGER_TAVG))

#########################################################


# Modèle 8
mod8 <- dynlm(detrended ~ TAVG + TDTR)

# Afficher le résumé du modèle
summary(mod8)

# Estimation de la variance robuste avec Newey-West
coeftest(mod8, vcov = NeweyWest(mod8, lag = 18, prewhite = FALSE))

# Données estimées par le modèle mod8
donnees_est_8 <- fitted(mod8)
plot(donnees_est_8)

summary(donnees_est_8)
# Séries temporelle des données estimées
temp.fit.mod8 <- ts(donnees_est_8,
                    start = c(1980, as.numeric(format(inds[1], "%j"))),
                    frequency = 365)

# Visualisation de la série temporelle des données estimées
plot(temp.fit.mod8)
summary(temp.fit.mod8)
# Superposition sur la série temporelle originale
plot(detrended, ylab = "", sub = "Alger", col = "blue")
lines(temp.fit.mod8, lwd = 2, col = "green")

###############################################################

# Statistiques des températures

summary_temp_avg <- summary(TEMP_ALGER$`TAVG (Degrees Fahrenheit)`)
summary_temp_max <- summary(TEMP_ALGER$`TMAX (Degrees Fahrenheit)`)
summary_temp_min <- summary(TEMP_ALGER$`TMIN (Degrees Fahrenheit)`)
summary_temp_dif <- summary(TEMP_ALGER$TDTR)

# Affichage des résultats

summary_temp_avg
summary_temp_max
summary_temp_min
summary_temp_dif

library(ggplot2)

# Histogramme des températures moyennes

ggplot(TEMP_ALGER, aes(x = `TAVG (Degrees Fahrenheit)`)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogramme des Températures Moyennes", x = "Température Moyenne (°F)", y = "Fréquence")

# Historgramme des températures diurne

ggplot(TEMP_ALGER, aes(x = TDTR)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogramme des températures diurne ", x = "Température diurne (°F)", y = "Fréquence")

# Diagramme de dispersion entre TMAX et TMIN

ggplot(TEMP_ALGER, aes(x = `TMAX (Degrees Fahrenheit)`, y = `TMIN (Degrees Fahrenheit)`)) +
  geom_point(color = "green", alpha = 0.7) +
  labs(title = "Diagramme de Dispersion entre TMAX et TMIN", x = "Température Maximale (°F)", y = "Température Minimale (°F)")

library(dplyr)

# Conversion de la colonne "Date" en format Date :
TEMP_ALGER$Date <- as.Date(TEMP_ALGER$Date)

# Création d'une nouvelle colonne "Month" :
TEMP_ALGER <- TEMP_ALGER %>%
  mutate(Month = as.Date(format(Date, "%Y-%m-%d")))

# Calcul des statistiques mensuelles :
monthly_stats <- TEMP_ALGER %>%
  group_by(Month) %>%
  summarize(
    Avg_Temperature = mean(`TAVG (Degrees Fahrenheit)`, na.rm = TRUE),
    Max_Temperature = max(`TMAX (Degrees Fahrenheit)`, na.rm = TRUE),
    Min_Temperature = min(`TMIN (Degrees Fahrenheit)`, na.rm = TRUE),
    diurine_temp = mean(TDTR, na.rm = TRUE)
  )

# Afficher les résultats
print(monthly_stats)

# Tracer le graphique de la température moyenne mensuelle
library(ggplot2)

ggplot(monthly_stats, aes(x = Month, y = Avg_Temperature)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Température Moyenne Mensuelle à Alger", x = "Mois", y = "Température Moyenne (°F)")


# Graphique de la température maximale et minimale mensuelle

ggplot(monthly_stats, aes(x = Month)) +
  geom_line(aes(y = Max_Temperature), color = "red", linetype = "dashed", size = 1) +
  geom_line(aes(y = Min_Temperature), color = "green", linetype = "dashed", size = 1) +
  geom_point(aes(y = Max_Temperature), color = "red") +
  geom_point(aes(y = Min_Temperature), color = "green") +
  labs(title = "Température Maximale et Minimale Mensuelle à Alger", x = "Mois", y = "Température (°F)") +
  scale_y_continuous(limits = c(0, 120))  # Ajuster les limites de l'axe y selon vos besoins

# Graphique de la température diurine mensuelle

ggplot(monthly_stats, aes(x = Month, y = diurine_temp)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Température diurine Mensuelle à Alger", x = "Mois", y = "Température Moyenne (°F)")


##Visualisation des Températures au fil des années
# Créer une colonne 'Year' pour extraire l'année

# Conversion de la colonne "Date" en format Date :
TEMP_ALGER$Date <- as.Date(TEMP_ALGER$Date)

# Ajout de la colonne "Year" :
TEMP_ALGER <- TEMP_ALGER %>%
  mutate(Year = format(Date, "%Y"))

# Calculer la température moyenne annuelle
annual_avg_temp <- TEMP_ALGER %>%
  group_by(Year) %>%
  summarize(Avg_Temperature = mean(`TAVG (Degrees Fahrenheit)`, na.rm = TRUE))

# Graphique de l'évolution de la température moyenne au fil des ans
library(ggplot2)

ggplot(annual_avg_temp, aes(x = as.numeric(Year), y = Avg_Temperature)) +
  geom_line(color = "orange", size = 1) +
  geom_point(color = "orange", size = 3) +
  labs(title = "Évolution de la Température Moyenne Annuelle à Alger", x = "Année", y = "Température Moyenne (°F)")


##Analyse des Températures Maximales en Été
# Filtrer les données pour l'été (par exemple, juin, juillet, août)

library(lubridate)

summer_data <- TEMP_ALGER %>%
  filter(month(Date) %in% c(6, 7, 8))

# Identifier les années avec les températures maximales les plus élevées en été
hottest_summer_years <- summer_data %>%
  group_by(Year) %>%
  summarize(Max_Temperature = max(`TMAX (Degrees Fahrenheit)`, na.rm = TRUE)) %>%
  filter(Max_Temperature == max(Max_Temperature))

# Afficher les années avec les températures maximales les plus élevées en été
print(hottest_summer_years)


library(dplyr)
library(ggplot2)

# Ajout de la colonne "Year" :
TEMP_ALGER <- TEMP_ALGER %>%
  mutate(Year = year(Date))

# Filtrer les années 1988 et 2020
selected_years_data <- TEMP_ALGER %>%
  filter(Year %in% c(1988, 2023))

# Graphique comparatif des températures quotidiennes entre deux années
ggplot(selected_years_data, aes(x = Date, y = `TAVG (Degrees Fahrenheit)`, color = as.factor(Year))) +
  geom_line(size = 1) +
  labs(title = "Comparaison des Températures Quotidiennes entre 1988 et 2023", x = "Date", y = "Température Moyenne (°F)", color = "Année") +
  scale_color_manual(values = c("1988" = "red", "2023" = "green"))


#Analyse : Tendances Temporelles des Températures
# Calculer les tendances temporelles des températures moyennes
trend_model <- lm(`TAVG (Degrees Fahrenheit)` ~ Date, data = TEMP_ALGER)

# Afficher le résumé du modèle de tendance
summary(trend_model)

# Visualisation de la tendance temporelle
ggplot(TEMP_ALGER, aes(x = Date, y = `TAVG (Degrees Fahrenheit)`)) +
  geom_line(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(title = "Tendance Temporelle des Températures Moyennes", x = "Date", y = "Température Moyenne (°F)")



#Analyse : Corrélation entre les Températures Maximales et Minimales
# Calculer la corrélation entre les températures maximales et minimales
correlation <- cor(TEMP_ALGER$`TMAX (Degrees Fahrenheit)`, TEMP_ALGER$`TMIN (Degrees Fahrenheit)`, use = "complete.obs")

# Afficher la corrélation
cat("Corrélation entre les températures maximales et minimales :", round(correlation, 2))

#Visualisation en boîte à moustaches des températures moyennes par mois
ggplot(TEMP_ALGER, aes(x = factor(month(Date, label = TRUE)), y = `TAVG (Degrees Fahrenheit)`)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution des Températures Moyennes par Mois", x = "Mois", y = "Température Moyenne (°F)")

# Visualisation de la carte de chaleur des températures moyennes par année
ggplot(TEMP_ALGER, aes(x = factor(Year), y = factor(month(Date, label = TRUE)), fill = `TAVG (Degrees Fahrenheit)`)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  labs(title = "Carte de Chaleur des Températures Moyennes par Année", x = "Année", y = "Mois", fill = "Température Moyenne (°F)")
