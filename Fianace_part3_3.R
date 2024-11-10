library(readr)
library(zoo)
library(ggplot2)
library(dplyr)
library(lmtest)
library(car)
#

# Lecture des données Excel
sheet_3 <- readxl::read_excel("C:/Users/DELL/OneDrive/Bureau/sd.xlsx")

# Supprimer les dernières colonnes qui contiennent rien
sheet_3 <- sheet_3[, -c(29:42)]

# Générer les dates de 1996 à 2022
dates <- seq(as.Date("1996-12-31"), as.Date("2022-12-31"), by = "year")

Revenue_from_Business_Activities_Total <- as.numeric(sheet_3[24,])
Revenue_from_Business_Activities_Total <- na.omit(Revenue_from_Business_Activities_Total)
Revenue_from_Business_Activities_Total <- rev(Revenue_from_Business_Activities_Total)

Income_Available_to_Common_Shares <- as.numeric(sheet_3[180,])
Income_Available_to_Common_Shares <- na.omit(Income_Available_to_Common_Shares)
Income_Available_to_Common_Shares <- rev(Income_Available_to_Common_Shares)

Derivative_Hedging_Gain_Loss_Total_Supplemental <- as.numeric(sheet_3[358,])
Derivative_Hedging_Gain_Loss_Total_Supplemental <- na.omit(Derivative_Hedging_Gain_Loss_Total_Supplemental)
Derivative_Hedging_Gain_Loss_Total_Supplemental <- rev(Derivative_Hedging_Gain_Loss_Total_Supplemental)

data_df1 <- data.frame(Date = dates[1:length(Revenue_from_Business_Activities_Total)], Value = Revenue_from_Business_Activities_Total)
data_df2 <- data.frame(Date = dates[1:length(Income_Available_to_Common_Shares)], Value = Income_Available_to_Common_Shares)
data_df3 <- data.frame(Date = dates[1:length(Derivative_Hedging_Gain_Loss_Total_Supplemental)], Value = Derivative_Hedging_Gain_Loss_Total_Supplemental)

y_max <- max(max(data_df1$Value), max(data_df2$Value), max(data_df3$Value))

plot(data_df1$Date, data_df1$Value, type = "l", xlab = "Année", ylab = "Valeurs", main = "Tendances des revenus d'activités commerciales et du revenu disponible pour les actionnaires ordinaires sur la période 1996 à 2022", cex.lab = 1, ylim = c(0, y_max * 1.1))

lines(data_df2$Date, data_df2$Value, col = "red", type = "l")
lines(data_df3$Date, data_df3$Value, col = "blue", type = 'l')
legend("topleft", legend = c("Revenue from Business Activities Total", "Income Available to Common Shares", "Derivative Hedging Gain Loss Total Supplemental"), col = c("black", "red","blue"), lty = 1, lwd = 0.5, xpd = TRUE,xjust = 1, yjust = 1, bty = "n")