library(readr)
library(zoo)
library(ggplot2)
library(dplyr)
library(lmtest)
library(car)
#

# Lecture des données Excel
balance_sheet <- readxl::read_excel("C:/Users/DELL/OneDrive/Bureau/sd.xlsx")

# Supprimer les dernières colonnes qui contiennent rien
balance_sheet <- balance_sheet[, -c(29:34)]

# Générer les dates de 1996 à 2022
dates <- seq(as.Date("1996-12-31"), as.Date("2022-12-31"), by = "year")

# Convertir les données en numérique
Cash_Short_Term_Investments <- as.numeric(balance_sheet[2, ])
Cash_Short_Term_Investments <- na.omit(Cash_Short_Term_Investments)

Common_Stock_Addnl_Paid_in_Capital_incl_Option_Reserve <- as.numeric(balance_sheet[270, ])
Common_Stock_Addnl_Paid_in_Capital_incl_Option_Reserve <- na.omit(Common_Stock_Addnl_Paid_in_Capital_incl_Option_Reserve)

Total_Assets <- as.numeric(balance_sheet[169, ])
Total_Assets <- na.omit(Total_Assets)

Total_Liabilities <- as.numeric(balance_sheet[250, ])
Total_Liabilities <- na.omit(Total_Liabilities)



# Créer un dataframe avec les dates et les valeurs
data_df1 <- data.frame(Date = dates[1:length(Cash_Short_Term_Investments)], Value = Cash_Short_Term_Investments)
data_df2 <- data.frame(Date = dates[1:length(Common_Stock_Addnl_Paid_in_Capital_incl_Option_Reserve)], Value = Common_Stock_Addnl_Paid_in_Capital_incl_Option_Reserve)
data_df3 <- data.frame(Date = dates[1:length(Total_Assets)], Value = Total_Assets)
data_df4 <- data.frame(Date = dates[1:length(Total_Liabilities)], Value = Total_Liabilities )

y_max <- max(max(data_df1$Value), max(data_df2$Value), max(data_df3$Value), max(data_df4$Value))
# Tracer la courbe d'évolution pour le premier ensemble de données
plot(data_df1$Date, data_df1$Value, type = "l", xlab = "Année", ylab = "Valeurs", main = "Tendances des investissements à court terme, du capital social additionnel, des actifs totaux et des passifs totaux sur la période 1996 à 2022", cex.lab = 1, ylim = c(0, y_max * 1.1))

# Tracer le deuxième graphique dans la même fenêtre
lines(data_df2$Date, data_df2$Value, col = "red", type = "l")
lines(data_df3$Date, data_df3$Value, col = "blue", type = 'l')
lines(data_df4$Date, data_df4$Value, col = "brown", type = 'l')
legend("topleft", legend = c("Cash Short Term Investments", "Common Stock - Addnl Paid in Capital incl Option Reserve", "Total Assets", "Total Liabilities"), col = c("black", "red","blue", "brown"), lty = 1, lwd = 0.5, xpd = TRUE,xjust = 1, yjust = 1, bty = "n")

