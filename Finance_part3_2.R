library(readr)
library(zoo)
library(ggplot2)
library(dplyr)
library(lmtest)
library(car)
#

# Lecture des données Excel
sheet_2 <- readxl::read_excel("C:/Users/DELL/OneDrive/Bureau/sd.xlsx")

# Générer les dates de 1996 à 2022
dates <- seq(as.Date("1999-12-31"), as.Date("2022-12-31"), by = "year")

Dividends_Paid_Cash_Total_Cash_Flow <- as.numeric(sheet_2[76, ])
Dividends_Paid_Cash_Total_Cash_Flow <- na.omit(Dividends_Paid_Cash_Total_Cash_Flow)
Dividends_Paid_Cash_Total_Cash_Flow <- rev(Dividends_Paid_Cash_Total_Cash_Flow)

Common_Stock_Buyback_Net <- as.numeric(sheet_2[134, ])
Common_Stock_Buyback_Net <- na.omit(Common_Stock_Buyback_Net)
Common_Stock_Buyback_Net <- rev(Common_Stock_Buyback_Net)

Dividends_Provided_Paid_Common <- as.numeric(sheet_2[139, ])
Dividends_Provided_Paid_Common <- na.omit(Dividends_Provided_Paid_Common)
Dividends_Provided_Paid_Common <- rev(Dividends_Provided_Paid_Common)

data_df1 <- data.frame(Date = dates[1:length(Dividends_Paid_Cash_Total_Cash_Flow)], Value = Dividends_Paid_Cash_Total_Cash_Flow)
data_df2 <- data.frame(Date = dates[1:length(Common_Stock_Buyback_Net)], Value = Common_Stock_Buyback_Net)
data_df3 <- data.frame(Date = dates[1:length(Dividends_Provided_Paid_Common)], Value = Dividends_Provided_Paid_Common)

y_max <- max(max(data_df1$Value), max(data_df2$Value), max(data_df3$Value))

plot(data_df1$Date, data_df1$Value, type = "l", xlab = "Année", ylab = "Valeurs", main = "Évolution des flux de trésorerie et des rachats d'actions sur la période 1999 à 2022", cex.lab = 1, ylim = c(0, y_max * 1.1))

lines(data_df2$Date, data_df2$Value, col = "red", type = "l")
lines(data_df3$Date, data_df3$Value, col = "blue", type = 'l')
legend("topleft", legend = c("Dividends Paid Cash Total Cash Flow", "Common Stock Buyback Net", "Dividends Provided Paid Common"), col = c("black", "red","blue"), lty = 1, lwd = 0.5, xpd = TRUE,xjust = 1, yjust = 1, bty = "n")