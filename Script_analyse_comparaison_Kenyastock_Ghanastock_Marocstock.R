#Exercice 01

# QST01: on va calculer les rendements simples des series

# on commence par importer nos donnees

install.packages("readxl")
library(readxl)
setwd("C:/Users/DELL/OneDrive/Bureau/S1/économetrie/économétrie")
FINAF <- read_excel("FINAF.xlsx")
View(FINAF)
str(FINAF)

Ghana <- as.numeric(FINAF$ghanastock)
Kenya <- as.numeric(FINAF$Kenyastock)
Maroc <- as.numeric(FINAF$marocstock)

# Dans cet partie, on va creer nos series temporelles (pays-stoch par date)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
install.packages("xts")
install.packages("zoo")
library(zoo)
library(xts)
Date <- as.Date(FINAF$Dates, Format = "%d%m%y")
Ghana_xts <- xts(Ghana, order.by = Date)
Kenya_xts <- xts(Kenya, order.by = Date)
Maroc_xts <- xts(Maroc, order.by = Date)

###Visualisation Ghana :

plot(Ghana_xts)
str(Ghana_xts)
summary(Ghana_xts)
length(Ghana_xts)

#Rendements simples Ghana

rendements_simples_Ghana <- diff(Ghana_xts) / lag(Ghana_xts)
rendements_simples_Ghana=rendements_simples_Ghana[-1]
date_max_Ghana <- index(rendements_simples_Ghana)[which.max(rendements_simples_Ghana)]
rendement_max_Ghana <- max(rendements_simples_Ghana)
date_min_Ghana <- index(rendements_simples_Ghana)[which.min(rendements_simples_Ghana)]
rendement_min_Ghana <- min(rendements_simples_Ghana)
cat("Date et rendement le plus élevé :", date_max_Ghana, rendement_max_Ghana, "\n")
cat("Date et rendement le plus bas :", date_min_Ghana, rendement_min_Ghana, "\n")
head(rendements_simples_Ghana, n=1822)
plot(rendements_simples_Ghana)

library(PerformanceAnalytics)





## logarithme des prix

log_Ghana <- log(Ghana_xts)
head(log_Ghana, n=1746)
head(Ghana_xts, n=1746)
plot(log_Ghana, main = "Évolution du Log(prix) de l'indice Ghana Stock Exchange",
     lwd = 2, type = "l", col = "black", ylab = "Prix")
plot(Ghana_xts, main = "Évolution du prix de l'indice Ghana Stock Exchange",
     lwd = 2, type = "l", col = "black", ylab = "Prix")

plot(rendements_simples_Ghana, main = "Évolution du rendement simple Ghana Stock Exchange",
     lwd = 2, type = "l", col = "black", ylab = "return")
abline(h=0)


#########Visualisation Kenya :

plot(Kenya_xts)
str(Kenya_xts)
summary(Kenya_xts)
length(Kenya_xts)

#Rendement simples Kenya 

rendements_simples_Kenya <- diff(Kenya_xts) / lag(Kenya_xts)
rendements_simples_Kenya <- rendements_simples_Kenya[-1]
date_max_Kenya <- index(rendements_simples_Kenya)[which.max(rendements_simples_Kenya)]
rendement_max_Kenya <- max(rendements_simples_Kenya)
date_min_Kenya <- index(rendements_simples_Kenya)[which.min(rendements_simples_Kenya)]
rendement_min_Kenya <- min(rendements_simples_Kenya)
cat("Date et rendement le plus élevé :", date_max_Kenya, rendement_max_Kenya, "\n")
cat("Date et rendement le plus bas :", date_min_Kenya, rendement_min_Kenya, "\n")
head(rendements_simples_Kenya, n=1725)
plot(rendements_simples_Kenya)

library(PerformanceAnalytics)





#################Visualisation Maroc :

plot(Maroc_xts)
str(Maroc_xts)
summary(Maroc_xts)
length(Kenya_xts)

#Rendement simples Maroc

rendements_simples_Maroc <- diff(Maroc_xts) / lag(Maroc_xts)
rendements_simples_Maroc <- rendements_simples_Maroc[-1]
date_max_Maroc <- index(rendements_simples_Maroc)[which.max(rendements_simples_Maroc)]
rendement_max_Maroc <- max(rendements_simples_Maroc)
date_min_Maroc <- index(rendements_simples_Maroc)[which.min(rendements_simples_Maroc)]
rendement_min_Maroc <- min(rendements_simples_Maroc)
cat("Date et rendement le plus élevé :", date_max_Maroc, rendement_max_Maroc, "\n")
cat("Date et rendement le plus bas :", date_min_Maroc, rendement_min_Maroc, "\n")
head(rendements_simples_Maroc, n=10)
plot(rendements_simples_Maroc)

library(PerformanceAnalytics)





### QUESTION 02 : Calcul des statistiques descriptives 
install.packages("fBasics")
library(fBasics)

basicStats(rendements_simples_Ghana)
basicStats(rendements_simples_Kenya)
basicStats(rendements_simples_Maroc)

##### QUESTION 04 :

d_Ghana <- density(rendements_simples_Ghana)
plot(d_Ghana, col="blue")

f= hist(rendements_simples_Ghana, prob=T, breaks = 100, add=TRUE)
m = curve (dnorm(x, mean(rendements_simples_Ghana), sd(rendements_simples_Ghana)), col="orange", add=TRUE, lwd=2)
lg0 = c("Densite rendement indice ghaneen", "estimation de la loi normale")
legend("topright", legend = lg0, lty= c(1,2), lwd = 2, col=c("blue","orange"), cex=0.4)

d_Ghana <- density(rendements_simples_Kenya)
plot(d_Ghana, col="brown")

f= hist(rendements_simples_Kenya, prob=T, breaks = 100, add=TRUE)
m = curve (dnorm(x, mean(rendements_simples_Kenya), sd(rendements_simples_Kenya)), col="pink", add=TRUE, lwd=2)
lg0 = c("Densite rendement indice kenyan", "estimation de la loi normale")
legend("topright", legend = lg0, lty= c(1,2), lwd = 2, col=c("brown","pink"), cex=0.4)


d_Ghana <- density(rendements_simples_Maroc)
plot(d_Ghana, col="red")

f= hist(rendements_simples_Maroc, prob=T, breaks = 100, add=TRUE)
m = curve (dnorm(x, mean(rendements_simples_Maroc), sd(rendements_simples_Maroc)), col="yellow", add=TRUE, lwd=2)
lg0 = c("Densite rendement indice marocain", "estimation de la loi normale")
legend("topright", legend = lg0, lty= c(1,2), lwd = 2, col=c("red","yellow"), cex=0.4)



######### TEST DE NORMALITE

normalTest(rendements_simples_Ghana, method="jb")


normalTest(rendements_simples_Kenya, method="jb")


normalTest(rendements_simples_Maroc, method="jb")


#### QUESTION 05:

#Rendement logarithmique Ghana

rendements_logarithmique_Ghana <- Return.calculate(Ghana_xts, method = "log")
rendements_logarithmique_Ghana <- rendements_logarithmique_Ghana[-1]
date_max_Ghana_log <- index(rendements_logarithmique_Ghana)[which.max(rendements_logarithmique_Ghana)]
rendement_max_Ghana_log <- max(rendements_logarithmique_Ghana)
date_min_Ghana_log <- index(rendements_logarithmique_Ghana)[which.min(rendements_logarithmique_Ghana)]
rendement_min_Ghana_log <- min(rendements_logarithmique_Ghana)
cat("Date et rendement le plus élevé :", date_max_Ghana_log, rendement_max_Ghana_log, "\n")
cat("Date et rendement le plus bas :", date_min_Ghana_log, rendement_min_Ghana_log, "\n")
head(rendements_logarithmique_Ghana, n=1746)
plot(rendements_logarithmique_Ghana)

#Rendement logarithmique Kenya 

rendements_logarithmique_Kenya <- Return.calculate(Kenya_xts, method = "log")
rendements_logarithmique_Kenya <- rendements_logarithmique_Kenya[-1]
date_max_Kenya_log <- index(rendements_logarithmique_Kenya)[which.max(rendements_logarithmique_Kenya)]
rendement_max_Kenya_log <- max(rendements_logarithmique_Kenya)
date_min_Kenya_log <- index(rendements_logarithmique_Kenya)[which.min(rendements_logarithmique_Kenya)]
rendement_min_Kenya_log <- min(rendements_logarithmique_Kenya)
cat("Date et rendement le plus élevé :", date_max_Kenya_log, rendement_max_Kenya_log, "\n")
cat("Date et rendement le plus bas :", date_min_Kenya_log, rendement_min_Kenya_log, "\n")
head(rendements_logarithmique_Kenya, n=10)
plot(rendements_logarithmique_Kenya)

#Rendement logarithmique Maroc

rendements_logarithmique_Maroc <- Return.calculate(Maroc_xts, method = "log")
rendements_logarithmique_Maroc <- rendements_logarithmique_Maroc[-1]
date_max_Maroc_log <- index(rendements_logarithmique_Maroc)[which.max(rendements_logarithmique_Maroc)]
rendement_max_Maroc_log <- max(rendements_logarithmique_Maroc)
date_min_Maroc_log <- index(rendements_logarithmique_Maroc)[which.min(rendements_logarithmique_Maroc)]
rendement_min_Maroc_log <- min(rendements_logarithmique_Maroc)
cat("Date et rendement le plus élevé :", date_max_Maroc_log, rendement_max_Maroc_log, "\n")
cat("Date et rendement le plus bas :", date_min_Maroc_log, rendement_min_Maroc_log, "\n")
head(rendements_logarithmique_Maroc, n=10)
plot(rendements_logarithmique_Maroc)

###### TEST D'HYPOTHESE  Test de student

t.test(rendements_logarithmique_Ghana) 
N=length(Ghana)

mu <- mean(rendements_logarithmique_Ghana); v1 <- var(rendements_logarithmique_Ghana)
lcl <- mu-1.96*sqrt(v1)
ucl <- mu+1.96*sqrt(v1)
c(lcl, ucl)

t.test(rendements_logarithmique_Kenya) 
N=length(Kenya)

mu <- mean(rendements_logarithmique_Kenya); v1 <- var(rendements_logarithmique_Kenya)
lcl <- mu-1.96*sqrt(v1)
ucl <- mu+1.96*sqrt(v1)
c(lcl, ucl)

t.test(rendements_logarithmique_Maroc) 
N=length(Maroc)

mu <- mean(rendements_logarithmique_Maroc); v1 <- var(rendements_logarithmique_Maroc)
lcl <- mu-1.96*sqrt(v1)
ucl <- mu+1.96*sqrt(v1)
c(lcl, ucl)




##### QUESTION 06
N = length(rendements_logarithmique_Ghana)
skew_Ghana <- skewness(rendements_logarithmique_Ghana)
p_v_Ghana <- 2 * (1 - pnorm(abs(s9)/sqrt(6/N)))
print(skew_Ghana)
print(p_v_Ghana)


N = length(rendements_logarithmique_Kenya)
skew_Kenya <- skewness(rendements_logarithmique_Kenya)
p_v_Kenya <- 2 * (1 - pnorm(abs(s8)/sqrt(6/N)))
print(skew_Kenya)
print(p_v_Kenya)


N = length(rendements_logarithmique_Maroc)
skew_Maroc <- skewness(rendements_logarithmique_Maroc)
p_v_Maroc <- 2 * (1 - pnorm(abs(s7)/sqrt(6/N)))
print(skew_Maroc)
print(p_v_Maroc)


##### QUESTION 07:

kurt_Ghana <- kurtosis(rendements_logarithmique_Ghana)
te1 <- kurt_Ghana/sqrt(24/1823)
pv_Ghana <- 2*(1-pnorm(te1))
pv_Ghana

kurt_Kenya <- kurtosis(rendements_logarithmique_Kenya)
te2 <- kurt_Kenya/sqrt(24/1823)
pv_Kenya <- 2*(1-pnorm(te2))
pv_Kenya

kurt_Maroc <- kurtosis(rendements_logarithmique_Maroc)
te3 <- kurt_Maroc/sqrt(24/1823)
pv_Maroc <- 2*(1-pnorm(te3))
p_v_Maroc








