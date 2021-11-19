################################################################################
# Títol: Outlier treatment 
# Autor: Aleix Nieto
# Fecha: 19/11/21
# Descripción: Estudi estadístic d'una base de dades amb diferents features de 
# ratolins proporcionada per IDIBAPS i IBEC
################################################################################
# install the package 
# install.packages("ggstatsplot")
# Load the package
library(ggstatsplot)
#Tractar outliers, mirar-se lo random que són els missings, millorar mice, françoa pca

################################################################################
# Títol: Principal Component Analysis 
# Autor: Aleix Nieto
# Fecha: 19/11/21
# Descripción: Estudi estadístic d'una base de dades amb diferents features de 
# ratolins proporcionada per IDIBAPS i IBEC
################################################################################

# Instal·lem els paquets que farem servir
# install.packages("corrplot")
# install.packages("PerformanceAnalytics")
# install.packages("FactorMineR")
# install.packages("factoextra")
# install.packages("mice")
# install.packages("naniar")
# install.packages("VIM")
# install.packages("missForest")
# Carreguem els paquets
library("corrplot")
library("PerformanceAnalytics")
library("FactoMineR")
library("factoextra")
library("mice")
library("naniar")
library("VIM")
library("missForest")
# 1st way to read the dataframe
# <- read.csv("C:/Users/garys/Desktop/PRACTIQUES/ESTUDI ESTADÍSTIC RATOLINS/estudi_ratolins.csv", sep=";")

# 2nd way to read the dataframe
# Webpage to remove "ï.." that appears in the first variable when reading csv 
# https://www.roelpeters.be/removing-i-umlaut-two-dots-data-frame-column-read-csv/ 
setwd("C:/Users/garys/Desktop/PRACTIQUES/ESTUDI ESTADÍSTIC RATOLINS/")
dd <- read.table("estudi_ratolins.csv", header=T, sep=";", fileEncoding = 'UTF-8-BOM');

# Identifiquem cada individu amb la columna label
row.names(dd)<-dd[,2]
identificador <- row.names(dd)
identificador
#Eliminem la columna dels labels ja que els hem posat coma a identificadors i no aporten res
dd<-dd[,-2]

# Mirem de quina classe són les variables
sapply(dd,class)

#Algunes característiques de la base de dades
summary(dd)
objects()
attributes(dd)

# Decralació de variables

# Definim el tipus de variables
v<-list(
  categoric=c('gender','label','diet','time'),
  integer=c('group','final_auc','fasting_glucosa_final'),
  continua=c('final_weight','weight_gain','LV_weight','LV_ratio','WAT_weight','WAT_ratio',
             'insulin_0_final','insulin_15_final','homa_ir','homa_beta','liver_trigly'))

v$numeric<-c(v$integer,v$continua)

# Descripción general
summary(dd[,v$categoric])
summary(dd[,v$numeric])

# Convertim les variables en numèriques per poder aplicar el PCA, primer canviem totes les comes
# dels decimals per punts i després convertim en character per poder convertir en numèriques
for(i in v$numeric) dd[,i]<-as.numeric(as.character(gsub(",",".",dd[,i],fixed=TRUE)))
sapply(dd,class)

# Veiem els missings de cada variable
sapply(dd, function(x) sum(is.na(x)))

# **Missing Imputation**
# Simple imputation (mean)
# Si no poso na.rm=TRUE i faig mean(dd[,i]) ens donarà NA perquè hi ha missings, aleshores na.rm=TRUE
# el que fa es eliminar les components TRUE del vector is.na(dd[,i]) que és TRUE si és missing i FALSE altrament.
#for(i in v$numeric) dd[,i][which(is.na(dd[,i]))]=mean(dd[,i], na.rm=TRUE)

# Treballem amb la base de dades només numèrica pel PCA, imputarem els missings en aquesta base ja que 
# les variables que hem perdut definien els grups i no tenien missings
dd.pca <- dd[,c(5:17)]
# Create a boxplot of the dataset
for (i in 1:length(dd.pca))boxplot(dd.pca[,i],main = names(dd.pca)[i], horizontal = TRUE)
  
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

for (i in 1:length(dd.pca)) dd.pca[,i] <- remove_outliers(dd.pca[,i])


# https://stats.stackexchange.com/questions/58525/re-check-boxplot-after-outlier-removal
for (i in 1:length(dd.pca))boxplot(dd.pca[,i],main = names(dd.pca)[i], horizontal = TRUE)

vis_miss(dd.pca)

#saving the dataframe in an external file
write.table(dd.pca, file = "dataclean.csv", sep = ",", na = "NA",row.names = TRUE, col.names = TRUE)
