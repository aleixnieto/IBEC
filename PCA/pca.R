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

# Visualització dels missings amb el package naniar <- https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
# This plot provides a specific visualiation of the amount of missing data, showing in black the location of missing values, and also 
# providing information on the overall percentage of missing values overall (in the legend), and in each variable.
vis_miss(dd.pca)
md.pattern(dd.pca, plot=TRUE, rotate.names = FALSE)

missplot <- aggr(dd.pca, col=c('aquamarine', 'olivedrab3'),
                 numbers=TRUE, sortVars=TRUE,
                 labels=names(dd.pca), cex.axis=.7,
                 gap=3, ylab=c("Missing data","Pattern"))


gg_miss_upset(dd.pca)


# MICE imputation <- https://www.r-bloggers.com/2016/06/handling-missing-data-with-mice-package-a-simple-approach/
#methods(mice)
# No podem imputar només fent: dd.pca = mice(dd, m=5) ja que dona un error Warning message:
# Number of logged events --> https://stefvanbuuren.name/fimd/sec-toomany.html; les variables
# categòriques tenen 0 valor predictiu i reelentitzen moltíssim l'algorisme més altres coses que s'expliquen allà
#?mice

# MIRAR BÉ IMPUTACIÓ AMB EL MICE A LA PÀGINA ANTERIOR

init = mice(dd.pca, maxit=0)
# meth = init$method
# predM = init$predictorMatrix
# predM[v$categoric]=0

dd.pca = mice(dd.pca, m=5)
dd.pca <- complete(dd.pca)
sapply(dd.pca, function(x) sum(is.na(x)))

# Correlation matrix
# https://www.youtube.com/watch?v=qUmmATEJdgM&ab_channel=NurseKillam
# https://courses.lumenlearning.com/introstats1/chapter/testing-the-significance-of-the-correlation-coefficient/
cor.mat <- round(cor(dd.pca),2)
head(cor.mat)

corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

testRes = cor.mtest(dd.pca, conf.level = 0.95)

corrplot(cor.mat, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.55, order = 'AOE', diag=FALSE)
# PCA
res.pca <- PCA(dd.pca, graph = FALSE)
print(res.pca)

eigenvalues <- res.pca$eig
head(eigenvalues[, 1:2])

# barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
#         main = "Variances",
#         xlab = "Principal Components",
#         ylab = "Percentage of variances",
#         col ="steelblue")
# 
# # Add connected line segments to the plot
# lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
#       type="b", pch=19, col = "red")

fviz_screeplot(res.pca, ncp=10)

# coordinates of variables on the principal components
head(res.pca$var$coord)

# The quality of representation of the variables of the principal components are called the cos2.
head(res.pca$var$cos2)

# Variable contributions in the determination of a given principal component are (in percentage) : (var.cos2 * 100) / (total cos2 of the component)
head(res.pca$var$contrib)

# Control variable colors using their contribution
fviz_pca_var(res.pca, col.var="contrib")


# Graph of individuals
# Coordinates of individuals on the principal components
head(res.pca$ind$coord)

# Cos2 : quality of representation of individuals on the principal components
head(res.pca$ind$cos2)

# Contribition of individuals to the princial components
head(res.pca$ind$contrib)

# Graph of individuals
# Control automatically the color of individuals using the cos2 values (the quality of the individuals on the factor map)
fviz_pca_ind(res.pca,  col.ind="cos2") +
  scale_color_gradient2(low="blue", mid="white", 
                        high="red", midpoint=0.50)+
  theme_minimal()

# Make a biplot of individuals and variables :
fviz_pca_biplot(res.pca,  geom = "text")

fviz_pca_ind(res.pca, label="none")


fviz_pca_ind(res.pca,  label="none", habillage=as.factor(dd$gender))

fviz_pca_ind(res.pca, label="none", habillage=as.factor(dd$gender),
             addEllipses=TRUE, ellipse.level=0.95)


fviz_pca_biplot(res.pca, 
                habillage = as.factor(dd$gender), addEllipses = TRUE,
                col.var = "red", alpha.var ="cos2",
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()