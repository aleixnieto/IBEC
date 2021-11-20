################################################################################
# Títol: Descriptiva
# Autor: Aleix Nieto
# Data: 19/11/21
# Descripció: Estudi estadístic d'una base de dades amb diferents features de 
# ratolins proporcionada per IDIBAPS i IBEC
################################################################################

# 1st way to read the dataframe
# <- read.csv("C:/Users/garys/Desktop/PRACTIQUES/ESTUDI ESTADÍSTIC RATOLINS/estudi_ratolins.csv", sep=";")

# 2nd way to read the dataframe
# Webpage to remove "ï.." that appears in the first variable when reading csv 
# https://www.roelpeters.be/removing-i-umlaut-two-dots-data-frame-column-read-csv/ 
setwd("C:/Users/garys/Desktop/PRACTIQUES/ESTUDI ESTADÍSTIC RATOLINS/")
dd <- read.table("estudi_ratolins.csv", header=T, sep=";", fileEncoding = 'UTF-8-BOM');

# Identifiquem cada individu amb la columna label
row.names(dd)<-dd[,2]
# identificador <- row.names(dd)
# identificador

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
  categoric=c('gender','diet','time'),
  integer=c('group','final_auc','fasting_glucosa_final'),
  continua=c('final_weight','weight_gain','LV_weight','LV_ratio','WAT_weight','WAT_ratio',
             'insulin_0_final','insulin_15_final','homa_ir','homa_beta','liver_trigly'))

v$numeric<-c(v$integer,v$continua)

# Descripción general
summary(dd[,v$categoric])
summary(dd[,v$numeric])

# Convertim les variables en numèriques per poder aplicar el diferents métodes estadístics, primer canviem totes les comes
# dels decimals per punts i després convertim en character per poder convertir en numèriques
for(i in v$numeric) dd[,i]<-as.numeric(as.character(gsub(",",".",dd[,i],fixed=TRUE)))
sapply(dd,class)

# Veiem els missings de cada variable
sapply(dd, function(x) sum(is.na(x)))

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
                 labels=names(dd), cex.axis=.7,
                 gap=3, ylab=c("Missing data","Pattern"))


gg_miss_upset(dd.pca)


# Veiem els missings de cada variable
sapply(dd, function(x) sum(is.na(x)))
