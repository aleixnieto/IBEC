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
# Webpage to remove "ï.." that appears in the first variable when reading csv -->
# https://www.roelpeters.be/removing-i-umlaut-two-dots-data-frame-column-read-csv/ 
# check.names: logical. If TRUE then the names of the variables in the data frame
# are checked to ensure that they are syntactically valid variable names and are not duplicated.
# En el nostre cas check.names no faria falta ja que tots els noms de les variables són vàlids(sense punts,espais,etc)
setwd("C:/Users/garys/Desktop/PRACTIQUES/ESTUDI ESTADÍSTIC RATOLINS/")
dd <- read.table("estudi_ratolins.csv", header=T, sep=";", fileEncoding = 'UTF-8-BOM', check.names = FALSE);
library("ggplot2")
library("labelled")
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
  categoric=c('gender','diet','time','group'),
  integer=c('final_auc','fasting_glucosa_final'),
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
names(v$numeric)
# https://medium.com/mlearning-ai/visualizing-continous-data-with-ggplot2-in-r-2e4b7f433f67
# for(i in names(dd)){
# plot(ggplot(dd, aes(x=weight_gain)) + 
#   geom_histogram(binwidth=1, fill="#FF9999", color="#e9ecef", alpha=0.9) +
#   labs(title  = i))
# }
for(i in v$numeric){
  p <- ggplot(dd, aes(x=dd[,i]))+ theme_minimal()+
    geom_histogram(color="darkblue", fill="lightblue", bins=30)
  
  print(p + labs(title= i,
          x = names(dd)[which(names(dd)==i)], y = "Count")) 
}
#Veiem les taules de contingència de les variables categòriques

#Versió dels pie charts no tan clean
# for(i in v$categoric){
#     pie(table(dd[,which(names(dd)==i)]), radius = 1, col = 2:(length(names(table(dd[,which(names(dd)==i)])))+1),labels=NA, angle = 45, density=NULL)
#     legend(x = "topleft", legend = names(table(dd[,which(names(dd)==i)])),
#            fill= 2:(length(names(table(dd[,which(names(dd)==i)])))+1), cex=0.8)
# }

require("RColorBrewer")
for(i in v$categoric){
  pie(table(dd[,which(names(dd)==i)]), radius = 1, col=brewer.pal(length(names(table(dd[,which(names(dd)==i)]))),'Spectral'))
  legend(x = "topleft", legend = names(table(dd[,which(names(dd)==i)])),
         fill=brewer.pal(length(names(table(dd[,which(names(dd)==i)]))),'Spectral'))
}

?pie
?legend

# Visualització dels missings amb el package naniar <- https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
# This plot provides a specific visualiation of the amount of missing data, showing in black the location of missing values, and also 
# providing information on the overall percentage of missing values overall (in the legend), and in each variable.
vis_miss(dd)
#?md.pattern
#md.pattern(dd, plot=TRUE, rotate.names = TRUE)

missplot <- aggr(dd, col=c('aquamarine', 'olivedrab3'),
                 numbers=TRUE, sortVars=TRUE,
                 labels=names(dd), cex.axis=.7,
                 gap=3, ylab=c("Missing data","Pattern"))


gg_miss_upset(dd)


# Veiem els missings de cada variable
sapply(dd, function(x) sum(is.na(x)))

#saving the dataframe in an external file
write.table(dd, file = "datatopreprocess.csv", sep = ";", na = "NA",row.names = TRUE, col.names = TRUE)
