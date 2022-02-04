################################################################################
# Title: Outlier treatment
# Author: Aleix Nieto
# Date: 19/11/21
# Description: Outlier detection and total missings overview
################################################################################

# install the package ggstatsplot
install.packages("ggstatsplot")
# Load the package
library(ggstatsplot)

setwd("C:/Users/garys/Desktop/PRACTIQUES/MICE STATISTICAL ANALYSIS/DATAFRAMES GENERATED/")
dd <- read.table("datatopreprocess.csv", header=T, sep=";", fileEncoding = 'UTF-8-BOM');

v<-list(
  categoric=c('gender','diet','time','group'),
  integer=c('final_auc','fasting_glucosa_final'),
  continua=c('final_weight','weight_gain','LV_weight','LV_ratio','WAT_weight','WAT_ratio',
             'insulin_0_final','insulin_15_final','homa_ir','homa_beta','liver_trigly'))

for(i in v$continua) dd[,i]<-as.numeric(as.character(gsub(",",".",dd[,i],fixed=TRUE)))
for(i in v$categoric) dd[[i]]<-as.factor(dd[[i]])
for(i in v$integer) dd[[i]]<-as.integer(dd[[i]])

v$numeric<-c(v$integer,v$continua)

# Create a boxplot of the dataset
for (i in v$numeric) boxplot(dd[,i], main = names(dd)[which(names(dd)==i)], horizontal = TRUE)
  
remove_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

for (i in v$numeric) dd[,i] <- remove_outliers(dd[,i])

# https://stats.stackexchange.com/questions/58525/re-check-boxplot-after-outlier-removal
for (i in v$numeric)boxplot(dd[,i],main = names(dd)[i], horizontal = TRUE)

# Missings of each variable, now we see that outliers are NA's
sapply(dd, function(x) sum(is.na(x)))
vis_miss(dd)

# Saving the dataframe in an external file
write.table(dd, file = "dataclean.csv", sep = ",", na = "NA",row.names = TRUE, col.names = TRUE)

