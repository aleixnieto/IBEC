################################################################################
# Title: Descriptive analysis
# Author: Aleix Nieto
# Date: 19/11/21
# Description: Descriptive analysis of the variables, overview of dataframe features,
# and set up the dataframe to be able to preprocess it. 
################################################################################

# Install and load packages that we will use
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("ggplot2","naniar", "mice", "VIM")
ipak(packages)

# If we wanted to read the dataframe from github, in our case our dataframe is confidential so we can't open it from a public repository
#data <-read.csv("githublink")

# 1st way to read the dataframe
# <- read.csv("C:/Users/garys/Desktop/PRACTIQUES/ESTUDI ESTADÍSTIC RATOLINS/estudi_ratolins.csv", sep=";")

# 2nd way to read the dataframe
# Webpage to remove "ï.." that appears in the first variable when reading csv -->
# https://www.roelpeters.be/removing-i-umlaut-two-dots-data-frame-column-read-csv/ 
# check.names: logical. If TRUE then the names of the variables in the data frame
# are checked to ensure that they are syntactically valid variable names and are not duplicated.
# In our case check.names is not necessary because all the variable names are valid (no dots, spaces, etc)
setwd("C:/Users/garys/Desktop/PRACTIQUES/MICE STATISTICAL ANALYSIS/DATAFRAMES GENERATED/")
dd <- read.table("estudi_ratolins.csv", header=T, sep=";", fileEncoding = 'UTF-8-BOM', check.names = FALSE);

# We identify every individual with column label
row.names(dd)<-dd[,2]
row.names(dd)

# We delete the labels column because we have put them as identifiers and they do not contribute
dd<-dd[,-2]

# Check variables class
sapply(dd,class)

# Som characteristics of the dataframe
summary(dd)
objects()
attributes(dd)

# Variable declaration

# We define variable type
v<-list(
  categoric=c('gender','diet','time','group'),
  integer=c('final_auc','fasting_glucosa_final'),
  continua=c('final_weight','weight_gain','LV_weight','LV_ratio','WAT_weight','WAT_ratio',
             'insulin_0_final','insulin_15_final','homa_ir','homa_beta','liver_trigly'))

# We convert the variables into numerical to be able to apply the different statistical methods,
# first we change all the commas of the decimals by points and then we convert to character to be able to turn into numerical
for(i in v$continua) dd[,i]<-as.numeric(as.character(gsub(",",".",dd[,i],fixed=TRUE)))
for(i in v$categoric) dd[[i]]<-as.factor(dd[[i]])
for(i in v$integer) dd[[i]]<-as.integer(dd[[i]])

sapply(dd,class)

v$numeric<-c(v$integer,v$continua)

# General description
summary(dd[,v$categoric])
summary(dd[,v$numeric])

# We see the missing values of each variable
sapply(dd, function(x) sum(is.na(x)))

# https://medium.com/mlearning-ai/visualizing-continous-data-with-ggplot2-in-r-2e4b7f433f67

# Histogrms for the numerical variables version 1, not that clean
# for(i in names(dd)){
# plot(ggplot(dd, aes(x=weight_gain)) +
#   geom_histogram(binwidth=1, fill="#FF9999", color="#e9ecef", alpha=0.9) +
#   labs(title  = i))
# }

# We plot all the numerical histograms into a PC folder
for(i in v$numeric){
    png(file = paste0('C:/Users/garys/Desktop/PRACTIQUES/MICE STATISTICAL ANALYSIS/PLOTS/', i, ".","png"), bg = "transparent")
    p <- ggplot(dd, aes(x=dd[,i]))+ theme_minimal()+
    geom_histogram(color="darkblue", fill="lightblue", bins=30)
  
  print(p + labs(title= i,
          x = names(dd)[which(names(dd)==i)], y = "Count")) 
  dev.off()
}

# Contingency tables of categorical variables
for(i in v$categoric){
  print(table(dd[,which(names(dd)==i)]))
}

# Other Pie Chart version not that clean
# for(i in v$categoric){
#     pie(table(dd[,which(names(dd)==i)]), radius = 1, col = 2:(length(names(table(dd[,which(names(dd)==i)])))+1),labels=NA, angle = 45, density=NULL)
#     legend(x = "topleft", legend = names(table(dd[,which(names(dd)==i)])),
#            fill= 2:(length(names(table(dd[,which(names(dd)==i)])))+1), cex=0.8)
# }

suppressWarnings({ 
require("RColorBrewer")
for(i in v$categoric){
  png(file = paste0('C:/Users/garys/Desktop/PRACTIQUES/ESTUDI ESTADÍSTIC RATOLINS/PLOTS/', i, ".","png"), bg = "transparent")
  pie(table(dd[,which(names(dd)==i)]), labels= NA, radius = 1, col=brewer.pal(length(names(table(dd[,which(names(dd)==i)]))),'Spectral'))
  legend(x = "topleft",bty != "n", legend = names(table(dd[,which(names(dd)==i)])),
         fill=brewer.pal(length(names(table(dd[,which(names(dd)==i)]))),'Spectral'),cex=1)
    dev.off()
  }
})
# Missing visualitzation with naniar package <- https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
# This plot provides a specific visualiation of the amount of missing data, showing in black the location of missing values, and also 
# providing information on the overall percentage of missing values overall (in the legend), and in each variable.
vis_miss(dd)

md.pattern(dd, plot=TRUE, rotate.names = TRUE)

missplot <- aggr(dd, col=c('aquamarine', 'olivedrab3'),
                 numbers=TRUE, sortVars=TRUE,
                 labels=names(dd), cex.axis=.7,
                 gap=3, ylab=c("Missing data","Pattern"))


gg_miss_upset(dd)


# Missings of each variable
sapply(dd, function(x) sum(is.na(x)))

# Saving the dataframe in an external file
write.table(dd, file = "datatopreprocess.csv", sep = ";", na = "NA",row.names = TRUE, col.names = TRUE)

