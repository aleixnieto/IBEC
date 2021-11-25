setwd("C:/Users/garys/Desktop/PRACTIQUES/ESTUDI ESTADÍSTIC RATOLINS/")
dd <- read.table("datapreprocessed.csv", header=T, sep=",", fileEncoding = 'UTF-8-BOM');
dd[,1:4] <- lapply(dd[,1:4],as.factor);
names(dd)
dim(dd)
summary(dd)
length(dd)
actives<-c(1:length(dd))

##Comprobaci?n del preprocessing
sapply(dd, function(x) sum(is.na(x)))
##Necesary libraries
library(cluster)
library(lattice)
library(factoextra)

###Clustering
##Dissimilarity Matrix (Gower as we have mixed data)
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)
distMatrix<-dissimMatrix^2

##Clustering Ward Method
h1 <- hclust(distMatrix,method="ward.D2")
class(h1)
str(h1)
plot(h1, labels = F)
rect.hclust(h1, k = 3, border = rainbow(8)) #[we cut at h = 1.5 but use k for efficiency reasons]

##Cutting previous cluster
k<-3
c2 <- cutree(h1,k)
dd[,18]<- as.factor(c2)
#class sizes 
table(c2)
