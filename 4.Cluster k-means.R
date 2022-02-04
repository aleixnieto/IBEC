################################################################################
# Title: k-means clustering
# Author: Aleix Nieto
# Date: 11/21 - 12/21
# Description: K-means clustering + mean variable plot comparison between clusters
################################################################################

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr")
ipak(packages)

setwd("C:/Users/garys/Desktop/PRACTIQUES/MICE STATISTICAL ANALYSIS/DATAFRAMES GENERATED/")
dd<- read.table("datapreprocessed.csv", header=T, sep=",", fileEncoding = 'UTF-8-BOM');

v<-list(
  categoric=c('gender','diet','time','group'),
  integer=c('final_auc','fasting_glucosa_final'),
  continua=c('final_weight','weight_gain','LV_weight','LV_ratio','WAT_weight','WAT_ratio',
             'insulin_0_final','insulin_15_final','homa_ir','homa_beta','liver_trigly'))

for(i in v$continua) dd[,i]<-as.numeric(as.character(gsub(",",".",dd[,i],fixed=TRUE)))
for(i in v$categoric) dd[[i]]<-as.factor(dd[[i]])
for(i in v$integer) dd[[i]]<-as.integer(dd[[i]])

sapply(dd,class)

v$numeric<-c(v$integer,v$continua)

dd.num <- dd[,5:17]

# Normalize data
dd.num <- scale(dd.num)
head(dd.num)

# Distance matrix
# We can also use these methods: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
m.distancia <- get_dist(dd.num, method = "euclidean") 
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

# Estimate the number of clusters
# Elbow, silhouette o gap_stat  method
fviz_nbclust(dd.num, kmeans, method = "wss")
fviz_nbclust(dd.num, kmeans, method = "silhouette")
fviz_nbclust(dd.num, kmeans, method = "gap_stat")

# Estimate the number of clusters with different methods
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
resnumclust<-NbClust(dd.num, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)

# We calculate the clusters with k=2
k2 <- kmeans(dd.num, centers = 2, nstart = 25)
# Here we get all the information of the cluster, size, cluster of each individual
# cluster mean for each variable, cluster vector, within cluster sum of squares,
# between_SS / total_SS
k2

# We calculate the clusters with k=3
k3 <- kmeans(dd.num, centers = 3, nstart = 25)
k3
 
# Cluster plot
fviz_cluster(k2, data = dd.num)
fviz_cluster(k2, data = dd.num, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k2, data = dd.num, ellipse.type = "norm")
fviz_cluster(k2, data = dd.num, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

fviz_cluster(k3, data = dd.num)
fviz_cluster(k3, data = dd.num, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k3, data = dd.num, ellipse.type = "norm")
fviz_cluster(k3, data = dd.num, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

res2 <- hcut(dd.num, k = 2, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5, show_labels = FALSE, k_colors = c("red","#2E9Fdd"))

res3 <- hcut(dd.num, k = 3, stand = TRUE)
fviz_dend(res3, rect = TRUE, cex = 0.5, show_labels = FALSE, k_colors = c("red","#2E9Fdd","green"))

# Mean of each cluster for every variable
dd.num <- dd[,v$numeric]
dd.num %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

# Pass the clusters to my initial dd.num to work with them
dd.num <- scale(dd.num)
dd.num<- as.data.frame(dd.num)
dd.num$clus<-as.factor(k2$cluster)
dd.num

dd.num$clus<-factor(dd.num$clus)
data_long <- gather(dd.num, caracteristica, value, v$numeric, factor_key=TRUE)
data_long

ggplot(data_long, aes(caracteristica, y = value, group=clus, colour = clus)) +
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(geom="line")
geom_point(aes(shape=clus))



