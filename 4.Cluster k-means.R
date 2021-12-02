#Posar encapçalament
#Cargar y utilizar función IPAK
#ver vídeo https://www.youtube.com/watch?v=UjQz9SxG9rk
#https://www.youtube.com/watch?v=7AFuL-1Q8eg&ab_channel=PabloVallejoMedina

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr")
ipak(packages)

setwd("C:/Users/garys/Desktop/PRACTIQUES/ESTUDI ESTADÍSTIC RATOLINS/")
dd <- read.table("datapreprocessed.csv", header=T, sep=",", fileEncoding = 'UTF-8-BOM');
dd <- dd[,5:17]

#normalizar las puntuaciones
dd <- scale(dd)
head(dd)

#calcular la matriz de distacias
m.distancia <- get_dist(dd, method = "euclidean") #el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

#estimar el número de clústers
#Elbow, silhouette o gap_stat  method
fviz_nbclust(dd, kmeans, method = "wss")
fviz_nbclust(dd, kmeans, method = "silhouette")
fviz_nbclust(dd, kmeans, method = "gap_stat")

#con esta función se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
resnumclust<-NbClust(dd, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)

# Calculamos los dos clústers
k2 <- kmeans(dd, centers = 3, nstart = 25)
k2
class(k2)
str(k2)

#plotear los cluster
fviz_cluster(k2, data = dd)
fviz_cluster(k2, data = dd, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k2, data = dd, ellipse.type = "norm")
fviz_cluster(k2, data = dd, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

res2 <- hcut(dd, k = 2, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9Fdd"))

res4 <- hcut(dd, k = 4, stand = TRUE)
fviz_dend(res4, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9Fdd","green","black"))

# #pasar los cluster a mi dd inicial para trabajar con ellos
# 
# USArrests %>%
#   mutate(Cluster = k2$cluster) %>%
#   group_by(Cluster) %>%
#   summarise_all("mean")
# 
# dd <- USArrests
# dd
# dd$clus<-as.factor(k2$cluster)
# dd
# 
# dd <- USArrests
# dd <- scale(dd)
# dd<- as.data.frame(dd)
# dd$clus<-as.factor(k2$cluster)
# dd
# 
# dd$clus<-factor(dd$clus)
# data_long <- gather(dd, caracteristica, valor, Murder:Rape, factor_key=TRUE)
# data_long
# 
# ggplot(data_long, aes(as.factor(x = caracteristica), y = valor,group=clus, colour = clus)) + 
#   stat_summary(fun = mean, geom="pointrange", size = 1)+
#   stat_summary(geom="line")
# #geom_point(aes(shape=clus))