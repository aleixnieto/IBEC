################################################################################
# Title: Hierarchical clustering
# Author: Aleix Nieto
# Date: 11/21 - 12/21
# Description: Statistical study of a mice dataframe provided by IBAPS & IBEC
################################################################################
for(i in c("cluster","lattice", "FactoMineR", "factoextra")) require(i,character.only = T)

setwd("C:/Users/garys/Desktop/PRACTIQUES/ESTUDI ESTADÍSTIC RATOLINS/")
dd <- read.table("datapreprocessed.csv", header=T, sep=",", fileEncoding = 'UTF-8-BOM');
dd[,1:4] <- lapply(dd[,1:4],as.factor);

sapply(dd,class)
actives<-c(1:length(dd))

# Comprovation of the preprocessing
sapply(dd, function(x) sum(is.na(x)))

# Variable declaration
v<-list(
  categoric=c('gender','diet','time','group'),
  integer=c('final_auc','fasting_glucosa_final'),
  continua=c('final_weight','weight_gain','LV_weight','LV_ratio','WAT_weight','WAT_ratio',
             'insulin_0_final','insulin_15_final','homa_ir','homa_beta','liver_trigly'))

for(i in v$continua) dd[,i]<-as.numeric(as.character(gsub(",",".",dd[,i],fixed=TRUE)))
for(i in v$categoric) dd[[i]]<-as.factor(dd[[i]])
for(i in v$integer) dd[[i]]<-as.integer(dd[[i]])

v$numeric<-c(v$integer,v$continua)

# Clustering
# Dissimilarity Matrix (Gower as we have mixed data)
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)
distMatrix<-dissimMatrix^2

# Clustering Ward Method, k=3
h1 <- hclust(distMatrix,method="ward.D2")
class(h1)
plot(h1, labels = F)
rect.hclust(h1, k = 3, border = rainbow(8)) #[we cut at h = 1.5 but use k for efficiency reasons]

# Cutting previous cluster
c2 <- cutree(h1,3)
dd$cluster<- as.factor(c2)

# Class sizes 
table(c2)
dd.pca <- dd[,v$numeric]
res.pca <- PCA(dd.pca, graph = FALSE)

# GROUP
fviz_pca_ind(res.pca,  label="none", habillage=as.factor(dd$cluster))

fviz_pca_ind(res.pca, label="none", habillage=as.factor(dd$cluster),
             addEllipses=TRUE, ellipse.level=0.95)

# # Cutting previous cluster, k=2
# rect.hclust(h1, k = 2, border = rainbow(8))
# c2 <- cutree(h1,2)
# dd$cluster_k_2<- as.factor(c2)
# 
# # Class sizes 
# table(c2)
# dd.pca <- dd[,v$numeric]
# res.pca <- PCA(dd.pca, graph = FALSE)
# 
# # GROUP
# fviz_pca_ind(res.pca,  label="none", habillage=as.factor(dd$cluster_k_2))
# 
# fviz_pca_ind(res.pca, label="none", habillage=as.factor(dd$cluster_k_2),
#              addEllipses=TRUE, ellipse.level=0.95)

# Profiling

# Calcula els valor test de la variable Xnum per totes les modalitats del factor P
ValorTestXnum <- function(Xnum,P){
  # nº of individuals in each cluster & total number of individuals
  nk <- as.vector(table(P)); 
  n <- sum(nk); 
  # Mean by group
  xk <- tapply(Xnum,P,mean);
  # Valors test
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk))); 
  # p-values
  pxk <- pt(txk,n-1,lower.tail=F);
  
  for(c in 1:length(levels(as.factor(P)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}

ValorTestXquali <- function(P,Xquali){
  taula <- table(P,Xquali);
  n <- sum(taula); 
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2]);      
  dpf <- pf - pjm; 
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); 
  # If there are divisions equal to 0 it gives NA and it does not work
  zkj <- dpf
  zkj[dpf!=0]<-dpf[dpf!=0]/dvt[dpf!=0]; 
  pzkj <- pnorm(zkj,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
}

# Dades contain the dataset
dades<-dd
K<-dim(dades)[2]
P<-c2
nc<-length(levels(factor(P)))
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(dades)))
nameP<-"Class"
n<-dim(dades)[1]

for(k in 1:(K-1)){
  if (is.numeric(dades[,k])){ 
    print(paste("Analysis by class of the variable:", names(dades)[k]))
    
    boxplot(dades[,k]~P, main=paste("Boxplot of", names(dades)[k], "vs", nameP ), horizontal=TRUE)
    
    barplot(tapply(dades[[k]], P, mean),main=paste("Means of", names(dades)[k], "by", nameP ))
    abline(h=mean(dades[[k]]))
    legend(0,mean(dades[[k]]),"global mean",bty="n")
    print("Statistics per groups:")
    for(s in levels(as.factor(P))) {print(summary(dades[P==s,k]))}
    o<-oneway.test(dades[,k]~P)
    print(paste("p-valueANOVA:", o$p.value))
    kw<-kruskal.test(dades[,k]~P)
    print(paste("p-value Kruskal-Wallis:", kw$p.value))
    pvalk[,k]<-ValorTestXnum(dades[,k], P)
    print("p-values ValorsTest: ")
    print(pvalk[,k])      
  }else{
    #qualitatives
      print(paste("Variable", names(dades)[k]))
      table<-table(P,dades[,k])
      #   print("Cross-table")
      #   print(table)
      rowperc<-prop.table(table,1)
      
      colperc<-prop.table(table,2)
      #  print("Distributioned conditioned to files")
      # print(rowperc)
      
      # Be careful because if the feature is true or false, it is identified
      # with the type Logical, this does not have levels -> preventive coertion 
      
      dades[,k]<-as.factor(dades[,k])
      
      
      marg <- table(as.factor(P))/n
      print(append("Categories=",levels(as.factor(dades[,k]))))
      
      # From next plots, select one of them according to your practical case with legend
      plot(marg,type="l",ylim=c(0,1),main=paste("Proportion by",names(dades)[k]))
      paleta<-rainbow(length(levels(dades[,k])))
      for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
      legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
      
      # Conditioned to classes with legend
      print(append("Categories=",levels(dades[,k])))
      plot(marg,type="n",ylim=c(0,1),main=paste("Proportion by",names(dades)[k]))
      paleta<-rainbow(length(levels(dades[,k])))
      for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
      legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
      
      # With variables in the x axis with legend
      marg <-table(dades[,k])/n
      print(append("Categories=",levels(dades[,k])))
      plot(marg,type="l",ylim=c(0,1),main=paste("Proportion by",names(dades)[k]), las=3)
      #x<-plot(marg,type="l",ylim=c(0,1),main=paste("Proportion by",names(dades)[k]), xaxt="n")
      #text(x=x+.25, y=-1, adj=1, levels(CountryName), xpd=TRUE, srt=25, cex=0.7)
      paleta<-rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c]) }
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)      
      
      # Conditioned to column with legend
      plot(marg,type="n",ylim=c(0,1),main=paste("Proportion by",names(dades)[k]), las=3)
      paleta<-rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c]) }
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
      
      table<-table(dades[,k],P)
      print("Cross Table:")
      print(table)
      print("Distributions conditioned to columns:")
      print(colperc)
      
      # Applied bar charts                                         
      
      paleta<-rainbow(length(levels(dades[,k])))
      barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta, main=paste("Frequency by",names(dades)[k]) )
      legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
      
      # Attached bar charts
      
      barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta, main=paste("Frequency by",names(dades)[k]))
      legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
      
      print("Test Chi quadrat: ")
      print(chisq.test(dades[,k], as.factor(P)))
      
      print("valorsTest:")
      print( ValorTestXquali(P,dades[,k]))
      }
  }
