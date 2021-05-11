#--------------------------PCA----------------------------------------------
#data1 <- read.csv('global_waf.csv', header=TRUE)
#names(data1)
library(readxl)
global_waf <- read_excel("global_waf.xlsx")

music_pca <- data.frame(danceability=global_waf[,10],energy=global_waf[,11],
                        loudness=global_waf[,13],speechiness=global_waf[,15],
                        acousticness=global_waf[,16],instrumentalness=global_waf[,17],
                        liveness=global_waf[,18],valence=global_waf[,19],
                        tempo=global_waf[,20],duration=global_waf[,21])
music_pca = scale(music_pca)
f <- as.numeric(music_pca[1,])
names(f) <- names(music_pca)
barplot(f)
library(psych)
scree(music_pca, factors=FALSE, pc=TRUE)
#Got 4 PCA components

library(psych)
pcav <- principal(music_pca, nfactors=4, rotate='varimax')
pcav
pcav$values
pcav$communality
#ANS:
#RC1 = 0.87(energy) + 0.85(loudness) 
#RC3 = 0.83(danceability)
#RC2 = 0.74(speechiness) + 0.76(tempo)
#RC4 = 0.65(duration_ms)

par(mar=c(4,4,1,1))
library(psych)
biplot.psych(pcav, col=c("black","red"), cex=c(0.5,1),
             arrow.len=0.08, main=NULL, labels=row.names(USArrests))

#Energy and loudness most influence in RC1, they are correlated to each other
#and so on, as equation stated.


#----------------------------------CLUSTEERING-------------------------------
library(NbClust)
nb <- NbClust(music_pca, distance = "euclidean", min.nc = 2,
      max.nc = 7, method = "complete", index ="kl")
n  <- nb$Best.nc[1]
# we got number of clusters to be 4!
#Please wait for a while for this part of code...
kc <- kmeans(music_pca, centers=4, nstart=5)
kc
#music_pca = music_pca[]
library(cluster)
clusplot(music_pca,kc$cluster,color=TRUE,shade=TRUE,labels=2)


#--------------------------PCA----------------------------------------------
data1 <- read.csv('au_waf.csv', header=TRUE)
names(data1)
#library(readxl)
#global_waf <- read_excel("global_waf.xlsx")

music_pca <- data.frame(danceability=data1[,10],energy=data1[,11],
                        loudness=data1[,13],speechiness=data1[,15],
                        acousticness=data1[,16],instrumentalness=data1[,17],
                        liveness=data1[,18],valence=data1[,19],
                        tempo=data1[,20],duration=data1[,21])
music_pca = scale(music_pca)
f <- as.numeric(music_pca[1,])
names(f) <- names(music_pca)
barplot(f)
library(psych)
scree(music_pca, factors=FALSE, pc=TRUE)
#Got 4 PCA components

library(psych)
pcav <- principal(music_pca, nfactors=4, rotate='varimax')
pcav
pcav$values
pcav$communality
#ANS:
#RC1 = 0.87(energy) + 0.85(loudness) 
#RC3 = 0.83(danceability)
#RC2 = 0.74(speechiness) + 0.76(tempo)
#RC4 = 0.65(duration_ms)

par(mar=c(4,4,1,1))
library(psych)
biplot.psych(pcav, col=c("black","red"), cex=c(0.5,1),
             arrow.len=0.08, main=NULL, labels=row.names(USArrests))

#Energy and loudness most influence in RC1, they are correlated to each other
#and so on, as equation stated.


#----------------------------------CLUSTEERING-------------------------------
library(NbClust)
nb <- NbClust(music_pca, distance = "euclidean", min.nc = 2,
              max.nc = 7, method = "complete", index ="kl")
n  <- nb$Best.nc[1]
# we got number of clusters to be 5!
#but use 4, cuz we got 4 from global data
kc <- kmeans(music_pca, centers=4, nstart=5)
kc
#music_pca = music_pca[]
library(cluster)
clusplot(music_pca,kc$cluster,color=TRUE,shade=TRUE,labels=2)

ds1 <- dist(music_pca, method="euclidean") 
hcst1 <- hclust(ds1,method="complete")
plot(hcst1, labels=rownames(music_pca), cex=1) 
rect.hclust(hcst1, 4)

#now, check before and after COVID that music cluster is changed to other types

clus = kc$cluster
i = 1
for (item in clus) {
  if (item == 1){
    out[i] = lvs[2]
  }
  else{
    out[i] = lvs[1]
  }
  i = i + 1
}

#do hypothesis, t-test and check the change with p-value

library(fmsb)
music_pca = as.data.frame(music_pca)
radarchart(music_pca[1:3,])
