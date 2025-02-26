##############################################################
## Project: RFM analysis for OnlineRetail data
## Script purpose: do RFM analysis for customer segmentation 
##                 via clustering methods (K means and K medoids),
##                 compare the results of the two techniques with 
##                 and without standardized RFM attributes, examine
##                 whether we need to remove the outliers
## Date: 2023/04/20
## Author: Zhen-Yan Chen
##############################################################

x <- read.csv("OnlineRetail.csv")
#missing value and meaningless
y <- na.omit(x)
y <- y[-which(y$UnitPrice<=0),]
y$InvoiceNo[which(y$Quantity<0)]
#Recency frequency Monetary
y[,5] <- as.POSIXct(y[,5], format="%d-%m-%Y %H:%M", tz= Sys.timezone())
z <- unique(y$CustomerID)
R <- c(1:length(z))
f <- c(1:length(z))
M <- c(1:length(z))
for (i in c(1:length(z))) {
  R[i] <- max(y[which(y$CustomerID==z[i]),5])
  f[i] <- length(unique(y[which(y$CustomerID==z[i]),1]))
  M[i] <- sum(y[which(y$CustomerID==z[i]),4]*y[which(y$CustomerID==z[i]),6])
}
rfm <- data.frame(R,f,M,row.names =z)
rfm1 <- data.frame(scale(R),scale(f),scale(M),row.names =z)
# EDA: outlier
plot(rfm1$scale.M., rfm1$scale.f.) # M and f have outliers
plot(rfm1$scale.R., rfm1$scale.f.)
boxplot(rfm1)
boxplot(rfm1$scale.R.)
boxplot(rfm1$scale.f.)
boxplot(rfm1$scale.M.)
# simply remove some outliers
rfmo <- rfm1[-which(rfm1$scale.M. > 10),]
rfmo <- rfmo[-which(rfmo$scale.f. > 10),]
plot(rfmo$scale.M., rfmo$scale.f.)
# clustering
library("cluster")
library("stats")

# Determine the number of clusters
fviz_nbclust(rfm, kmeans, method = "silhouette") + 
  geom_vline(xintercept = 2, linetype = 2, col = "blue")  # 2

fviz_nbclust(rfmo, kmeans, method = "silhouette") + 
  geom_vline(xintercept = 2, linetype = 2, col = "blue")  # 3

fviz_nbclust(rfm1, kmeans, method = "silhouette") + 
  geom_vline(xintercept = 2, linetype = 2, col = "blue")  # 4

# K-means
res.kmeans <- kmeans(rfm, centers=2, nstart = 1) 
res.kmeanso <- kmeans(rfmo, centers=3, nstart = 1) 
res.kmeans1 <- kmeans(rfm1, centers=4, nstart = 1) 
# centers:	either the number of clusters or a set of initial (distinct) cluster centres. 
#           If a number, a random set of (distinct) rows in x is chosen as the initial centres.
# nstart:   If centers is a number, how many random sets should be chosen?

res.kmeans1$cluster
res.kmeans1$withinss
res.kmeans1$betweenss
res.kmeans1$totss

res.kmeanso$cluster
res.kmeanso$withinss
res.kmeanso$betweenss
res.kmeanso$totss

# Visualize the clustering result
library("factoextra")
fviz_cluster(res.kmeans,           
             data = rfm,              
             geom = c("point"))  

fviz_cluster(res.kmeanso,           
             data = rfmo,              
             geom = c("point"))  

fviz_cluster(res.kmeans1,           
             data = rfm1,              
             geom = "point")  

library(scatterplot3d)
rfm2 <- data.frame(rfm, group=res.kmeans$cluster)
scatterplot3d(rfm2)

s3d <- with(rfm2, scatterplot3d(rfm, color = as.numeric(group), pch = 1))
legend("bottomright",s3d$xyz.convert(0.5, 0.8, 0.5), pch = 1, yjust=0,
       legend = c(1,2), col = seq_along(unique(rfm2$group)))

rfmo2 <- data.frame(rfmo, group=res.kmeanso$cluster)
scatterplot3d(rfmo2)

s3d <- with(rfmo2, scatterplot3d(rfmo, color = as.numeric(group), pch = 1))
legend("bottomright",s3d$xyz.convert(0.5, 0.8, 0.5), pch = 1, yjust=0,
       legend = c(1,2,3), col = seq_along(unique(rfmo2$group)))

rfm12 <- data.frame(scale(R),scale(f),scale(M),row.names =z,group=res.kmeans1$cluster)
scatterplot3d(rfm12)

s3d <- with(rfm12, scatterplot3d(scale(R),scale(f),scale(M), color = as.numeric(group), pch = 1))
legend("bottomright",s3d$xyz.convert(0.5, 0.8, 0.5), pch = 1, yjust=0,
       legend = c(1,2,3,4), col = seq_along(unique(rfm12$group)))

# removing outliers seem to remove group 4

# K-medoid
# Simply replace the "kmeans" function with "pam"
?pam
res.pam <- pam(rfm, 2, nstart = 1) 
res.pam1 <- pam(rfm1, 2, nstart = 1) 

res.pam$clustering
res.pam1$clustering
rfm22 <- data.frame(scale(R),scale(f),scale(M),row.names =z,group=res.pam1$clustering)
scatterplot3d(rfm22)
sd <- with(rfm22, scatterplot3d(scale(R),scale(f),scale(M), color = as.numeric(group), pch = 1))
legend("bottomright",sd$xyz.convert(0.5, 0.8, 0.5), pch = 1, yjust=0,
       legend = c(1,2), col = seq_along(unique(rfm22$group)))
library("factoextra")

fviz_cluster(res.pam,           
             data = rfm,              
             geom = c("point")) 
fviz_cluster(res.pam1,           
             data = rfm1,              
             geom = "point") 

fviz_nbclust(rfm, pam, method = "silhouette") + 
  geom_vline(xintercept = 2, linetype = 2, col = "blue")    

fviz_nbclust(rfm1, pam, method = "silhouette") + 
  geom_vline(xintercept = 2, linetype = 2, col = "blue")    

