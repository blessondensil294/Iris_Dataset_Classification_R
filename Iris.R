#Loading the iris Dataset
data("iris")

#plotting the iris dataset
plot(iris)

#scale the data
irisscaled <- scale(iris[,-5])

summary(irisscaled)

#K means clustering
kss <- kmeans(irisscaled,3)

#get the cluster details WSS should have higher %
kss

#to display the internal structure of the kss
str(kss)

plot(iris, col = kss$cluster)


#choosing K cluster models based on the no of cluster - fixed point and compares the distance
k <- list()
for(i in 1:10){
  k[[i]] <- kmeans(irisscaled, i)
}

k

#to get the between and total SS percentage to the list and then plot it in the graph
between_totalSS <- list()
for(i in 1:10){
  between_totalSS[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}

between_totalSS

#plot the graphs for hte KSS to get the Elbow graph
plot(1:10, between_totalSS, type = "b", ylab = "Between and Total Sum of Square", xlab = "Clusters")


#plot the cluster based on k-4
for (i in 1:4) {
  plot(iris, col = k[[i]]$cluster)
}

#HIERARCHAL CLUSTERING 
#caluclating the distance of the scaled clusters
d <- dist(irisscaled)

#selecting the fit cluster model
fitH <- hclust(d,"ward.D2")
?hclust

#plotting the cluster in the graph and selecting the broders when k=3
plot(fitH)
rect.hclust(fitH,k=3,border = "green")

#gettiong the vector
cluster <- cutree(fitH,3)
cluster

plot(iris, col=cluster)