library(ggplot2)

#Read data
iris <- read.csv('iris.data',header = FALSE)

#Insert column names
feature_name <- c('sepal_length','sepal_width','petal_length','petal_width', 'species')
colnames(iris) <- feature_name

#summary statistic of all the 4 variables
summary(iris)

#top rows of data
head(iris)

#assessing of clustering tendency
library(factoextra)
tendency <- get_clust_tendency(iris[,-5], 100, graph = TRUE,
                               gradient = list(low = "blue", mid = "white", high = "red"),
                               seed = 123)

tendency$hopkins_stat


#storing  atributte species in another variable
iris2<- iris[,c(1,2,3,4)]
iris.class<- iris[,"species"]
#head(iris.new)
#head(iris.class)

#the petal length and width show 3 clusters
sapply(iris[,-5], var)
iris2 = iris[,-5]

#finding the optimum number of clusters using wss
set.seed(200)
k.max <- 10
wss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 20)$tot.withinss})
#the best clustering with minimun wss
wss
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")


#finding the optimum number of clusters using bss
set.seed(200)
k.max <- 10
bss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 20)$betweenss})
#the best clustering with maximum bss
bss
plot(1:k.max,bss, type= "b", xlab = "Number of clusters(k)", ylab = "Between cluster sum of squares")


#Implement silhouette
#install.packages("factoextra")

library("dplyr")
# calculating euclidean distance
distance = dist(iris2)

library(cluster)
kmeans.model = kmeans(iris[,3:4], centers = 3, nstart = 50)
sil = silhouette(kmeans.model$cluster, distance)
plot(sil, main = "Silhoutte Analysis of Iris Data Set", col = c("green", "blue", "purple"), ylab="Number of Clusters", xlab="Silhouette Range or Distance")
#kmeans.model$size # gives no. of records in each cluste
#kmeans.model$centers # gives value of cluster center datapoint value(3 centers for k=3)
#kmeans.model$cluster  #gives cluster vector showing the custer where each record falls
fviz_silhouette(sil)

si.sum <- summary(sil)
si.sum

#verfying results
# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris[c(3,4)], col=kmeans.model$cluster, xlab = "Petal Length", ylab = "Petal Width")
# Plot to see how Petal.Length and Petal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris[c(3,4)], col=iris.class)

table(kmeans.model$cluster,iris.class) #output

kmeans.model  #summary of a model

