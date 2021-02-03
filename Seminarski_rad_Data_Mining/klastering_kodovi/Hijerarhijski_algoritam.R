#Read data
iris <- read.csv('iris.data',header = FALSE)

#Insert column names
feature_name <- c('sepal_length','sepal_width','petal_length','petal_width', 'species')
colnames(iris) <- feature_name

#Plot pair of values with their species
#install.packages("ggplot2")
library(ggplot2)
ggplot(iris, aes(iris$petal_length, iris$petal_width, colour = iris$species)) + 
  geom_point()

#Build distance matrix with euclidean distance
dist_mat <- dist(iris[, 3:4], method = 'euclidean')

#Average linkage method - create dendrogram
clusters <- hclust(dist_mat, method = 'average')
plot(clusters)

#Cut dendrogram into 3 clusters
clusterCut <- cutree(clusters, 3)

#Show difference between cluster cut and original dataset classes
table(clusterCut, iris$species)


#Implement silhouette
#install.packages("factoextra")
library("factoextra")

library("dplyr")
clusterRes <- iris[, 3:4] %>%
  eclust("hclust", k = 3, graph = FALSE)

fviz_silhouette(clusterRes)

# Silhouette width of observations
sil <- clusterRes$silinfo$widths[, 1:3]

# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]
