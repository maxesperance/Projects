# Conducting a cluster analysis

# Load data

data(mtcars)
?mtcars
head(mtcars)

mtcars1 <- mtcars[, c(1:4, 6:7, 9:11)]  
head(mtcars1)

mtcars2 <-scale(mtcars1)  #normalize the data
head(mtcars2)



# We'll use hierarchical clustering
# Need distance matrix (dissimilarity matrix)

d<-dist(mtcars2)
d


# Use distance matrix for clustering, hclust uses the complete linkage (i.e., maximum distance) method.

c<- hclust(d)
c

# We can also change the clustering linkage to "average", "centroid", "single", and "complete".
c2 <-hclust(d, method="average")
c2

# Plot dendrogram of clusters. 
plot(c)


plot(c2)

# Put observations in groups
# Need to specify either k = groups or h = height

g3 <- cutree(c, k = 3)  
g3


g4 <- cutree(c2, k = 3)  
g4
# "g3" = "groups 3"
# cutree(c, h = 5) will give same result


# Or do several levels of groups at once
# "gm" = "groups/multiple"


gm <- cutree(c, k = 2:5) 
gm

# Draw boxes around clusters

rect.hclust(c,k = 2,border="gray")
rect.hclust(c,k = 3, border = "blue")
rect.hclust(c,k = 4, border = "red")

# Run k-means clustering algorithm

km <- kmeans(mtcars2, 3)
km

# show cluster membership
km$cluster

# show size of each cluster
km$size

# Graph based on k-means

library(cluster)
clusplot(mtcars2, km$cluster, color = TRUE, lines = 3, labels = 2)

#For more information about this graph, please refer to https://www.youtube.com/watch?v=_UVHneBUBW0 


rm(list = ls())  # Clean up

