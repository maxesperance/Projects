#=======================#
#  clustering Example 2 #
#=======================#

# load data from  csv
dietoriginal <- read.csv("Diet by country.csv", header=TRUE)

head(dietoriginal)
# We find that the rownames are 1,2,3,4.....
# We need to rename row names to country names instead of 1,2,3,4...


row.names(dietoriginal) <-dietoriginal$Country 

head(dietoriginal)


#Now let's remove the first column.
dietoriginal1 <-dietoriginal[,-1]
head(dietoriginal1)


#scale the data 
diet <- scale(dietoriginal1)
head(diet)



# K-mean clustering on meat
set.seed(1234) ## to fix the random starting clusters


cluster1 <- kmeans(diet[,c("WhiteMeat","RedMeat")], centers=6, nstart=10)
# we first cluster based on WhiteMeat and RedMeat
# centers = cluster number
# nstart = 10. This means that R will try 10 different random starting assignments and then select the one with the lowest within cluster variation

#show cluster
cluster1$cluster


# the package cluster is for plotting the cluster
install.packages("cluster")
library(cluster)

#plot cluster
clusplot(diet[,c("WhiteMeat","RedMeat")], cluster1$cluster, main='2D Representation of the Cluster Solution', color=TRUE, shade=TRUE, labels=2, lines=1)
#please play with the number after labels and lines by yourself



### exercise:
# choose other two protein sources and run your clustering. 




# cluster on all protein source
set.seed(1234)
cluster2 <- kmeans(diet[,c("WhiteMeat","RedMeat","Eggs","Milk","Fish","Nuts")], centers=3, nstart=10)
clusplot(diet[,c("WhiteMeat","RedMeat","Eggs","Milk","Fish","Nuts")], cluster2$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)


### Code to conduct elbow chart
library(factoextra)
fviz_nbclust(diet, kmeans, method = "wss") +  geom_vline(xintercept = 6, linetype = 2)


### Hierarchical clustering

d<-dist(diet)
d

c<- hclust(d)
c

plot(c)

g1 <- cutree(c, k = 3)  
g1

rect.hclust(c,k = 3,border="gray")
rect.hclust(c,k = 4, border = "blue")
rect.hclust(c,k = 5, border = "red")
rect.hclust(c,k = 6, border = "red")

# code to create elbow chart for hierarchical
fviz_nbclust(diet, hcut, method = "wss") +  geom_vline(xintercept = 6, linetype = 2)


#Can you draw some interesting conclusion?

