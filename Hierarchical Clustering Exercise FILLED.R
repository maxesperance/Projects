


Utilities.df <- read.csv("Utilities.csv", header = TRUE)
head(Utilities.df)

### We notice the row names are numbers and we would like to change to the company names. Then we will remove the company name column because we cannot use it in the cluster analysis.

row.names(Utilities.df) <- Utilities.df[,1]
head(Utilities.df)

Utilities.df <- Utilities.df [,-1]

head(Utilities.df)

# Let's normalize the data.
Utilities.df.norm <- scale(Utilities.df)

head(Utilities.df.norm)


# Let's use hierarchical clustering. First let's calculate the distance matrix using the normalzied data. Then run the hierarachical clustering using the average method.
d<-dist(Utilities.df.norm)

c1 <-hclust(d, method="average")

## Plot dendrogram of the cluster

plot(c1, hang = -1)


# Show cluster membership by cutting the dendrogram. Setting k=5 please. What's your observation? Can you provide any explanations?
g1 <- cutree(c1, k = 5) 
g1


