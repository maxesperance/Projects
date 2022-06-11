

# Load the data
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

# Run K-means algorithm. We can use set.seed(100) here to make sure all of us get the same result.
set.seed(100)
km <- kmeans(Utilities.df.norm, 6)


# Show cluster membership
km$cluster

# show each cluster size
km$size


### Decide optimal cluster size.
install.packages("factoextra")
library(factoextra)

fviz_nbclust(Utilities.df.norm, kmeans, method = "wss") +  geom_vline(xintercept = 4, linetype = 2)
