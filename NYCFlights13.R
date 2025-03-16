# Install and load necessary libraries
install.packages("nycflights13")
install.packages("tidyverse")

library(nycflights13)
library(tidyverse)

# Load the nycflights13 dataset
data("flights")

# Data exploration and preprocessing
flights <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  select(dep_delay, arr_delay, distance, air_time, carrier, origin, dest)

# Summary statistics
summary(flights)


# Explore delays by carrier
carrier_delays <- flights %>%
  group_by(carrier) %>%
  summarize(avg_dep_delay = mean(dep_delay), avg_arr_delay = mean(arr_delay))

# Visualize delays by carrier
ggplot(carrier_delays, aes(x = carrier, y = avg_dep_delay)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Average Departure Delay by Carrier", x = "Carrier", y = "Average Departure Delay")

ggplot(carrier_delays, aes(x = carrier, y = avg_arr_delay)) +
  geom_bar(stat = "identity", fill = "lightcoral", color = "black") +
  labs(title = "Average Arrival Delay by Carrier", x = "Carrier", y = "Average Arrival Delay")

# Explore delays by origin
origin_delays <- flights %>%
  group_by(origin) %>%
  summarize(avg_dep_delay = mean(dep_delay), avg_arr_delay = mean(arr_delay))

# Visualize delays by origin
ggplot(origin_delays, aes(x = origin, y = avg_dep_delay)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "Average Departure Delay by Origin", x = "Origin", y = "Average Departure Delay")

ggplot(origin_delays, aes(x = origin, y = avg_arr_delay)) +
  geom_bar(stat = "identity", fill = "lightcoral", color = "black") +
  labs(title = "Average Arrival Delay by Origin", x = "Origin", y = "Average Arrival Delay")

# Scatter plot: Departure Delay vs. Arrival Delay
ggplot(flights, aes(x = dep_delay, y = arr_delay)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Departure Delay vs. Arrival Delay", x = "Departure Delay", y = "Arrival Delay")

# Scatter plot: Distance vs. Arrival Delay
ggplot(flights, aes(x = distance, y = arr_delay)) +
  geom_point(alpha = 0.5, color = "green") +
  labs(title = "Distance vs. Arrival Delay", x = "Distance", y = "Arrival Delay")

# Scatter plot: Air Time vs. Arrival Delay
ggplot(flights, aes(x = air_time, y = arr_delay)) +
  geom_point(alpha = 0.5, color = "orange") +
  labs(title = "Air Time vs. Arrival Delay", x = "Air Time", y = "Arrival Delay")

# Boxplot: Arrival Delays by Carrier
ggplot(flights, aes(x = carrier, y = arr_delay)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  labs(title = "Arrival Delays by Carrier", x = "Carrier", y = "Arrival Delay")

# Boxplot: Arrival Delays by Origin
ggplot(flights, aes(x = origin, y = arr_delay)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Arrival Delays by Origin", x = "Origin", y = "Arrival Delay")

# Install and load necessary libraries
install.packages("cluster")

# Load the library
library(cluster)

# Select relevant columns for clustering
cluster_data <- flights %>%
  select(dep_delay, arr_delay, distance, air_time)

# Standardize the data for clustering
scaled_data <- scale(cluster_data)

# K-means clustering
kmeans_result <- kmeans(scaled_data, centers = 3)  # You can adjust the number of clusters as needed

# Add cluster assignment to the data
flights_clustered <- flights %>%
  mutate(cluster = kmeans_result$cluster)

# Visualize clusters
ggplot(flights_clustered, aes(x = dep_delay, y = arr_delay, color = as.factor(cluster))) +
  geom_point(alpha = 0.5) +
  labs(title = "K-means Clustering of Departure Delay vs. Arrival Delay",
       x = "Departure Delay", y = "Arrival Delay")

# Take a random sample of the data for hierarchical clustering
set.seed(123)
sample_size <- 1000  # Adjust this based on your available memory
sampled_data <- flights[sample(seq_len(nrow(flights)), size = sample_size), ]

# Select relevant columns for clustering
cluster_data <- sampled_data %>%
  select(dep_delay, arr_delay, distance, air_time)

# Standardize the data for clustering
scaled_data <- scale(cluster_data)

# Hierarchical clustering
hierarchical_result <- hclust(dist(scaled_data))
dendrogram <- as.dendrogram(hierarchical_result)

# Visualize hierarchical clustering
plot(dendrogram, main = "Hierarchical Clustering of Flight Data (Subset)")

# Cut the dendrogram to form clusters
cut_dendrogram <- cutree(hierarchical_result, k = 3)  # You can adjust the number of clusters as needed

# Add hierarchical cluster assignment to the data
sampled_data_clustered <- sampled_data %>%
  mutate(cluster = cut_dendrogram)

# Visualize clusters
ggplot(sampled_data_clustered, aes(x = dep_delay, y = arr_delay, color = as.factor(cluster))) +
  geom_point(alpha = 0.5) +
  labs(title = "Hierarchical Clustering of Departure Delay vs. Arrival Delay (Subset)",
       x = "Departure Delay", y = "Arrival Delay")

###########################################################3


# Install and load necessary libraries
install.packages("nycflights13")
install.packages("tidyverse")

library(nycflights13)
library(tidyverse)

# Load the nycflights13 dataset
data("flights")

# Data exploration and preprocessing
flights <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  select(dep_delay, arr_delay, distance, air_time, carrier, origin, dest)

# Split the data into training and testing sets
set.seed(123)
splitIndex <- createDataPartition(flights$arr_delay, p = 0.8, list = FALSE)
train_data <- flights[splitIndex, ]
test_data <- flights[-splitIndex, ]

# Ensure 'dest' has the same levels in both train and test sets
test_data$dest <- factor(test_data$dest, levels = levels(train_data$dest))

# Build a predictive model (linear regression)
model <- lm(arr_delay ~ dep_delay + distance + air_time + carrier + origin + dest, data = train_data)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Evaluate the model
rmse <- sqrt(mean((test_data$arr_delay - predictions)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Visualize the results (scatter plot)
plot(test_data$arr_delay, predictions,
     main = "Actual vs. Predicted Delays",
     xlab = "Actual Delay",
     ylab = "Predicted Delay",
     pch = 16, col = "blue")
abline(0, 1, col = "red", lty = 2)

0..