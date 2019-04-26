# Import tidyverse libraries
library(tidyverse)

# Read in the datasets created from the Python notebook
# Also applying some default parameters for plotting
data_2 <- as_tibble(read.csv("data_2.csv")) %>%
  mutate(shape = 1, size = 1)
data_6 <- as_tibble(read.csv("data_6.csv")) %>%
  mutate(shape = 1, size = 1)

# Set a seed for reproducibility
set.seed(0)

## K-Means: 2 Clusters ##

# Fit and predict the clusters
km_2 <- kmeans(data_2[c("x", "y")], 2)

# Display the centers
print("Cluster centers:")
print(km_2$centers)

# Turn the centers into a dataset and assign a shape variable
km_2_centers <- as_tibble(km_2$centers) %>%
  mutate(pred = 99, shape = 2, size = 2)

# Make a new tibble for the predicted values
data_2p <- data_2 %>%
  mutate(pred = km_2$cluster) %>%
  bind_rows(km_2_centers)

# Plot the original
data_2 %>%
  ggplot(aes(x, y)) +
  geom_point(aes(color = factor(z), shape = factor(shape), size = factor(size))) +
  scale_color_manual(values = c("red", "green")) +
  scale_shape_manual(values = c(16)) +
  scale_size_manual(values = c(2)) +
  theme(legend.position = "none") +
  ggtitle("K Means with 2 Clusters: Original Groupings") +
  ggsave("R_Original_2.png", width = 9, height = 6)

# Plot the predicted
data_2p %>%
  ggplot(aes(x, y)) +
  geom_point(aes(color = factor(pred), shape = factor(shape), size = factor(size))) +
  scale_color_manual(values = c("red", "green", "black")) +
  scale_shape_manual(values = c(16, 18)) +
  scale_size_manual(values = c(2, 4)) +
  theme(legend.position = "none") +
  ggtitle("K Means with 2 Clusters: Predicted Groupings") +
  ggsave("R_K_pred_2.png", width = 9, height = 6)

## K-Means: 6 Clusters ##
km_6 <- kmeans(data_6[c("x", "y")], 6)

# Display the centers
print("Cluster centers:")
print(km_6$centers)

# Add the predicted values back to data_2
# Turn the centers into a dataset and assign a shape variable
km_6_centers <- as_tibble(km_6$centers) %>%
  mutate(pred = 99, shape = 2, size = 2)

# Make a new tibble for the predicted values
data_6p <- data_6 %>%
  mutate(pred = km_6$cluster) %>%
  bind_rows(km_6_centers)

# Plot the original
data_6 %>%
  ggplot(aes(x, y)) +
  geom_point(aes(color = factor(z), shape = factor(shape), size = factor(size))) +
  scale_color_manual(values = c("red", "green", "blue", "purple", "grey", "lawngreen")) +
  scale_shape_manual(values = c(16)) +
  scale_size_manual(values = c(2)) +
  theme(legend.position = "none") +
  ggtitle("K Means with 6 Clusters: Original Groupings") +
  ggsave("R_Original_6.png", width = 9, height = 6)

# Plot the predicted
data_6p %>%
  ggplot(aes(x, y)) +
  geom_point(aes(color = factor(pred), shape = factor(shape), size = factor(size))) +
  scale_color_manual(values = c("red", "green", "blue", "purple", "grey", "lawngreen", "black")) +
  scale_shape_manual(values = c(16, 18)) +
  scale_size_manual(values = c(2, 4)) +
  theme(legend.position = "none") +
  ggtitle("K Means with 6 Clusters: Predicted Groupings") +
  ggsave("R_K_pred_6.png", width = 9, height = 6)

## Hierarchical Clustering ##

# First demonstrate the dendogram
subset <- data_2[1:30, 1:2]
subset_dist <- dist(subset, method = "euclidean")

hc <- hclust(subset_dist, method = "ward.D")

png("Rdendogram.png", height = 6, width = 9, units = "in", res = 200)
plot(hc)
dev.off()

# Now proceed with the replication - start with 2 clusters

# Measure the distancees
data_2_dist <- dist(data_2[, 1:2], method = "euclidean")

# Cluster
hc_2 <- hclust(data_2_dist, method = "ward.D")

# Now we cut the tree
data_2_cut <- cutree(hc_2, k = 2)

# Add in the predicted groupings
data_2_hc <- data_2 %>%
  mutate(pred = data_2_cut)

# Plot
data_2_hc %>%
  ggplot(aes(x, y)) +
  geom_point(aes(color = factor(pred), shape = factor(shape), size = factor(size))) +
  scale_color_manual(values = c("red", "green")) +
  scale_shape_manual(values = c(16)) +
  scale_size_manual(values = c(2)) +
  theme(legend.position = "none") +
  ggtitle("Hierarchical Clustering with 2 Clusters: Predicted Groupings") +
  ggsave("R_HC_2.png", width = 9, height = 6)

# Now move to 6 clusters
data_6_dist <- dist(data_6[, 1:2], method = "euclidean")

# Cluster
hc_6 <- hclust(data_6_dist, method = "ward.D")

# Now we cut the tree
data_6_cut <- cutree(hc_6, k = 6)

# Add in the predicted groupings
data_6_hc <- data_6 %>%
  mutate(pred = data_6_cut)

# Plot
data_6_hc %>%
  ggplot(aes(x, y)) +
  geom_point(aes(color = factor(pred), shape = factor(shape), size = factor(size))) +
  scale_color_manual(values = c("red", "green", "blue", "purple", "grey", "lawngreen")) +
  scale_shape_manual(values = c(16)) +
  scale_size_manual(values = c(2)) +
  theme(legend.position = "none") +
  ggtitle("Hierarchical Clustering with 6 Clusters: Predicted Groupings") +
  ggsave("R_HC_6.png", width = 9, height = 6)
