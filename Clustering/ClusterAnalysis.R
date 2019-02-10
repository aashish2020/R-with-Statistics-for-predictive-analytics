## Cluster Analysis
## Last updated: November 12, 2018

library(tidyverse)
library(ggrepel)
theme_set(theme_classic())


  # EXAMPLE: Utilities data
setwd("D:/Fall-2018/BAR-Sourav/R_codes/Session13_R_Codes/")
utilities.df <- read.csv("Utilities.csv")

dim(utilities.df)
names(utilities.df)
str(utilities.df)


  ## Visualize scatter plot between Sale  and Fuel_Cost
g1 <- ggplot(utilities.df, aes(x=Fuel_Cost, y=Sales, label=Company)) + 
  geom_point(col = "blue", size = 3.5) +
  labs(x = "Fuel Cost", 
       title = "Relationship Between Fuel Cost and Sales",
       subtitle = "(for 22 Utility Companies)")

g1 + geom_label_repel(color = "dimgrey",
                      nudge_x = 0.1,
                      nudge_y = -0.2)



  ## Calculate Distances

  ## Set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]

  ## Remove the utility column (include only numeric variables in Cluster Analysis)
utilities.df <- utilities.df[,-1]

  ## Compute Euclidean distance
d <- dist(utilities.df, method = "euclidean")

  ## Normalize and calculate distances
utilities.df.norm <- sapply(utilities.df, scale)

  ## Add row names: utilities
row.names(utilities.df.norm) <- row.names(utilities.df) 

  ## Compute normalized distance based on variables Sales and FuelCost
d.norm <- dist(utilities.df.norm[,c(6,8)], method = "euclidean")

  ## ALT: Compute normalized distance based on all 8 variables
d.norm <- dist(utilities.df.norm, method = "euclidean")



  # Hierarchical Clustering (Agglomerative Method)

  ## Generate Dendrogram using multiple methods  
hc1 <- hclust(d.norm, method = "single")
plot(hc1, hang = -1, main = "Single Linkage", col = "cornflowerblue") +
  abline(h=2.7, lty = 1, lwd = 1, col = "firebrick4")

hc2 <- hclust(d.norm, method = "complete")
plot(hc2, hang = -1, main = "Complete Linkage", col = "dodgerblue") +
  abline(h=3.8, lty = 1, lwd = 1, col = "firebrick4")

hc3 <- hclust(d.norm, method = "average")
plot(hc3, hang = -1, main = "Average Linkage", col = "deepskyblue") +
  abline(h= 3.5, lty = 1, lwd = 1, col = "firebrick4")

  ## Check memberships in above clusters
memb1 <- cutree(hc1, k = 6)
memb1

memb2 <- cutree(hc2, k = 6)
memb2

memb3 <- cutree(hc3, k = 6)
memb3



  # k-Means Clustering 

set.seed(2)
km <- kmeans(utilities.df.norm, 6)

  ## Cluster size
km$size

  ## Cluster membership
km$cluster

  ## Cluster centroids
km$centers

  ## Within-cluster sum of squares
km$withinss


  # Generate a Profile Plot

  ## plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", xlab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8), bty = "n")
  ## label x-axes
axis(1, at = c(1:8), labels = names(utilities.df))
  ## plot centroids
for (i in c(1:6)) {
lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                       "tomato", "olivedrab"))
}
  ## name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))

