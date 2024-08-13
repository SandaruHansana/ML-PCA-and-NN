# Load required libraries
library(readxl)
library(tidyverse)
library(cluster)
library(NbClust)
library(datasets)
library(factoextra)
library(cluster)

# Read the CSV file
df <- read_excel("D:/IIT/Machine Learning/CW/CW/Whitewine_v6.xlsx")

# Remove the "quality" feature
df <- df[, -which(names(df) == "quality")]

# Identify potential outliers using Tukey's method
outlierIndices <- vector("list", length = ncol(df))
for (i in seq_along(df)) {
  Q <- quantile(df[[i]], c(0.25, 0.75), na.rm = TRUE)
  IQR <- Q[2] - Q[1]
  lowerBound <- Q[1] - 1.5 * IQR
  upperBound <- Q[2] + 1.5 * IQR
  outlierIndices[[i]] <- which(df[[i]] < lowerBound | df[[i]] > upperBound)
}

# Combine outlier indices
allOutlierIndices <- unique(unlist(outlierIndices))

# Remove outliers
dfNoOutliers <- df[-allOutlierIndices, ]

# Check if any outliers were removed
if (length(allOutlierIndices) > 0) {
  cat("Removed", length(allOutlierIndices), "outliers.\n")
} else {
  cat("No outliers were found.\n")
}

# Scaling the data
dfScaled <- scale(dfNoOutliers)

# Print the outliers chart
boxplot(df)

boxplot(dfScaled)

# Check for missing values, NaNs, or Infs
if (any(is.na(dfScaled)) || any(!is.finite(dfScaled))) {
  # Handle missing values, NaNs, or Infs
  # One simple method is to remove them
  dfScaled <- dfScaled[complete.cases(dfScaled), ]
  cat("Removed missing values, NaNs, or Infs.\n")
}

# Print the cleaned and scaled dataset
print(dfScaled)
class(dfScaled)

# NbClust
nbResults <- NbClust(dfScaled, min.nc = 2, max.nc = 10, method = "kmeans", index = "all")

# Elbow Method
wcss <- numeric(10)
for (i in 1:10) {
  kmeansFit <- kmeans(dfScaled, centers = i)
  wcss[i] <- kmeansFit$tot.withinss
}

# Plotting the Elbow Method
plot(1:10, wcss, type = "b", xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares (WCSS)", main = "Elbow Method")

# Silhouette score
silhouetteScore <- function(k){
  km <- kmeans(dfScaled, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(dfScaled))
  mean(ss[, 3])
}

k <- 2:10
avgSil <- sapply(k, silhouetteScore)

plot(k, avgSil, type='b', xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

# Gap statistics
fviz_nbclust(dfScaled, kmeans, method = 'gap_stat')


# k-means clustering
k <- 2
kmeansWine <- kmeans(dfScaled, centers = k, nstart = 10)
print(kmeansWine)

# Extract cluster centers
centers <- kmeansWine$centers
clusterAssignments <- kmeansWine$cluster

# Calculate the total sum of squares (TSS)
TSS <- sum(apply(dfScaled, 1, function(x) sum((x - mean(dfScaled))^2)))

# Calculate the between-cluster sum of squares (BSS)
BSS <- sum(apply(centers, 1, function(c) sum((c - mean(dfScaled))^2))) * k

# Calculate the within-cluster sum of squares (WSS)
WSS <- sum(kmeansWine$withinss)

# Calculate the ratio of BSS to TSS
BSS_TSS_ratio <- BSS / TSS

# Print the calculated indices
cat("Total Sum of Squares (TSS):", TSS, "\n")
cat("Between-Cluster Sum of Squares (BSS):", BSS, "\n")
cat("Within-Cluster Sum of Squares (WSS):", WSS, "\n")
cat("Ratio of BSS to TSS:", BSS_TSS_ratio, "\n")

# Get cluster centers
centers <- kmeansWine$centers
print("Cluster Centers:")
print(centers)


# Calculate silhouette scores
silhouetteScores <- silhouette(kmeansWine$cluster, dist(dfScaled))

# Plot silhouette plot
plot(silhouetteScores, col = kmeansWine$cluster, border = NA)

# Add average silhouette width to the plot
abline(h = mean(silhouetteScores[, "sil_width"]), lty = 2)

# Average silhouette width score
avgSilhouetteWidth <- mean(silhouetteScores[, "sil_width"])
print(avgSilhouetteWidth)
