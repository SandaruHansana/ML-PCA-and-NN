# Load required libraries
library(readxl)
library(tidyverse)
library(cluster)
library(NbClust)
library(datasets)
library(factoextra)
library(cluster)
library(fpc) 

# Read the CSV file
dfChangeRate <- read_excel("D:/IIT/Machine Learning/CW/CW/Whitewine_v6.xlsx")

# Remove the "quality" feature
dfChangeRate <- dfChangeRate[, -which(names(dfChangeRate) == "quality")]

# Identify potential outliers using Tukey's method
outlierIndicesChangeRate <- vector("list", length = ncol(dfChangeRate))
for (i in seq_along(dfChangeRate)) {
  QChangeRate <- quantile(dfChangeRate[[i]], c(0.25, 0.75), na.rm = TRUE)
  IQRChangeRate <- QChangeRate[2] - QChangeRate[1]
  lowerBoundChangeRate <- QChangeRate[1] - 1.5 * IQRChangeRate
  upperBoundChangeRate <- QChangeRate[2] + 1.5 * IQRChangeRate
  outlierIndicesChangeRate[[i]] <- which(dfChangeRate[[i]] < lowerBoundChangeRate | dfChangeRate[[i]] > upperBoundChangeRate)
}

# Combine outlier indices
allOutlierIndicesChangeRate <- unique(unlist(outlierIndicesChangeRate))

# Remove outliers
dfNoOutliersChangeRate <- dfChangeRate[-allOutlierIndicesChangeRate, ]

# Scaling the data
dfScaledChangeRate <- scale(dfNoOutliersChangeRate)

# Perform PCA
pcaResultChangeRate <- prcomp(dfScaledChangeRate, scale = TRUE)

# Eigenvalues and eigenvectors from PCA
eigenvaluesPcaChangeRate <- pcaResultChangeRate$sdev^2
eigenvectorsPcaChangeRate <- pcaResultChangeRate$rotation

# Print eigenvalues, eigenvectors, and summary
print(eigenvaluesPcaChangeRate)
print(eigenvectorsPcaChangeRate)
summary(pcaResultChangeRate)

# Cumulative variance explained by each PC
cumulativeVariance <- cumsum(pcaResultChangeRate$sdev^2 / sum(pcaResultChangeRate$sdev^2) * 100)
print(cumulativeVariance)

# Find the index of the PC that provides at least 85% cumulative variance
numPcs <- which(cumulativeVariance >= 85)[1]
print(numPcs)

# Select the PCs and Create a new dataframe with selected principal components
selectedPcs <- pcaResultChangeRate$x[, 1:numPcs]
newDf <- as.data.frame(selectedPcs)
print(head(newDf))

boxplot(dfChangeRate)
boxplot(newDf)

# NbClust
nbResults <- NbClust(newDf, min.nc = 2, max.nc = 10, method = "kmeans", index = "all")

# Elbow Method
wcss <- numeric(10)
for (i in 1:10) {
  kmeansFit <- kmeans(newDf, centers = i)
  wcss[i] <- kmeansFit$tot.withinss
}
# Plotting the Elbow Method
plot(1:10, wcss, type = "b", xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares (WCSS)", main = "Elbow Method")

# Silhouette score
silhouetteScore <- function(k){
  km <- kmeans(newDf, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(newDf))
  mean(ss[, 3])
}

k <- 2:10
avgSil <- sapply(k, silhouetteScore)

plot(k, avgSil, type='b', xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

# Gap statistics
fviz_nbclust(newDf, kmeans, method = 'gap_stat')

# k-means clustering
k <- 2
kmeansWine <- kmeans(newDf, centers = k, nstart = 10)
print(kmeansWine)

# Extract cluster centers
centers <- kmeansWine$centers
clusterAssignments <- kmeansWine$cluster

# Calculate the total sum of squares (TSS)
TSS <- sum(apply(newDf, 1, function(x) sum((x - mean(newDf))^2)))

# Calculate the between-cluster sum of squares (BSS)
BSS <- sum(apply(centers, 1, function(c) sum((c - mean(newDf))^2))) * k

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
silhouetteScores <- silhouette(kmeansWine$cluster, dist(newDf))

# Plot silhouette plot
plot(silhouetteScores, col = kmeansWine$cluster, border = NA)

# Add average silhouette width to the plot
abline(h = mean(silhouetteScores[, "sil_width"]), lty = 2)

# Average silhouette width score
avgSilhouetteWidth <- mean(silhouetteScores[, "sil_width"])
print(avgSilhouetteWidth)

# Calculate Calinski-Harabasz index using cluster.stats
calinski_harabasz <- cluster.stats(dist(newDf), kmeansWine$cluster)$ch
print("Calinski-Harabasz Index:")
print(calinski_harabasz)
