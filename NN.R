# Load required libraries
library(readxl)
library(nnet)
library(neuralnet)

# Read the Excel file
df <- read_excel("D:/IIT/Machine Learning/CW/CW/ExchangeUSD.xlsx")

# Ensure that the column names are appropriate
names(df)[3] <- "USDEUR"

# Remove "YYYY/MM/DD" and "Wdy" columns
df <- df[, -c(1, 2)]

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

# Create lagged variables up to lag 4 for the exchange rate column
for (i in 1:4) {
  dfNoOutliers[[paste0("Lag", i)]] <- c(rep(NA, i), head(dfNoOutliers$USDEUR, -i))
}

# Remove rows with NA values introduced by lagging
dfNoOutliers <- dfNoOutliers[complete.cases(dfNoOutliers), ]

# Rename column names
inputVars <- c("Lag1", "Lag2", "Lag3", "Lag4")  # Input variables
outputVar <- "USDEUR"  # Output variable

# Normalize the dataset using Min-Max method
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to all columns except the output variable
dfFinal <- as.data.frame(lapply(dfNoOutliers[, inputVars], normalize))

# Add the output variable to the normalized dataframe
dfFinal <- cbind(dfFinal, USDEUR = dfNoOutliers$USDEUR)

# Split the dataset
trainData <- dfFinal[1:400, ]
testData <- dfFinal[401:nrow(dfFinal), ] 

# Function to build MLP models with different configurations
buildMlpModel <- function(trainData, inputVars, outputVar, hiddenLayers, linearOutput, activationFunction) {
  formula <- as.formula(paste(outputVar, "~", paste(inputVars, collapse = " + ")))
  model <- neuralnet(formula = formula,
                     data = trainData,
                     hidden = hiddenLayers,
                     linear.output = linearOutput,
                     act.fct = activationFunction)
  return(model)
}

# List of configurations
hiddenLayers <- list(c(5), c(10), c(15), c(5, 5), c(10, 10), c(15, 15), c(5, 10), c(10, 5), c(5, 6), c(10, 4), c(4, 15), c(5, 8), c(5, 2), c(3, 4), c(13, 12))
linearOutputs <- c(TRUE, FALSE)
activationFunctions <- c("logistic", "tanh")

# Build 15 MLP models with different configurations
mlpModels <- list()
counter <- 1
for (hiddenLayer in hiddenLayers) {
  for (linearOutput in linearOutputs) {
    for (activationFunction in activationFunctions) {
      modelName <- paste("exchangeRatesModel", counter, sep = "")
      mlpModels[[modelName]] <- buildMlpModel(trainData, inputVars, outputVar, hiddenLayer, linearOutput, activationFunction)
      counter <- counter + 1
      if (counter > 15) {
        break
      }
    }
    if (counter > 15) {
      break
    }
  }
  if (counter > 15) {
    break
  }
}

# Function to calculate MAPE
calculateMape <- function(actual, predicted) {
  if (length(actual) == 0 | length(predicted) == 0) {
    return(NA)
  }
  mape <- mean(abs((actual - predicted) / actual)) * 100
  return(mape)
}

# Function to calculate sMAPE
calculateSmape <- function(actual, predicted) {
  if (length(actual) == 0 | length(predicted) == 0) {
    return(NA)
  }
  smape <- 2 * mean(abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
  return(smape)
}

# Calculate metrics for each model
metrics <- data.frame(Model = character(), RMSE = double(), MAE = double(), MAPE = double(), sMAPE = double(), stringsAsFactors = FALSE)

for (i in 1:length(mlpModels)) {
  modelName <- paste("exchangeRatesModel", i, sep = "")
  model <- mlpModels[[modelName]]
  
  # Make predictions
  predicted <- predict(model, newdata = testData)
  
  if (length(predicted) == 0) {
    next
  }
  
  # Denormalize the predictions
  denormalize <- function(x, originalData) {
    minVal <- min(originalData)
    maxVal <- max(originalData)
    return(x * (maxVal - minVal) + minVal)
  }
  
  denormalizedPredictions <- denormalize(predicted, dfNoOutliers$USDEUR)
  
  # Calculate RMSE
  rmse <- sqrt(mean((testData$USDEUR - denormalizedPredictions)^2))
  
  # Calculate MAE
  mae <- mean(abs(testData$USDEUR - denormalizedPredictions))
  
  # Calculate MAPE
  mape <- calculateMape(testData$USDEUR, denormalizedPredictions)
  
  # Calculate sMAPE
  smape <- calculateSmape(testData$USDEUR, denormalizedPredictions)
  
  # Store metrics
  metrics[i, ] <- list(modelName, rmse, mae, mape, smape)
}

# View metrics
print(metrics)

# Plot actual vs predicted exchange rates for model 3
model3 <- mlpModels[["exchangeRatesModel3"]]
predictedModel3 <- predict(model3, newdata = testData)
denormalizedPredictionsModel3 <- denormalize(predictedModel3, dfNoOutliers$USDEUR)

plot(testData$USDEUR, denormalizedPredictionsModel3,
     main = "Actual vs Predicted Exchange Rates - Model 3",
     xlab = "Actual Exchange Rates",
     ylab = "Predicted Exchange Rates",
     col = "blue",
     pch = 20)
abline(0, 1, col = "red")

# Plot actual vs predicted exchange rates for model 4
model4 <- mlpModels[["exchangeRatesModel4"]]
predictedModel4 <- predict(model4, newdata = testData)
denormalizedPredictionsModel4 <- denormalize(predictedModel4, dfNoOutliers$USDEUR)

plot(testData$USDEUR, denormalizedPredictionsModel4,
     main = "Actual vs Predicted Exchange Rates - Model 4",
     xlab = "Actual Exchange Rates",
     ylab = "Predicted Exchange Rates",
     col = "blue",
     pch = 20)
abline(0, 1, col = "red")
