# Example-1: Random Forest

# Install and load required packages
install.packages("randomForest")  # Only if not installed
library(randomForest)

# Load the iris dataset
data(iris)

# Set seed for reproducibility
set.seed(123)

# Split data into training (80%) and testing (30%) sets
sample_index <- sample(1:nrow(iris), 0.8 * nrow(iris))
train_data <- iris[sample_index, ]
test_data <- iris[-sample_index, ]

# Fit Random Forest model on training data
rf_model <- randomForest(Species ~ ., data = train_data, ntree = 100, mtry = 2, importance = TRUE)

# Print model summary
print(rf_model)

# Plot the error rates
plot(rf_model)

# Add a legend manually
legend("topright",
       legend = c("Overall OOB Error", "Setosa", "Versicolor", "Virginica"),
       col = c("black", "red", "green", "blue"),
       lty = 1,   # Line type (solid)
       cex = 0.8) # Text size


# Predict on test data
predictions <- predict(rf_model, newdata = test_data)

# Create confusion matrix to evaluate performance
confusion_matrix <- table(Predicted = predictions, Actual = test_data$Species)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Test Accuracy:", round(accuracy * 100, 2), "%\n")

# Plot variable importance
varImpPlot(rf_model)

#-------------------------------------------------------------------------------------------------------------------------
  
# Example-2

# Load necessary packages
library(MASS)  # For Boston dataset
library(nnet)  # For neural network

# Load the Boston housing data
data(Boston)

#Splitting the Dataset
set.seed(123)  # For reproducibility
n <- nrow(Boston)
train_index <- sample(1:n, size = 0.8 * n)  # 80% training data

train_data <- Boston[train_index, ]
test_data  <- Boston[-train_index, ]

# Fitting NN Model

nn_model <- nnet(medv ~ ., 
                 data = train_data, 
                 size = 5,        # Number of neurons in hidden layer
                 linout = TRUE,   # Linear output for regression
                 decay = 0.01,    # Regularization parameter
                 maxit = 500)     # Maximum iterations


predictions <- predict(nn_model, newdata = test_data)

# Mean Squared Error
mse <- mean((predictions - test_data$medv)^2)
# Root Mean Squared Error
rmse <- sqrt(mse)
# R-squared
sst <- sum((test_data$medv - mean(test_data$medv))^2)
sse <- sum((test_data$medv - predictions)^2)
r_squared <- 1 - (sse/sst)

cat("Test Mean Squared Error (MSE):", round(mse, 2), "\n")
cat("Test Root Mean Squared Error (RMSE):", round(rmse, 2), "\n")
cat("Test R-squared (Model Accuracy):", round(r_squared, 3), "\n")



# Actual vs Predicted plot
plot(test_data$medv, predictions,
     xlab = "Actual MEDV", ylab = "Predicted MEDV",
     main = "Actual vs Predicted House Prices (Scaled Data)",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)

# Neural Network structure plot
library(NeuralNetTools)
plotnet(nn_model)

# Important predictors
olden(nn_model)


