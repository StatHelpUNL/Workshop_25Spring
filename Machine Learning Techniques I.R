
#-----------------------BEGINNING OF LOGISTIC REGRESSION------------------------------------------------------
#Installs packages if they are not already installed. This package has the data we will be using.
if (!requireNamespace("ISLR", quietly = TRUE)) {
  install.packages("ISLR")
}

#functions from this library will be used for model fitting and training.
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
#load packages
library(ISLR)
library(caret)

#load data
data<-Default


#How R is going to see the categorical variables
#contrasts(data$student)
#contrasts(data$default)

#we want to make sure that R sees the student variable as a factor
data$student<-as.factor(data$student)

#randomly split data into training and test
set.seed(1253)  # For reproducibility
train_size <- floor(0.8 * nrow(data)) # Calculate the sample size for training set (80% of the data)

train_indices <- sample(seq_len(nrow(data)), size = train_size) # Generate a random sample of row indices for the training set using the sample function. Specify how many row indeces with size parameter.

# Create training and testing sets
train_data <- data[train_indices, ]#use random indeces for  training set
test_data <- data[-train_indices, ]#use remaining indeces for test set


#create validation function
#specify the type of training method used & the number of folds
ctrlspecs<-trainControl(method = "cv", number = 5,
                        savePredictions = "all",
                        classProbs = TRUE)


#specify a random seed for the sample folds used in the cross-validation for reproducibility.
set.seed(422)

#model fitting
model<-train(default ~ student + balance + income, data= train_data, method="glm", family = binomial, trControl = ctrlspecs)


summary(model) #The greater the Residual deviance the better your model

print(model)

#Accuracy can be misleading especially when one class is more frequent than the other
#Kappa measures agreement between the model’s predictions and the actual labels, correcting for chance

#kappa Rules for iterpretation
# 0.81 to 1   (Almost perfect agreement)
# 0.61 to 0.8 (susbstantial agreement)
# 0.41 to 0.6 (Moderately agreement)
# 0.21 to 0.4 (Fairly good)
# 0.0 to 0.2 (Slight)
# 0.41 to 0.6 (Moderately good)




#---------Model Evaluations----------------

# Predict the outcomes on the test data
predictions <- predict(model, newdata = test_data)


#create a confusion matrix
conf_matrix<-confusionMatrix(data = predictions, test_data$default)

# Print confusion matrix to get the necessary values
print(conf_matrix)

# Extract confusion matrix elements
TP <- conf_matrix$table[2, 2]  # True Positives (Yes predicted as Yes)
TN <- conf_matrix$table[1, 1]  # True Negatives (No predicted as No)
FP <- conf_matrix$table[1, 2]  # False Positives (No predicted as Yes)
FN <- conf_matrix$table[2, 1]  # False Negatives (Yes predicted as No)

# Calculate Precision, Sensitivity, Specificity, and F-1 Score
precision <- TP / (TP + FP)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)

# Print the metrics
cat("Precision: ", precision, "\n")
cat("Sensitivity: ", sensitivity, "\n")
cat("Specificity: ", specificity, "\n")
cat("F-1 Score: ", f1_score, "\n")


#-------------END OF LOGISTIC REGRESSION----------------------------------------------------------



#-------------------------BEGINNING OF NAIVE BAYES------------------------------

#Installs packages if they are not already installed
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071") # class library is used for implementing the knn
}

library(e1071)


#load data
#We will be using the famous Iris data set which is a dataset of flowers
iris<-iris

##Randomly splitting the data into training and test set.
#because there are 50 observations for each class, we want to make sure that the training set has equal percentage of each class. 
set.seed(4000) # we want to make sure that our sample is reproducible.
setosa<- rbind(iris[iris$Species=="setosa",]) # get all the setosa classes.
versicolor<- rbind(iris[iris$Species=="versicolor",]) # get all the versicolor classes.
virginica<- rbind(iris[iris$Species=="virginica",]) # get all the virginica classes.


ind <- sample(1:nrow(setosa), nrow(setosa)*0.8) #get a random sample of 80% (40 total) row indexes out of 50 rows.  
iris.train<- rbind(setosa[ind,], versicolor[ind,], virginica[ind,]) #use these sampled row indeces to sample 80% (or 40 observation) from each class to create the training set.
iris.test<- rbind(setosa[-ind,], versicolor[-ind,], virginica[-ind,]) #Use the remaining rows that were not sampled to form the test set
iris[,1:4] <- scale(iris[,1:4]) #scales the features to improve accuracy



## create a naive bayes model
model <- naiveBayes(Species~ ., data = iris.train) #Fit the model with training set
model

#show the confusion matrix
confusion_matrix<-table(predict(model, iris), iris[,5])
confusion_matrix

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 2)))



#-------------------------END OF NAIVE BAYES-----------------------------------





#-------------BEGINNING OF K-NEAREST NEIGHBOR---------------------------------------

#Installs packages if they are not already installed
if (!requireNamespace("class", quietly = TRUE)) {
  install.packages("class") # class library is used for implementing the knn
}

#Installs packages if they are not already installed
if (!requireNamespace("class", quietly = TRUE)) {
  install.packages("class") # class library is used for implementing the knn
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}



library(class)
library(caret)  # for confusion matrix and model evaluation
library(ggplot2)



#load data
#We will be using the famous Iris data set which is a dataset of flowers
iris<-iris

##Randomly splitting the data into training and test set.
#because there are 50 observations for each class, we want to make sure that the training set has equal percentage of each class. 
set.seed(12420) # we want to make sure that our sample is reproducible.
setosa<- rbind(iris[iris$Species=="setosa",]) # get all the setosa classes.
versicolor<- rbind(iris[iris$Species=="versicolor",]) # get all the versicolor classes.
virginica<- rbind(iris[iris$Species=="virginica",]) # get all the virginica classes.


ind <- sample(1:nrow(setosa), nrow(setosa)*0.8) #get a random sample of 80% (40 total) row indexes out of 50 rows.  
iris.train<- rbind(setosa[ind,], versicolor[ind,], virginica[ind,]) #use these sampled row indeces to sample 80% (or 40 observation) from each class to create the training set.
iris.test<- rbind(setosa[-ind,], versicolor[-ind,], virginica[-ind,]) #Use the remaining rows that were not sampled to form the test set
iris[,1:4] <- scale(iris[,1:4]) #scales the features to improve accuracy


#Finding the optimum k to use
error <- c()
# we fit knn models and try k values from 1 to 15 to find the optimum k
for (i in 1:15)
{
  # This code implements the knn with the training set, using different values of k running from 1 to 15.
  knn.fit <- knn(train = iris.train[,1:4], test = iris.test[,1:4], cl = iris.train$Species, k = i)
  error[i] = 1- mean(knn.fit == iris.test$Species) # percentage of missclassification errors for each k.
}

#plot to see which k produces the lowest misclassification error. 
ggplot(data = data.frame(error), aes(x = 1:15, y = error)) +
  geom_line(color = "black")


## We want to use a simple model so we choose the lowest k for prediction
##PREDICTION
iris_pred <- knn(train = iris.train[,1:4], test = iris.test[,1:4], cl = iris.train$Species, k=4) 

conf_matrix<-table(iris.test$Species,iris_pred)
conf_matrix
Acc = 1-error[4]

cat("Prediction accuracy is ", Acc, "\n")

#---------------END OF K-NEAREST NEIGHBOR--------------------------------------------