#Data Analytics Lab 5 -- Isabel Bateman
#SVM Classification and kNN
#Evaluating performance with Precision, Recall, Accuracy, and f1 Metrics

rm(list=ls())
library(readr)
library(rpart)
library(class)
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)
library(caret)
library(e1071)
library(GGally)

#Importing and Cleaning Dataset
wine.data <- read.csv("~/Spring 2026/Data Analytics/Labs/Lab 5/wine.data")
col_names <- c(
  "Class", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_of_Ash",
  "Magnesium", "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols",
  "Proanthocyanins", "Color_Intensity", "Hue",
  "OD280_OD315", "Proline"
)
colnames(wine.data) <- col_names
dataset <- wine.data
dataset$Class <- as.factor(dataset$Class)
dataset <- na.omit(dataset)
#View(dataset)

#Split Train/Test Dataset
N <- nrow(dataset)
train.indexes <- sample(N, 0.7*N)
train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]


#Train 2 SVM Classifiers to Predict Wine Type using a subset of other variables
#Model 1: Linear Kernel
svm.mod0 <- svm(Class ~ Alcohol + Magnesium + Proline, data = train, kernel = 'linear')
X <- train[,c(2,6,14)]
Y <- train[,1]

test.pred <- predict(svm.mod0, test)
cm <- as.matrix(table(Actual = test$Class, Predicted = test.pred))
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diagv = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diagv) / n
accuracy
recall <- diagv/rowsums
precision <- diagv / colsums
f1 <- 2*precision*recall / (precision + recall)

svm.mod0.res <- data.frame(model='linear',precision,recall,f1)
results <- svm.mod0.res

###############################################################################
#Model 2: Radial Kernel
svm.mod1 <- svm(Class ~ Alcohol + Magnesium + Proline, data = train, kernel = 'radial')
X <- train[,c(2,6,14)]
Y <- train[,1]

test.pred <- predict(svm.mod1, test)
cm = as.matrix(table(Actual = test$Class, Predicted = test.pred))
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diagv = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diagv)/n
accuracy
recall = diagv / rowsums 
precision = diagv / colsums
f1 = 2 * precision * recall / (precision + recall) 

svm.mod1.res <- data.frame(model = "radial", precision, recall, f1)
results <- rbind(results,svm.mod1.res)


###############################################################################
#Model 3: kNN based on same features
subset1 <- dataset[,c("Alcohol","Magnesium","Proline","Class")]
subset1 <- na.omit(subset1)
set.seed(6)
s.train <- sample(nrow(subset1), 0.7*nrow(subset1))
s1.train <- subset1[s.train,]
s1.test <- subset1[-s.train,]
knn.predicted <- knn(s1.train[,1:3],s1.test[,1:3],s1.train[,4],k=3)

cm <- as.matrix(table(Actual=test$Class, Predicted=knn.predicted))
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diagv = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diagv)/n
accuracy
recall <- diagv / rowsums 
precision <- diagv / colsums
f1 <- 2 * precision * recall / (precision + recall) 

rf.res <- data.frame(model = "kNN", precision, recall, f1)
results <- rbind(results, rf.res)
results

#The results table outputs the model performance (precision, recall, and f1)
#for each Wine Type (1,2,3). Across all wine classes, the kNN model performed 
#the worst. Using f1 as the overall metric of performance, it appears that
#radial SVM is the best performing model overall, specifically at predicting Class 2.

