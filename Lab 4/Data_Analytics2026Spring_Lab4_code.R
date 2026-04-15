#Lab 4 -- Isabel Bateman
##########################################
### Principal Component Analysis (PCA) ###
##########################################
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

wine <- read_csv("~/Spring 2026/Data Analytics/Labs/Lab 4/wine.data")
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash",
                 "Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols",
                 "Proanthocyanins","Color Intensity","Hue",
                 "Od280/od315 of diluted wines","Proline")
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####
wine$Type <- as.factor(wine$Type)

pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)
ggpairs(wine, ggplot2::aes(colour = Type))

X <- wine[,-1]
Y <- wine$Type
###

#Start Lab 4
#Compute PCs and Plot dataset with 1st and 2nd PCs
Xmat <- as.matrix(X)
Xc <- scale(Xmat, center=T,scale=F)
pcs <- princomp(Xc)
autoplot(pcs, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, scale = 0)

#Identify variables contributing most to 1st PC
pcs$loadings
#PC1 most affected by Proline (1.0), PC2 most affected by Magnesium (0.999)

#Train a classification model (kNN) to predict Wine Type using a subset of 3-4 variables in the original dataset
subset1 <- wine[,c("Proline", "Magnesium", "Alcalinity of ash", "Color Intensity")]
subset1 <- na.omit(subset1)
class.label <- wine$Type

set.seed(6)
s.train <- sample(nrow(subset1), 0.7*nrow(subset1))
s1.train <- subset1[s.train,]
s1.test <- subset1[-s.train,]
cl.train <- class.label[s.train]
knn.predicted <- knn(s1.train, s1.test, cl.train, k=3)

#Train a classification model to predict Wine Type using the data projected onto the first 2 PCs (scores in princomp())
pc.data <- pcs$scores[,1:2]
set.seed(6)
s.train <- sample(nrow(pc.data), 0.7*nrow(pc.data))
pc.train <- pc.data[s.train,]
pc.test <- pc.data[-s.train,]
cl.train <- Y[s.train]
cl.test <- Y[-s.train]
knn.pc.predicted <- knn(pc.train, pc.test, cl.train, k=3)

#Compare classification models using contingency tables and precision/recall/f1 metrics
#Accuracy = Correct Classifications / Total Observations
cl.test <- class.label[-s.train]
cm1 <- table(Predicted = knn.predicted, Actual = cl.test)
accuracy1 <- sum(diag(cm1)) / sum(cm1)
accuracy1

precision <- diag(cm1)/rowSums(cm1)
recall <- diag(cm1)/colSums(cm1)
f1 <- 2 * (precision * recall)/(precision + recall)

#Metrics for First Classification Model
precision
recall
f1

#Metrics for PC Classification Model
cm.pc <- table(Predicted = knn.pc.predicted, Actual = cl.test)
accuracy2 <- sum(diag(cm.pc)) / sum(cm.pc)
accuracy2

precision.pc <- diag(cm.pc) / rowSums(cm.pc)
recall.pc <- diag(cm.pc) / colSums(cm.pc)
f1.pc <- 2 * (precision.pc * recall.pc) / (precision.pc + recall.pc)

precision.pc
recall.pc
f1.pc
