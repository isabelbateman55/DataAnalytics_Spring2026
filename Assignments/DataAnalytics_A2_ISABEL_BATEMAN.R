##Assignment 2 -- Isabel Bateman
rm(list=ls())
library(readr)
library("ggplot2")
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)

library(rpart)
library(rpart.plot)
library(class)

epi_data <- read_csv("~/Spring 2026/Data Analytics/Assignments/Assignment 2/epi_results_2024_pop_gdp.csv")
dataset <- epi_data
View(dataset)

#Variable Distributions: using variable eco.new
eco.new <- epi_data$ECO.new
eco.new <- eco.new[!is.na(eco.new)]

#1.1 Histogram w/ Density Lines
mu <- mean(eco.new)
sigma <- sd(eco.new)
x1 <- seq(20,90,1)
d1 <- dnorm(x1,mu,sigma,log=FALSE)
hist(eco.new,x1,prob=TRUE)
lines(x1,d1)

#1.2 Boxplots for each region
region <- epi_data$region
boxplot(eco.new ~ region, main="ECO.new by Region", xlab="Region",ylab="ECO.new")

#2 Derive 2 subsets for any 2 regions of our choice
subsaharan <- dataset[dataset$region=="Sub-Saharan Africa",]
latinamerica <- dataset[dataset$region=="Latin America & Caribbean",]

#2.1 Histograms of the above variable for each region
subsaharan_eco.new <- subsaharan$ECO.new
mu_a <- mean(subsaharan_eco.new)
sd_a <- sd(subsaharan_eco.new)
x1_a <- seq(20,80,1)
d1_a <- dnorm(x1_a, mu_a, sd_a, log=FALSE)
hist(subsaharan_eco.new, x1_a, prob=TRUE)
lines(x1_a,d1_a)

latinamerica_eco.new <- latinamerica$ECO.new
mu_b <- mean(latinamerica_eco.new)
sd_b <- sd(latinamerica_eco.new)
x1_b <- seq(35,65,1)
d1_b <- dnorm(x1_b, mu_b, sd_b, log=FALSE)
hist(latinamerica_eco.new, x1_b, prob=TRUE)
lines(x1_b, d1_b)

#2.2 QQ plot for the variable between above 2 subsets
qqplot(subsaharan_eco.new,latinamerica_eco.new,main="ECO.new Comparison between Sub-Saharan Africa, Latin America & Caribbean")

#Linear Models: For each subset...
#3.1 Plot population and GDP (separately) against the above variable w/ best fit line
ggplot(subsaharan, aes(x=log10(population), y=ECO.new)) + geom_point() + 
  stat_smooth(method = "lm", col="red") + 
  ggtitle("Population (log10) vs ECO.new: Sub-Saharan Africa")
ggplot(subsaharan, aes(x=log10(gdp), y=ECO.new)) + geom_point() + 
  stat_smooth(method = "lm", col="red") + 
  ggtitle("GDP (log10) vs ECO.new: Sub-Saharan Africa")

ggplot(latinamerica, aes(x=log10(population), y=ECO.new)) + geom_point() + 
  stat_smooth(method = "lm", col="blue") + 
  ggtitle("Population (log10) vs ECO.new: Latin America & Caribbean")
ggplot(latinamerica, aes(x=log10(gdp), y=ECO.new)) + geom_point() + 
  stat_smooth(method = "lm", col="blue") + 
  ggtitle("GDP (log10) vs ECO.new: Latin America & Caribbean")

#3.2 Fit 2 Linear models; Print summary stats and plot residuals.
lmod1 <- lm(log10(population)~subsaharan_eco.new, data=subsaharan)
summary(lmod1)
ggplot(lmod1, aes(x = .fitted, y = .resid)) + geom_point() +
  geom_hline(yintercept = 0, color="red") + 
  ggtitle("Residual Plot 1: Population vs ECO.new (Sub-Saharan Africa")

lmod2 <- lm(log10(gdp) ~ subsaharan_eco.new, data=subsaharan)
summary(lmod2)
ggplot(lmod2, aes(x = .fitted, y = .resid)) + geom_point() +
  geom_hline(yintercept = 0, color="red") + 
  ggtitle("Residual Plot 2: GDP vs ECO.new (Sub-Saharan Africa")

lmod3 <- lm(log10(population)~latinamerica_eco.new,data=latinamerica)
summary(lmod3)
ggplot(lmod3, aes(x = .fitted, y = .resid)) + geom_point() +
  geom_hline(yintercept = 0, color="blue") + 
  ggtitle("Residual Plot 3: Population vs ECO.new (Latin America & Caribbean")

lmod4 <- lm(log10(gdp) ~ latinamerica_eco.new,data=latinamerica)
summary(lmod4)
ggplot(lmod4, aes(x = .fitted, y = .resid)) + geom_point() +
  geom_hline(yintercept = 0, color="blue") + 
  ggtitle("Residual Plot 4: Population vs ECO.new (Latin America & Caribbean")

#3.3 Compare models for both regions -- describe which is better.
#Based on adjusted R^2 values, it appears the population ~ ECO.new model is a better fit, but barely. Maximum
#predictability of the response variable does not exceed 4.5%. None of the models had a p-value < 10%, which is
#already generous.

#Classification kNN
#4. Derive a new subset containing 2 regions.
multiregion <- dataset[dataset$region %in% c("Sub-Saharan Africa","Latin America & Caribbean"),]
multiregion <- multiregion[,c("population","gdp","ECO.new","region")]
multiregion <- na.omit(multiregion)


#4.1 Train a kNN model with class=region and input variables = pop, gdp, and eco.new
set.seed(6)
s.train <- sample(nrow(multiregion),0.7*nrow(multiregion))
multi.train <- multiregion[s.train,]
multi.test <- multiregion[-s.train,]
knn.predicted <- knn(multi.train[,1:3],multi.test[,1:3],multi.train[,4],k=3)

#Evaluate the model w/ a Confusion Matrix and calculate the accuracy of correct classifications.
#Accuracy = correctly classified / total data points
table(knn.predicted, multi.test[,4], dnn = c("predicted","actual"))
accuracy <- sum(knn.predicted == multi.test[,4]) / nrow(multi.test)
accuracy

k.list <- c(1,2,3,4,5,6,7)
acc.list <- c()

for(k in k.list) {
  pred <- knn(multi.train[,1:3],multi.test[,1:3],multi.train[,4],k=k)
  acc <- sum(pred == multi.test[,4]) / nrow(multi.test)
  acc.list <- c(acc.list, acc)
}
acc.list
#best k = 7

#4.2 Using the best performing k from above, train another model with a new 3rd variable instead of eco.new (EPI.new)
multiregion2 <- dataset[dataset$region %in% c("Sub-Saharan Africa","Latin America & Caribbean"), ]
multiregion2 <- multiregion2[, c("population","gdp","ECO.old","region")]
multiregion2 <- na.omit(multiregion2)

multi.train2 <- multiregion2[s.train, ]
multi.test2  <- multiregion2[-s.train, ]

knn.predicted2 <- knn(multi.train2[,1:3],multi.test2[,1:3],multi.train2[,4],k = 7)
table(knn.predicted2, multi.test2[,4],dnn = c("predicted","actual"))
accuracy2 <- sum(knn.predicted2 == multi.test2[,4]) / nrow(multi.test2)
accuracy2

#In 1-2 sentences explain which model performs better and why.
#Surprisingly, the models had the same accuracy when using the same k = 7. This is
#likely because ECO.old and ECO.new are such similar variables, so the models didn't
#produce statistically different results.



