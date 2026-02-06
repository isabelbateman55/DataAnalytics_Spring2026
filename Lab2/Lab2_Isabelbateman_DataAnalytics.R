rm(list=ls())

library("ggplot2")
library("readr")

#Read Dataset
NY_House_Dataset <- read_csv("~/Spring 2026/Data Analytics/Labs/Lab 2/NY-House-Dataset.csv")
dataset <- NY_House_Dataset
# names(dataset)
# View(dataset)


#Filter/Clean Data
dataset <- dataset[dataset$PRICE<195000000,]
dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]
dataset <- dataset[dataset$BATH!=0,]


#Plotting for Diagnostics
ggplot(dataset, aes(x = BEDS, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = log10(BEDS), y = log10(PRICE))) +
  geom_point()

ggplot(dataset, aes(x = BATH, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = log10(BATH), y = log10(PRICE))) +
  geom_point()

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()


#Fitting Linear Models & Printing Summary Statistics
lmod1 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data=dataset)
summary(lmod1)

lmod2 <- lm(log10(PRICE)~log10(PROPERTYSQFT) + log10(BEDS), data=dataset)
summary(lmod2)

lmod3 <- lm(log10(PRICE)~log10(PROPERTYSQFT) + log10(BEDS) + log10(BATH), data=dataset)
summary(lmod3)

#Scatterplot of Significant Variables Against Price
#Model 1: Property Sqft
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

#Model 2: Beds
ggplot(dataset, aes(x = log10(BEDS), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="blue")

#Model 3: Baths
ggplot(dataset, aes(x = log10(BATH), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="green")


#Plot of Residuals
#Model 1
ggplot(lmod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color="red") + ggtitle("Residual Plot 1: PropertySqft vs Price")

#Model 2
ggplot(lmod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0,color="blue") + ggtitle("Residual Plot 2: PropertySqft + Beds vs Price")

#Model 3
ggplot(lmod3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0,color="green") + ggtitle("Residual Plot 3: PropertySqft + Beds + Bath vs Price")

