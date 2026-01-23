##Lab 1 -- Data Analytics -- Isabel Bateman

#Libraries/Clear Existing Data
library(readr)
library(EnvStats)
library(nortest)
rm(list=ls())

#Set Working Directory
setwd("~/Spring 2026/Data Analytics/Labs/Lab 1")
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

#2 Variables I'll be Using: ECO.new & ECO.old
eco.new <- epi.data$ECO.new
eco.old <- epi.data$ECO.old

#Variable Summaries
summary(eco.new)
summary(eco.old)

#Clean Data / Remove NAs
newNAs <- is.na(eco.new)
oldNAs <- is.na(eco.old)

new <- eco.new[!newNAs]
old <- eco.old[!oldNAs]

#Boxplot
boxplot(new,old, names=c("ECO (new)","ECO (old)"))

#Histogram with Overlayed Theoretical Probability Distribution
#ECO new; First need stats, then range to plot, then hist plot
mean(new)
sd(new)

x1_new <- seq(20,90,1)
d1_new <- dnorm(x1_new,51.1,13.16,log=FALSE)

hist(new,x1_new,prob=TRUE)
lines(x1_new,d1_new)

#ECO old (same as above)
mean(old)
sd(old)
hist(old)
x1_old <- seq(20,90,1)
d1_old <- dnorm(x1_old, 49.5,12.7,log=FALSE)
hist(old,x1_old,prob=TRUE)
lines(x1_old, d1_old)


#ECDF Plots
plot(ecdf(new), do.points=FALSE,verticals=TRUE)
plot(ecdf(old),do.points=FALSE,verticals=TRUE)

#QQ Plots of each variable against normal distribution
qqnorm(new); qqline(new)
qqnorm(old);qqline(old)

#QQ Plot of variables against each other
qqplot(new,old,xlab="QQ Plot for ECO.new and ECO.old")

#Normality Test of Each Variable
shapiro.test(new)
shapiro.test(old)

#Identical Distribution Test (Kolmogorov-Smirnov)
ks.test(new,old)
