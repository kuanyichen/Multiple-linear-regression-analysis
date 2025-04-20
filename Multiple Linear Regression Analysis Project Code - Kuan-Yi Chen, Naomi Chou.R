# Read in file

library(readr)
happiness <- read_csv("2019.csv")
head(happiness)
attach(happiness)

# Scatterplot matrix

pairs(Score~`GDP per capita` + `Social support` + `Healthy life expectancy` + `Freedom to make life choices` + Generosity + `Perceptions of corruption`)

# Full model

model_full <- lm(Score ~ `GDP per capita` + `Social support` + `Healthy life expectancy` + `Freedom to make life choices` + Generosity + `Perceptions of corruption`)
summary(model_full)

# Added-variable plot of full model

avPlots(model_full)

# Reduced model

model_reduced <- lm(Score ~ `GDP per capita` + `Social support` + `Healthy life expectancy` + `Freedom to make life choices`)

# Partial F test

anova(model_reduced, model_full)

# Reduced model diagnostics

par(mfrow = c(2,2))
plot(model_reduced)

# Inverse response plot

install.packages("car")
library(car)
par(mfrow = c(1,1))
inverseResponsePlot(model_reduced, key = TRUE)

# Inverse response plot transformed model

IRP_transform <- lm(Score^0.42~`GDP per capita` + `Social support` + `Healthy life expectancy` + `Freedom to make life choices`)
summary(IRP_transform)
par(mfrow = c(2,2))
plot(IRP_transform)

# Box-cox method summary

adjusted_data <- cbind(Score, `GDP per capita`, `Social support`, `Healthy life expectancy`, `Freedom to make life choices`) + 0.0001
boxcox <- powerTransform(adjusted_data ~ 1)
summary(boxcox)

# Box-cox method transformed model

library(MASS)
happiness$t_score <- happiness$Score
happiness$t_GDP <- happiness$`GDP per capita`
happiness$t_social_support <- happiness$`Social support`^2
happiness$t_life_expect <- (happiness$`Healthy life expectancy`^1.52 - 1) / 1.52
happiness$t_freedom <- happiness$`Freedom to make life choices`  

box_cox_transform <- lm(t_score ~ t_GDP + t_social_support + t_life_expect + t_freedom)
summary(box_cox_transform)  
par(mfrow = c(2,2))
plot(box_cox_transform)

# VIF

vif(box_cox_transform)

# Variable selection

# All possible subsets

library(leaps)
X <- cbind(t_GDP, t_social_support, t_life_expect, t_freedom)
b <- regsubsets(as.matrix(X), Score)
summary(b)

p_vec <- 1:4
n <- 156

# Adjusted r^2

plot(x = p_vec, y = summary(b)$adjr2, xlab = 'subset size', ylab = 'adjusted r2')

# BIC

plot(x = p_vec, y = summary(b)$bic, xlab = 'subset size', ylab = 'BIC')

allsubsets_aic <- n*(summary(b)$rss/n) + 2*p_vec
allsubsets_aicc <- allsubsets_aic + 2 * (p_vec + 2) * (p_vec + 3) / (n - p_vec - 1)

# AIC

plot(x = p_vec, y = allsubsets_aic, xlab = 'subset size', ylab = 'AIC')

# AICc

plot(x = p_vec, y = allsubsets_aicc, xlab = 'subset size', ylab = 'AICc')

# Backward stepwise

step(box_cox_transform, direction = 'backward')
step(box_cox_transform, direction = 'backward', k = log(n))

# Forward selection

m_intercept <- lm(Score ~ 1, data = happiness)

# Using AIC

step(m_intercept, scope = list(
  lower = ~1, 
  upper = ~ t_GDP + t_social_support + t_life_expect + t_freedom), 
  direction = 'forward')

# Using BIC

step(m_intercept, scope = list(
  lower = ~1, 
  upper = ~ t_GDP + t_social_support + t_life_expect + t_freedom), 
  direction = 'forward', k = log(n))


