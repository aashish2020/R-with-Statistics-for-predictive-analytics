## Logistic Regression Example - Diabetes Data
## Last updated: October 29, 2018


library(tidyverse)
library(caret)
library(corrplot)
library(leaps)
library(MASS)
library(nnet)

theme_set(theme_classic())


  # Read the data
diab.df <- read.csv("diabetic_data.csv")

str(diab.df)
names(diab.df)



  # Data Exploration

  # Readmission Count
ggplot(diab.df, aes(readmitted)) +
  geom_bar(fill = "brown2", width = .3)


  # Readmission (proportion)
ggplot(diab.df, aes(readmitted)) +
  geom_bar(aes(y=..prop.., group = 1), 
           fill = "gold3", width = .3)


  # Checking Cross-Tabs
options(digits = 3)
prop.table(table(diab.df$readmitted, diab.df$insulin))

options(scipen = 999)
prop.table(table(diab.df$readmitted, diab.df$gender))





  # Partition the data
set.seed(123)
train.index <- createDataPartition(diab.df$readmitted, p = 0.8, list = FALSE)
train.diab.df <- diab.df[train.index, ]
valid.diab.df <- diab.df[-train.index, ]
  

  # Multionomial Logistic Regression 

train.diab.df$readmit2 <- relevel(train.diab.df$readmitted, ref = "NO")





reg1 <- multinom(readmit2 ~ insulin + A1Cresult, data = train.diab.df)
summary(reg1)

  # 2-sided z test
zstat <- summary(reg1)$coefficients/summary(reg1)$standard.errors
pval <- (1-pnorm(abs(zstat), 0, 1)) *2
pval

  ## Change in Relative Risk due to change in 1 unit of a predictor
exp(coef(reg1))

  ## predicted probabilities
head(pred1 <-  fitted(reg1))
