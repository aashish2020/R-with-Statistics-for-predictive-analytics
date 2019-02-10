##  Logistic Regressions
## Last updated: October 23, 2018

library(tidyverse)
library(gains)
library(leaps)
library(caret)


  ### Read in Universal Bank Data
setwd("D:/Fall-2018/BAR-Sourav/R_codes/Session10_R_Codes/")
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1,5)]  # Drop ID and zip code columns


  ### Convert "Education" to categorical variable
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))

  ### Partition data
set.seed(13)
train.index <- createDataPartition(bank.df$Personal.Loan, p = 0.6, list = FALSE)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

  # Logistic regression
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
logit.reg1 <- glm(Personal.Loan ~ Income, data = train.df) 
summary(logit.reg)

options(scipen=999)
summary(logit.reg)


  ### Evaluate Performance of the Logit Model
    ### Predict propensities
logit.reg.pred <- predict(logit.reg, valid.df[, -8], type = "response")

gain <- gains(valid.df$Personal.Loan, logit.reg.pred, groups = 10)

  ### Plot Lift Chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$Personal.Loan))~c(0,gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", main = "", type = "l")
lines(c(0,sum(valid.df$Personal.Loan))~c(0, dim(valid.df)[1]), lty = 5)

  ### Plot decile-wise chart
heights <- gain$mean.resp/mean(valid.df$Personal.Loan)
midpoints <- barplot(heights, names.arg = gain$depth,  ylim = c(0,9), col = "blue",  
                     xlab = "Percentile", ylab = "Mean Response", 
                     main = "Decile-wise lift chart")
    ### add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)


