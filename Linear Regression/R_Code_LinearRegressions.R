# Multiple Regression
## Last updated: September 18, 2018


library(tidyverse)
library(forecast)
library(leaps)


  ## Regression using ToyotaCorolla data
car.df <- read.csv("D:/Fall-2018/BAR-Sourav/R_codes/Session5_R_Codes/ToyotaCorolla.csv")

  ### Use first 1000 rows of data
car.df <- car.df[1:1000, ]

  ### Select variables for regression
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)

  ### Partitioning Data
set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)  
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

  ### Run regression
car.lm <- lm(Price ~ ., data = train.df)

options(scipen = 999)
summary(car.lm)



  ### Generate predictions
car.lm.pred <- predict(car.lm, valid.df)

some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
summary(car.lm.pred)
plot(some.residuals, type = "p", pch = 16,
     col = "blue1",
     ylab = "Sample Residuals", 
     ylim = c(-3500, 3500), bty = "n"
     )



data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)

  ### Accuracy measures
accuracy(car.lm.pred, valid.df$Price)





  ### Exhaustive Search
search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)###Only choosing the model here,prediction is not happening involving this model##
search2 <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "forward")
sum2 <- summary(search2)
search3 <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                      method = "backward")
sum3 <- summary(search3)


# show models
sum$which

# show metrics
sum3$rsq
sum3$adjr2
sum$cp



  ### Backward Selection

car.lm.step1 <- step(car.lm, direction = "backward")##Using step is another way to choose vairables,here model goes in as input for step and the result is a model using which we predict the price on the test data set##
summary(car.lm.step1)  # Which variables were dropped? Ans-Metallic color,automatic,doors,cc
car.lm.step1.pred <- predict(car.lm.step1, valid.df)
accuracy(car.lm.step1.pred, valid.df$Price)

  ### Foward Selection
    # create model with no predictors
car.lm.null <- lm(Price~1, data = train.df)
    # use step() to run forward regression.
car.lm.step2 <- step(car.lm.null, scope=list(lower=car.lm.null, upper=car.lm), direction = "forward")
summary(car.lm.step2)  # Which variables were added?
car.lm.step2.pred <- predict(car.lm.step2, valid.df)
accuracy(car.lm.step2.pred, valid.df$Price)

  ### Stepwise Regression
car.lm.step3 <- step(car.lm, direction = "both")
summary(car.lm.step3)  # Which variables were dropped/added?
car.lm.step3.pred <- predict(car.lm.step3, valid.df)
accuracy(car.lm.step3.pred, valid.df$Price)

