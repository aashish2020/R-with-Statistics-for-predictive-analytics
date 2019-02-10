## Performance Evaluations
## Last Updated: September 17, 2018


library(fpp2)
library(caret)
library(gains)
library(e1071)
library(pROC)



  ## Prediction Example - ToyoataCorolla Data
setwd("D:/Fall-2018/BAR-Sourav/R_codes/Session4_R_Codes/")
toyota.corolla.df <- read.csv("ToyotaCorolla.csv")


  ### randomly generate training and validation sets
set.seed(5)
training <- sample(toyota.corolla.df$Id, 600)
validation <- sample(setdiff(toyota.corolla.df$Id, training), 400)  ## sampling from obs in toyota.corolla.df but not in training

  ### Linear regression model
reg <- lm(Price~., data=toyota.corolla.df[,-c(1,2,8,11)], subset=training,
          na.action=na.exclude)
pred_t <- predict(reg, na.action=na.pass)
pred_v <- predict(reg, newdata=toyota.corolla.df[validation,-c(1,2,8,11)],
                  na.action=na.pass)
#   ### Evaluate performance

      ### Training
options(scipen = 999)
accuracy(pred_t, toyota.corolla.df[training,]$Price)
      ### validation
accuracy(pred_v, toyota.corolla.df[validation,]$Price)




  ### Remove missing Price data
toyota.corolla.df <-
  toyota.corolla.df[!is.na(toyota.corolla.df[validation,]$Price),]

  ### Generate random Training and Validation sets
training <- sample(toyota.corolla.df$Id, 600)
validation <- sample(setdiff(toyota.corolla.df$Id, training), 400)

  ### regression model based on all numerical predictors
reg <- lm(Price~., data = toyota.corolla.df[,-c(1,2,8,11)], subset = training)

  ### Predictions
pred_v <- predict(reg, newdata = toyota.corolla.df[validation,-c(1,2,8,11)])


  ## Lift Charts and Decile Charts

  ### Generate Lift Chats
gain <- gains(toyota.corolla.df[validation,]$Price[!is.na(pred_v)], pred_v[!is.na(pred_v)])


  ### cumulative lift chart
options(scipen=999)
    ### Compute gains relative to price
price <- toyota.corolla.df[validation,]$Price[!is.na(toyota.corolla.df[validation,]$Price)]
plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative Price", main="Lift Chart", 
     col = "blue1", type="l")
  ### baseline
lines(c(0,sum(price))~c(0,dim(toyota.corolla.df[validation,])[1]), col="brown2", lty=2)


  ### Decile-wise lift chart
barplot(gain$mean.resp/mean(price), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart",
        col = "coral1")





  ## Classification Example - Confusion Matrix and Misclassification Rate

owner.df <- read.csv("ownerExample.csv")
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.5, 'owner', 'nonowner')), 
                owner.df$Class)
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.25, 'owner', 'nonowner')), 
                owner.df$Class)
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.75, 'owner', 'nonowner')), 
                owner.df$Class)



  ## Accuracy and Misclassifcation Rate as a funcion on Cut-off rate

df <- read.csv("liftExample.csv")

  ### ROC Curve
library(pROC)
r <- roc(df$actual, df$prob)
plot.roc(r)
  ### compute auc
auc(r)


  ## Lift Charts

#library(gains)
gain <- gains(df$actual, df$prob, groups=dim(df)[1])
plot(c(0, gain$cume.pct.of.total*sum(df$actual)) ~ c(0, gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", type="l",
     col="blue1")
lines(c(0,sum(df$actual))~c(0,dim(df)[1]), col="red1", lty=2)


  ### Decile Lift Charts
gain <- gains(df$actual, df$prob)
barplot(gain$mean.resp / mean(df$actual), names.arg = gain$depth, xlab = "Percentile", 
        ylab = "Mean Response", main = "Decile-wise lift chart", col = "gold1")

