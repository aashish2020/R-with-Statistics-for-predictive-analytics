## Regression and Classification Trees
## Last updated: October 2, 2018

library(rpart)
library(rpart.plot)
library(caret)


    ## CLASSIFICATION TREES

mower.df <- read.csv("D:/Fall-2018/BAR-Sourav/R_codes/Session6_R_Codes/RidingMowers.csv",header = T,stringsAsFactors = F)

    ### Use rpart.control to specify depth & method to specify classification
class.tree <- rpart(Ownership ~ ., data = mower.df, 
                    control = rpart.control(maxdepth = 2), method = "class")
    ### plot tree
prp(class.tree, type = 1, extra = 1, split.font = 2, varlen = -10)  


  ### Example - Universal Bank (Default Tree produced by rpart())

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns

  ### Create Training and Validation sets
set.seed(1)  
 train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

  ### Generate classification tree
default.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class")
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)



  ### Fully-Grown Tree
deeper.ct <- rpart(Personal.Loan ~ ., data = train.df, 
                   method = "class", cp = 0, minsplit = 1)

length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])  # count number of leaves
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))  




  ### Confusion Matrices 
#library(caret)
    ### for Training set
default.ct.point.pred.train <- predict(default.ct, 
                                       data = train.df, 
                                       type = "class")
confusionMatrix(default.ct.point.pred.train, as.factor(train.df$Personal.Loan))

    ### for Validation set
default.ct.point.pred.valid <- predict(default.ct, 
                                       newdata = valid.df, 
                                       type = "class")
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$Personal.Loan))


  ### Complexity Parameters
    ### xval:  # of folds to use cross-validation procedure
    ### CP: sets the smallest value for the complexity paraeter
cv.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)
printcp(cv.ct)

  ### Pruned Classificatin Tree using CP with lowest error
pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)  

  ### Best-Pruned Tree
set.seed(1)
cv.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp = 0.00001, minsplit = 1, xval = 5)  # minsplit is the minimum number of observations in a node for a split to be attempted. xval is number K of folds in a K-fold cross-validation.
printcp(cv.ct)  # Print out the cp table of cross-validation errors. The R-squared for a regression tree is 1 minus rel error. xerror (or relative cross-validation error where "x" stands for "cross") is a scaled version of overall average of the 5 out-of-sample errors across the 5 folds.
pruned.ct <- prune(cv.ct, cp = 0.0154639)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 



 