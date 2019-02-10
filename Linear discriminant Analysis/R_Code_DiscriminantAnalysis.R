## Linear Discriminant Analysis (using two examples)
## Last updated: October 17, 2018
  

library(MASS)
library(caret)
library(ggplot2)
theme_set(theme_classic()) # set ggplot theme


setwd("D:/Fall-2018/BAR-Sourav/R_codes/Session9_R_Codes/")
    ### Example 1 - Riding Mower data
mowers.df <- read.csv("RidingMowers.csv")

  
  ### Linear Discriminant Analysis - use lda() from MASS package
lda1 <- lda(Ownership~., data = mowers.df)

  # output
lda1

  # predict
pred1 <- predict(lda1, mowers.df)
names(pred1)  ## "class", "posterior", "x"

## class: predicted class
## posterior: posterior probabilities of belonging to different classes
## x: linear disriminant values


  # check model accuracy
table(pred1$class, mowers.df$Ownership)  # pred v actual
mean(pred1$class == mowers.df$Ownership)  # percent accurate

sum(pred1$posterior[, 1] >=.5)
  
sum(pred1$posterior[, 1] >=.75)  # increase the cut-off from .5 to .75


  
  ## Example 2- IRIS data
  
  # Load the data set
data("iris")

  # Split the data into training (60%) and validation/test set (40%)
set.seed(123)
training.index <- createDataPartition(iris$Species, p = 0.6, list = FALSE)
iris.train <- iris[training.index, ]
iris.valid <- iris[-training.index, ]

  # normalize the data
    # Estimate preprocessing parameters
norm.values  <- preProcess(iris.train, method = c("center", "scale"))
    # Transform the data using the estimated parameters
iris.train.norm <- predict(norm.values, iris.train)
iris.valid.norm <- predict(norm.values, iris.valid)
  
  # run lda()
lda2 <- lda(Species~., data = iris.train.norm)
  # output
lda2


  # predict - using training data and plot
pred2.train <- predict(lda2, iris.train.norm)

    # generate lda plot
lda2.plot <- cbind(iris.train.norm, predict(lda2)$x)
ggplot(lda2.plot, aes(LD1, LD2)) +
  geom_point(aes(color = Species))


  # predict - using validation data
pred2 <- predict(lda2, iris.valid.norm)
names(pred2)

# check model accuracy
table(pred2$class, iris.valid.norm$Species)  # pred v actual
mean(pred2$class == iris.valid.norm$Species)  # percent accurate

sum(pred2$posterior[, 1] >=.5)

