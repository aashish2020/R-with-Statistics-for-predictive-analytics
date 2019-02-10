## K-NN
## Last updated: September 25, 2018

library(caret)
library(FNN)
library(class)
library(e1071)
library(dplyr)

mower.df <- read.csv("D:/Fall-2018/BAR-Sourav/R_codes/Session6_R_Codes/RidingMowers.csv",stringsAsFactors = F,header = T)

  ### Partitioning data
set.seed(111)
train.index <- sample(row.names(mower.df), 0.6*dim(mower.df)[1])  
valid.index <- setdiff(row.names(mower.df), train.index)  
train.df <- mower.df[train.index, ]
valid.df <- mower.df[valid.index, ]

  ### New household with income and lot_size
new.df <- data.frame(Income = 60, Lot_Size = 20)

  ### Scatterplot of data
plot(Lot_Size ~ Income, data=train.df, 
     pch=ifelse(train.df$Ownership=="Owner", 1, 3))
text(train.df$Income, train.df$Lot_Size, rownames(train.df), pos=4)
text(60, 20, "X")
legend("topright", c("owner", "non-owner", "newhousehold"), pch = c(1, 3, 4))




  ### Run K-NN
train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df
  
  ### Normalize data using preProcess() from CARET 
norm.values <- preProcess(train.df[, 1:2], method=c("center", "scale"))
train.norm.df[, 1:2] <- predict(norm.values, train.df[, 1:2])
valid.norm.df[, 1:2] <- predict(norm.values, valid.df[, 1:2])
mower.norm.df[, 1:2] <- predict(norm.values, mower.df[, 1:2])
new.norm.df <- predict(norm.values, new.df)

####Plotting after nomrlaisation to have a visual look of normalisation
plot(Lot_Size ~ Income, data=train.norm.df, 
     pch=ifelse(train.norm.df$Ownership=="Owner", 1, 3))
text(train.norm.df$Income, train.norm.df$Lot_Size, rownames(train.norm.df), pos=4)
text(-0.39, 0.58, "X")
legend("topright", c("owner", "non-owner", "newhousehold"), pch = c(1, 3, 4))


#library(FNN)
nn <- knn(train = train.norm.df[, 1:2], test = new.norm.df, 
          cl = train.norm.df[, 3], k = 3)
  ### Nearest-neighbor Index (ratio of observed distance divided by the expected distance)
row.names(train.df)[attr(nn, "nn.index")]



  ### Chooose optimal K

  ### Initialize a data frame with two columns: k and accuracy
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

  ### compute knn for different k on validation
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, 1:2], valid.norm.df[, 1:2], 
                  cl = train.norm.df[, 3], k = i)
  accuracy.df[i, 2] <- confusionMatrix(table(knn.pred, valid.norm.df[, 3]))$overall[1] 
}
accuracy.df


  ### Re-run KNN with all observation 
    ### using k = 4 (assuming it is "best")

knn.pred.new <- knn(mower.norm.df[, 1:2], new.norm.df, 
                    cl = mower.norm.df[, 3], k = 4)
row.names(train.df)[attr(knn.pred.new, "nn.index")]
