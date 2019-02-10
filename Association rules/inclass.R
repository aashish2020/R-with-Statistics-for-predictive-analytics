library(arules)
library(recommnderlab)

setwd("D:/Fall-2018/BAR-Sourav/R_codes/Session12_R_Codes/")
a <- read.csv("Faceplate.csv",header = T)

#drop first column and convert in to matrix
a <- as.matrix(a[-1])

##Convert the binary incidence matrix into a transaction database
a.tras <- as(a,"transactions")
inspect(a.tras)

##load dataset##
b <- read.csv("CharlesBookClub.csv",header = T)

##create a binary inci matrix##
count.books.df <- b[,8:18]
incid.books.df <- ifelse(count.books.df>0,1,0)
incid.books.mat <- as.matrix(incid.books.df[,-1])

###Convert Binary incidence matrix -> Transcations database
books.trans <- as(incid.books.mat,"transactions")
inspect(head(books.trans,10))

###create rules
rules <- apriori(books.trans,parameter=list(supp=0.05,conf=0.7,target="rules"))

inspect(head(sort(rules,by="lift")))
