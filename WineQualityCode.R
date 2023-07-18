#Wine Quality Project

library(tidyr)
library(stringr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(neuralnet)
library(nnet)

# load the csv file
wine<-read.csv("winequality.csv") 
summary(wine)
names(wine)

#Preprocessing
#check NA values
sum(is.na(wine))
wine

#Scale data
winerange <- preProcess(wine, method = "range")

# apply the normalization to the data set
wine.df <- predict(winerange, wine)
wine.df

#Model 1
#Mulitple linear regression model
Regwine<-lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+
              total.sulfur.dioxide+density+pH+sulphates+alcohol, data=wine.df)
summary(Regwine)

#backward method
modelbest<-step(Regwine, direction = "backward")
summary(modelbest) #best aic model

#Model 2
#Regression tree
#Split data into training(60%) and validation(40%)
set.seed(1)
train.index<-sample(c(1:dim(wine)[1]), dim(wine)[1]*0.6) 
train.index
train.df<-wine[train.index, ]# training
valid.df<-wine[-train.index,] # validation

winetree <- rpart(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+
                    total.sulfur.dioxide+density+pH+sulphates+alcohol, data=train.df)

# plot tree 
prp(winetree, type=1, extra =1, under= TRUE, split.font = 1)

# plot out rules 
print(winetree)

#prune the tree
#check for cp level
winetree$cptable 

#prune the tree with .01 cp level as it had min xerror
wine_prune <- prune(winetree, cp = 0.01) 
wine_prune

# plot pruned tree
prp(wine_prune, type=1, extra =1, under= TRUE, split.font = 1)

# print out rules for pruned tree
print(wine_prune)

#3rd Model
#Split data into training(60%) and validation(40%)
set.seed(1)
train2.index<-sample(c(1:dim(wine.df)[1]), dim(wine.df)[1]*0.6) 
train2.index
train2.df<-wine.df[train2.index, ]# training
valid2.df<-wine.df[-train2.index,] # validation

#Neural network model with one layer, 2 nodes
wine_nn<-neuralnet(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+ 
                     total.sulfur.dioxide+density+pH+sulphates+alcohol,data=train2.df, hidden=c(2)) 

#summary of NN model 
summary(wine_nn)

# Plot NN
plot(wine_nn, rep="best")