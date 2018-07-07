#####6. RANDOM FOREST#####

require(randomForest)
require(MASS)#Package which contains the Boston housing dataset

dim(Boston)
?Boston
bostonHousing<- Boston
head(bostonHousing)

#check for na's
library(Amelia)
missmap(bostonHousing,col=c('yellow','lightblue'),y.at=1,y.labels='',legend=TRUE)

#explore data
library(ggplot2)
library(GGally)
ggpairs(bostonHousing)

library(dplyr)
library(reshape2)
bostonHousing %>%
  select(c(crim, rm, age, rad, tax, lstat, medv,indus,nox,ptratio,zn)) %>%
  melt(id.vars = "medv") %>%
  ggplot(aes(x = value, y = medv, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Median House Price ($1000s)") +
  theme_minimal()

#Splitting data 
# Load the package:
library(caTools)
# Set our seed so we all get the same split
set.seed(13)
# Randomly split the data:
spl = sample.split(bostonHousing$medv, SplitRatio = 0.65)
trainData = subset(bostonHousing, spl == TRUE)
testData = subset(bostonHousing, spl == FALSE)

#-------------------------------------------------------------------------------------#
Boston.rf<-randomForest(medv ~ . , data = trainData, importance=TRUE)
Boston.rf
plot(Boston.rf)

#Variable importance
importance(Boston.rf)
VI_F=importance(Boston.rf)
VI_F

# represents the mean decrease in node impurity
#library(caret)
varImpPlot(Boston.rf)
varImpPlot(Boston.rf,type = 2)

####Prediction and accuracy measurement####

#Error percentage on train , ~95% accuracy
predictionsTrain<- predict(Boston.rf,trainData)
errorPTrain <- abs(trainData$medv - predictionsTrain)*100/trainData$medv
100-mean(errorPTrain)

#Error percentage on test, ~88% accuracy
predictionsTest<- predict(Boston.rf,testData)
errorPTest <- abs(testData$medv - predictionsTest)*100/testData$medv
100-mean(errorPTest)
#####################################################################################

## tune `random forest' for regression, using 10-fold cross validation (default)
library(caret)
library(e1071)
fitRFTune <- tune.randomForest(medv~., data = testData,ntree = c(3,5,7,11,15,25,40))
summary(fitRFTune)
plot(fitRFTune)

fitRFTune2 <- tune.randomForest(medv~., data = testData, mtry=c(3,5,7,11))
summary(fitRFTune2)
plot(fitRFTune2)

#Tune multiple parameters together
fitRFTune3 <- tune.randomForest(medv~., data = testData, mtry=c(3,5,7,11),ntree = c(25,40,50,100,200,400,1500))
summary(fitRFTune3)
plot(fitRFTune3)
#####################################################################################

#Grid Search
#Another search is to define a grid of algorithm parameters to try.
#Each axis of the grid is an algorithm parameter, and points in the grid are specific combinations of 
#parameters. Because we are only tuning one parameter, the grid search is a linear search through a vector 
#of candidate values.
metric <- "Accuracy" #for classification models
metric <- "RMSE" # for regression models
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(13)

tunegrid <- expand.grid(.mtry=c(1:13))
rf_gridsearch <- train(medv~., data=trainData, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
############################################################################################

#The above Random Forest model chose Randomly 4 variables to be considered at each split. We could now try all possible 13 predictors which can be found at each split.
oob.err<-double(13)
test.err<-double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:13) 
{
  rf=randomForest(medv ~ . , data = trainData,mtry=mtry,ntree=40) 
  oob.err[mtry] = rf$mse[40] #Error of all Trees fitted
  
  pred<-predict(rf,testData) #Predictions on Test Set for each Tree
  test.err[mtry]= with(testData, mean( (medv - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ")
  
}

test.err 
oob.err

#Comparing both Test Error and Out of Sample Estimation for Random Forests
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

#---------------------------------------------------------------------------------------#
rf_model <- randomForest(medv ~ ., data=trainData,
                         mtry=11,
                         ntree=40,     # number of trees in the Random Forest
                         nodesize=25, # minimum node size set small enough to allow for complex trees,
                         # but not so small as to require too large B to eliminate high variance
                         keep.inbag=TRUE, importance=TRUE)
