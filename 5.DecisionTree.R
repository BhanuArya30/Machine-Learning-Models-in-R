#####5. DECISION TREE#####

install.packages("MASS")
install.packages("tree")
install.packages("partykit")
install.packages("Amelia")

library(MASS)
library(tree)
library(caTools)
library(rpart)
library(mlbench)
library(partykit)


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

lr<- lm(medv~poly(lstat,2, raw = TRUE), data=bostonHousing)
summary(lr)
plot(x = bostonHousing$lstat, y = lr$fitted.values)
plot(lr)

#Splitting data 
# Load the package:
library(caTools)
# Set our seed so we all get the same split
set.seed(13)
# Randomly split the data:
spl = sample.split(bostonHousing$medv, SplitRatio = 0.65)
trainData = subset(bostonHousing, spl == TRUE)
testData = subset(bostonHousing, spl == FALSE)

#Creating the tree 

# grow tree 
fit <- rpart(medv~.,method="anova", data=trainData)
#fit <- lm(medv~.,data=trainData)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits
print(fit)

#Plotting the tree
library(rpart.plot)
prp(type=2,extra=1,digits=4,box.palette="auto",fallen.leaves = TRUE,fit)
#fancyRpartPlot(fit)
plot(as.party(fit))
library(rattle)
fancyRpartPlot(fit,main=paste('RPART:'))

## tune `rpart' for regression, using 10-fold cross validation (default)
library(e1071)
fitRpartTune <- tune.rpart(medv~., data = trainData, minsplit = c(5,10,15,20,25,30,35))
summary(fitRpartTune)
plot(fitRpartTune)

####Prediction and accuracy measurement####

#predicting on train
predictionsTrain<- predict(fit,trainData)

#Error percentage on train
errorPTrain <- abs(trainData$medv - predictionsTrain)*100/trainData$medv
100-mean(errorPTrain)

#predicting on test
predictionsTest<- predict(fit,testData)

#Error percentage on train
errorPTest <- abs(testData$medv - predictionsTest)*100/testData$medv
100-mean(errorPTest)

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Boston Housing ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
