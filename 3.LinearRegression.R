#####3. LINEAR REGRESSION#####

#Using R dataset - mtcars
mtData <- mtcars

####Exploring Dataset####

#Checking the strcuture
str(mtData)

#Checking the top 5 rows
head(mtData,5)

####Building Model####

#Run Linear regression predicting mpg, miles per gallon using only horsepower variable
linRegHp<- lm(mpg~hp, data=mtData)
plot(mtData$hp,mtData$mpg)
abline(linRegHp)

#Decoding linRegHp
summary(linRegHp)

#R-squared = Explained variation / Total variation
#R-squared is always between 0 and 100%:
#0% indicates that the model explains none of the variability of the response data around its mean.
#100% indicates that the model explains all the variability of the response data around its mean.

###Polynomial regression###

linRegHp2<- lm(mpg~poly(hp,2, raw = TRUE), data=mtData)
summary(linRegHp2)
plot(mtData$hp,mtData$mpg)
plot(x =mtData$hp, y = linRegHp2$fitted.values)
plot(x=mtData$mpg, y=linRegHp2$fitted.values)

#Run Linear regression predicting mpg, miles per gallon using all independent varaibles
linReg<- lm(mpg~., data=mtData)

#Decoding linReg
summary(linReg)

#Predicting mpg values on original dataset 
predictions<- predict(linReg,mtData)
predictions

#####Corelation####

#Compute corelation matrix
install.packages("corrplot")
library(corrplot)
corMat <- cor(mtData)
round(corMat,2)
corrplot(corMat, method="circle")

#Converting am to categorical variable
mtData$am <- as.factor(mtData$am)
str(mtData)

#Remove cyl as feature because highly corelated with disp, and has very large p-value 
linReg<- lm(mpg~disp+hp+drat+wt+qsec+vs+am+gear+carb, data=mtData)
linReg<- lm(mpg~.-cyl, data=mtData)

summary(linReg)
plot(mtData$mpg,linReg$fitted.values)

###Analyse resiudal plots, QQ plots etc of Linear Regression###

#Get all 4 plots in one screen
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(linReg)
par(mfrow=c(1,1)) # Change back to 1 x 1

####Splitting Dataset####

#Splitting data 
# Load the package:
library(caTools)
# Set our seed so we all get the same split
set.seed(13)

# Randomly split the data: specify the dependent/target variable and the split ratio
spl = sample.split(mtData$mpg, SplitRatio = 0.65)
trainData = subset(mtData, spl == TRUE)
testData = subset(mtData, spl == FALSE)

#Build model on train data set
linReg<- lm(mpg~., data=trainData)
summary(linReg)

####Prediction and accuracy measurement####

#predicting on train
predictionsTrain<- predict(linReg,trainData)

#Error percentage on train
errorPTrain <- abs(trainData$mpg - predictionsTrain)*100/trainData$mpg
100-mean(errorPTrain)

#predicting on test
predictionsTest<- predict(linReg,testData)

#Error percentage on test
errorPTest <- abs(testData$mpg - predictionsTest)*100/testData$mpg
100-mean(errorPTest)
