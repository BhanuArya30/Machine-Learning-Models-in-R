#####4. LOGISTIC REGRESSION#####

install.packages("mlbench")
install.packages("GGally")
install.packages("pROC")
install.packages("caret")
install.packages("caTools")
install.packages("ggplot2")
install.packages("plotly")


library(mlbench)
library(plotly)

#Get the data in the environment
data("PimaIndiansDiabetes")

#Store data in a variable
piData<-PimaIndiansDiabetes

#Dataset description
?PimaIndiansDiabetes

#Checking the strcuture
str(piData)

#Checking the top 5 rows
head(piData)

#explore data
library(ggplot2)
library(GGally)
ggpairs(piData)

#Splitting data 
# Load the package:
library(caTools)
# Set our seed so we all get the same split
set.seed(13)
# Randomly split the data:
spl = sample.split(piData$diabetes, SplitRatio = 0.7)
trainData = subset(piData, spl == TRUE)
testData = subset(piData, spl == FALSE)

#Run Logistic regression predicting probability of occurence of diabetes
library(caret)
logReg<- glm(diabetes~., data=trainData, family=binomial)

#checking variable importance of each independent variable learnt from the model
varImp(logReg)

#Summarize logistic regresion
summary(logReg)

#Checking Model's Predictions (probability estimates) on train Data
predicted_prob<-predict(logReg, trainData, type="response")
predicted_prob

#appends predicted vals to the test data and stores in a separate data frame
prediction_data<-data.frame(trainData,predicted_prob)
head(prediction_data)

#converting probabilities to pos/neg tag with 0.5 threshold
prediction_class<-ifelse(predicted_prob<0.5,"neg","pos")
prediction_class

#Confusion Matrix
#table(data = prediction_class, reference = testData$diabetes)
xtab<- table(  trainData$diabetes,prediction_class)
xtab

#Calculating accuracy i.e. proportion of correctly predicted instances accuracy=77.32%
confusionMatrix(xtab, positive = "pos")

#plotting roc curve
library(pROC)
rocCurve <- roc(diabetes ~ predicted_prob, data = trainData)
plot(rocCurve)

#Area under curve, auc = 0.8438
auc(rocCurve)

#vary threshold level

#vary threshold from 0.1-0.9 , step size=0.1
cutoffs <- seq(0.1,0.9,0.1)
accuracy <- NULL

#logging accuracy for each threshold level
for (i in seq(along = cutoffs)){
  prediction <- ifelse(logReg$fitted.values >= cutoffs[i], "pos", "neg") #Predicting for cut-off
  accuracy <- c(accuracy,length(which(trainData$diabetes ==prediction))/length(prediction)*100)
}


#Plotting varying accuracy by varying threshold
plot(cutoffs, accuracy, pch =19,type='b',col= "steelblue",
     main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")


#checking confusion matrix with 0.6 threshold, accuracy = 79% (accuracy increases with change in threshold from 0.5 to 0.6)
prediction_class<-ifelse(predicted_prob>=0.6,"pos","neg")
xtabNew<- table(trainData$diabetes, prediction_class)
confusionMatrix(xtabNew,positive = "pos")

#Plot most significant variable vs diabetes
boxplot(piData$glucose~piData$diabetes)
boxplot(piData$mass~ piData$diabetes)
boxplot(piData$pregnant~ piData$diabetes)
