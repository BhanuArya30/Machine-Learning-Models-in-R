#####2. INTRO TO STATS#####

#Reading : https://towardsdatascience.com/data-science-simplified-hypothesis-testing-56e180ef2f71


install.packages("plotrix")
install.packages("plyr")
install.packages("gridExtra")
install.packages("ggplot2")

#Descriptive statistics
str(mtcars)
dim(mtcars)

#Type of variables

#Categorical
summary(mtcars$gear)
#as.factor(mtcars$gear)

#check distribution with bar plot
counts<- table(mtcars$gear)
counts
barplot(counts, main="Car Distribution", 
        xlab="Gears")

#pie chart
pie(counts, main="Pie Chart of Gears")

#3D pie chart
library(plotrix)
label<-  paste(names(counts), "\n",counts, sep="")
pie3D(counts, labels = label,explode=0.1,
      main="Pie Chart of Gears ")


#Mode of categorical variables, get as Dataframe not as table
library(plyr)
countSpecies = count(mtcars, "gear")
countSpecies

#Numeric
str(mtcars)
summary(mtcars$mpg)
median(mtcars$mpg)
mean(mtcars$mpg)
min(mtcars$mpg)
max(mtcars$mpg)
sd(mtcars$mpg)

#Histograms
hist(mtcars$mpg, main="Histogram of Miles per Gallon", col = "darkblue")

#Boxplots
boxplot(mtcars$mpg)
boxplot(mtcars)

#Customise box plot
keep.par <- par()  #par(keep.par)
par(mar=c(10,4,4,2)+0.1)
boxplot(mtcars,las=3)

#Plot 
plot(mtcars$disp, mtcars$mpg, col=(mtcars$gear+1))
plot(mtcars$disp, mtcars$mpg, cex = mtcars$disp/100, col=mtcars$gear)

#Anscombe's quartet

#Anscombe's quartet is a set of four small datasets, constructed to show the importance of 
#visualising data and the dangers of reliance on simple summary statistics.

library(gridExtra)
library(ggplot2)
data(anscombe)
anscombe
summary(anscombe)

# correlation
sapply(1:4, function(x) cor(anscombe[, x], anscombe[, x+4]))

# variance
sapply(5:8, function(x) var(anscombe[, x]))

# linear regression (first pair only)
lm(y1 ~ x1, data = anscombe)

#Plotting regression line of lm on the 4 datasets and check the goodness of fit
p1 <- ggplot(anscombe) + geom_point(aes(x1, y1), color = "darkorange", size = 3) + theme_bw() + scale_x_continuous(breaks = seq(0, 20, 2)) + scale_y_continuous(breaks = seq(0, 12, 2)) + geom_abline(intercept = 3, slope = 0.5, color = "cornflowerblue") + expand_limits(x = 0, y = 0) + labs(title = "dataset 1")
p2 <- ggplot(anscombe) + geom_point(aes(x2, y2), color = "darkorange", size = 3) + theme_bw() + scale_x_continuous(breaks = seq(0, 20, 2)) + scale_y_continuous(breaks = seq(0, 12, 2)) + geom_abline(intercept = 3, slope = 0.5, color = "cornflowerblue") + expand_limits(x = 0, y = 0) + labs(title = "dataset 2")
p3 <- ggplot(anscombe) + geom_point(aes(x3, y3), color = "darkorange", size = 3) + theme_bw() + scale_x_continuous(breaks = seq(0, 20, 2)) + scale_y_continuous(breaks = seq(0, 12, 2)) + geom_abline(intercept = 3, slope = 0.5, color = "cornflowerblue") + expand_limits(x = 0, y = 0) + labs(title = "dataset 3")
p4 <- ggplot(anscombe) + geom_point(aes(x4, y4), color = "darkorange", size = 3) + theme_bw() + scale_x_continuous(breaks = seq(0, 20, 2)) + scale_y_continuous(breaks = seq(0, 12, 2)) + geom_abline(intercept = 3, slope = 0.5, color = "cornflowerblue") + expand_limits(x = 0, y = 0) + labs(title = "dataset 4")

grid.arrange(p1, p2, p3, p4, top = "Anscombe's Quartet")

#T-test
#The basic idea behind t-test is the inference problem from a small sample size data set to test 
#whether its sample mean may have large deviation from the true population mean.

#A very common problem you will encounter is having two data sets and you want to test whether 
#the two sets are coming from the same (assuming) normal distributions.

#In t-test, the null hypothesis is that the mean of the two samples is equal. This means that the
#alternative hypothesis for the test is that the difference of the mean is not equal to zero. 
#In a hypothesis test, we want to reject or accept the null hypothesis with some confidence 
#interval. Since we test the difference between the two means, the confidence interval in this 
#case specifies the range of values within which the difference may lie.

#The t-test will also produce the p-value, which is the probability of wrongly rejecting the null
#hypothesis. The p-value is always compared with the significance level of the test. 
#For instances, at 95% level of confidence, the significant level is 5% and the p-value is 
#reported as p<0.05. Small p-values suggest that the null hypothesis is unlikely to be true. 
#The smaller it is, the more confident we can reject the null hypothesis.


#Use the sleep data from R where there are 20 samples in two groups (group 1 and 2, each with 
#10 samples) that show the effect of two soporific drug to increase the hours in sleep
sleep
?sleep
plot(extra ~ group, data = sleep)

#there is naturally an overlap but the mean (half of the rectangle height) is different. 
#Can we confidently say that the two groups have different means?

t.test(extra ~ group, data=sleep)

#Based on the result, you can say: at 95% confidence level, there is no significant difference 
#(p-value = 0.0794) of the two means. Here you should accept the null hypothesis that the two 
#means are equal because the p-value is larger than 0.05.  The maximum difference of the mean 
#can be as low as -3.37 and as high as 0.21.  


#Z-Test

#Z-Distribution

?dnorm()  #Density, distribution function, quantile function and random generation 

#You will find the same basic functions for other distributions you may need, such as
?dt() ; ?dpois() ; ?dbinom() ; ?dchisq()

#Draw a z curve, a density function for z, Z is a variable that is normally distributed 
#with the mean 0 and the standard deviation 1

#Create a sequence of numbers for your x axis , -4 to 4 with interval 0.01
x<-seq(-4, 4, 0.01) 

#plot for each x the probability to draw x at random from the distribution
plot(x, dnorm(x), type="l")

#You can figure out what the attribute type= does by removing it 
plot(x, dnorm(x))
curve(dnorm(x), from= -4, to=4)

#Or by looking up ?plot()

#Distribution function, or Cumulative distribution function.
#For each x, what is the probability to draw x or a value lower than x
plot(x, pnorm(x))

#Quantile function is when you ask what value for z is at the 25% percentile. 
qnorm(0.25)

#You may want to find the interval of values for z that includes 95% of the distribution.
#Useful for calculating confidence intervals 
qnorm(c(0.025, 0.975))

#round the results, not for calculations but foe presentation. 
round(qnorm(c(0.025, 0.975)),2)

#it is very useful to draw graphs to enchance understanding.
#practice using the functions polygon() and text() to add clarity to your graphs.

#Plot blood pressure

#z-tests
bt <- seq(60, 120, 1) 
bt

#assume mean to be 90 and sd=10
plot(bt, dnorm(bt, 90, 10), type="l", xlim=c(60, 120), main="blood pressure") 

#Let's use the normal distribution for a statistical test.

#one tailed test
plot(bt, dnorm(bt, 90, 10), type="l", xlim=c(60, 120), main="one tailed test") 
pnorm(72, 90, 10) # probability of randomly selecting a subject bt 72 or lower
abline(v=72) # Draw a line for 72 . v is the x-value for a vertical line 
cord.x <- c(60,seq(60,72,1),72) 
cord.y <- c(0,dnorm(seq(60, 72, 1), 90, 10),0) 
polygon(cord.x,cord.y,col='skyblue') 
text(70, 0.005, "blue area = p = 0.0359") #got p value from pnorm(72, 90, 10)

#if you measure the blood pressure of a person and it turns out to be 72 then it's 
#quite possible that this person belongs to the population with the mean 90 and the 
#standard deviation 10 For this event to happen to draw a person that deviates to 72 
#or even lower blood pressure the probability that this would happen is 0.0359


#if you draw a random sample you are interested in not if this person has a blood pressure lower 
#than the certain mean, but what you are interested is whether this blood pressure deviates a certain amount 
#from the mean

#two-tailed test
bt <- seq(60, 120, 1) 
plot(bt, dnorm(bt, 90, 10), type="l", xlim=c(60, 120), main="two-tailed test") 
pnorm(72, 90, 10)  
abline(v=72) 
cord.x <- c(60,seq(60,72,1),72) 
cord.y <- c(0,dnorm(seq(60, 72, 1), 90, 10),0) 
polygon(cord.x,cord.y,col='skyblue') 

cord.x1 <- c(108,seq(108,120,1),120) 
cord.y1 <- c(0,dnorm(seq(108, 120, 1), 90, 10),0) 
polygon(cord.x1,cord.y1,col='skyblue') 
text(65, 0.005, round(pnorm(72, 90, 10), 3)) 
text(115, 0.005, round(pnorm(72, 90, 10), 3)) 
text(75, 0.02,  " p = 0.072 "  ) 

#the p-value for this two-tailed test is 36 + 36, 72. So the p-value is 0.072.
#This is the probability that you would randomly draw a person from the population 
#that deviates 18 units up or down from the mean 90


#Z, the standard normal distribution
#it allows us to take a sample and then describe how much it deviates from a hypothesized population

#By looking up the area under the Z curve from your measurement and then further away from 
#the mean you will be able to tell how common it would be to to take such a sample
#or a more extreme sample. This area is also referred to as the p-value

#If the sample deviates a lot from the mean we are going to reject the null hypothesis
#and state that this sample probably belongs to some other distribution with a different mean.
#This is called the alternative hypothesis. A common choice is to reject
#the null hypotheses if the p-value is lower than 0.05


#Using the Z distribution to decide whether a sample belongs to a population or not is actually
#a hypothesis test and as such it's called the Z test
