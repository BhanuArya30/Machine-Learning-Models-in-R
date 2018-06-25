#####1. INTRO TO R#####

#Dataframes, R dataset mtcars
mtcars
df<-mtcars

#Dimensions of data frame
dim(df)

#Number of rows and columns
cols<-ncol(df)
nrow(df)

#Understanding structure of dataframe 
str(df)

#Checking top 5 rows of dataframe
head(df,5)

#Extracting one column from dataframe
df$mpg

#Check type of variable
typeof(df$mpg)

#Summary
summary(df$mpg)
summary(df)

#Selcting subset of rows from dataframe
subRow <- df[5:15,]
subRow
#Selecting subset of coumns from dataframe
subCol<- mtcars[,2:4]
#Select subset of non conttiguous columns
subColNonConti<- mtcars[, c(2:4,6:9,11)]
subColNonConti
#Selecting subset by removing certain columns
subColRem<- mtcars[,-c(2:4,6)]
subColRem

subCol<- mtcars[,c('cyl','mpg')]
subCol

#Split data set
mtcars1<- mtcars[1:16,]
mtcars1
mtcars2<- mtcars[17:nrow(mtcars),]
mtcars2

#merge 2 datasets
mtcarsFull <- rbind(mtcars1,mtcars2)
mtcarsFull

#order dataset by desired feature in descending order
mtcarsFull<- mtcarsFull[order(-mtcarsFull$mpg),]

mtcarsFull

#Statistical summary of variabes
summary(mtcarsFull)

#analyse vehicles with mpg>20, filtering
mtcarsHighMpg <- mtcarsFull[mtcarsFull$mpg>20,]

mtcarsHighMpg

# check proportion of cylinders in high mpg cars, frequency distribution of categorical variables
counts<- table(mtcarsHighMpg$cyl)
counts

#pie chart
pie(counts, main="Pie Chart of Cylinders")

mean(mtcarsHighMpg$mpg)

#Scatter plot, mpg vs disp
plot(mtcars$mpg,mtcars$disp)

mean(mtcarsFull$mpg)

# check proportion of cylinders in all the cars
counts<- table(mtcarsFull$cyl)
counts

#pie chart
jpeg('rplot3.jpg')
#pie chart
pie(counts, main="Pie Chart of Cylinders")
dev.off()
