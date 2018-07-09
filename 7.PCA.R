#####8. PCA#####

#Use dataset USArrests
?USArrests
str(USArrests)
head(USArrests)
names(USArrests)
states=row.names(USArrests)
states

#Find means of all columns

#Note that the apply() function allows us to apply a function-in this case,
#the mean() function-to each row or column of the data set. The second
#input here denotes whether we wish to compute the mean of the rows, 1,
#or the columns, 2

apply(USArrests,2,mean)

#We notice that the variables have vastly different means.

#Analyse the variance
apply(USArrests,2,var)

#perform principal components analysis using the prcomp() function
pr.out=prcomp(USArrests , scale=TRUE)

names(pr.out)

#By default, the prcomp() function centers the variables to have mean zero.
#By using the option scale=TRUE, we scale the variables to have standard
#deviation one.

#The center and scale components correspond to the means and standard
#deviations of the variables that were used for scaling prior to implementing PCA

pr.out$center
pr.out$scale

#The rotation matrix provides the principal component loadings; each column
#of pr.out$rotation contains the corresponding principal component
#loading vector

pr.out

pr.out$rotation

dim(pr.out$x)
pr.out$x

#We can plot the first two principal components as follows:
biplot (pr.out , scale =0) 

#The scale=0 argument to biplot() ensures that the arrows are scaled to biplot() represent the 
#loadings; other values for scale give slightly different biplots with different interpretations

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot (pr.out , scale =0)

#The first loading vector places approximately equal weight on Assault, Murder, and Rape, 
#with much less weight on UrbanPop. Hence this component roughly corresponds to a measure of overall
#rates of serious crimes. The second loading vector places most of its weight
#on UrbanPop and much less weight on the other three features. Hence, this
#component roughly corresponds to the level of urbanization of the state

#crime-related variables are correlated
#with each other-states with high murder rates tend to have high
#assault and rape rates-and that the UrbanPop variable is less correlated
#with the other three.

#states with large positive scores on the first component,
#such as California, Nevada and Florida, have high crime rates, while
#states like North Dakota, with negative scores on the first component, have
#low crime rates. California also has a high score on the second component,
#indicating a high level of urbanization, while the opposite is true for states
#like Mississippi. States close to zero on both components, such as Indiana,
#have approximately average levels of both crime and urbanization

#The prcomp() function also outputs the standard deviation of each principal component
pr.out$sdev

#The variance explained by each principal component is obtained by squaring
pr.var=pr.out$sdev ^2
pr.var

#To compute the proportion of variance explained by each principal component,
#we simply divide the variance explained by each principal component
#by the total variance explained by all four principal components:

pve=pr.var/sum(pr.var)
pve

#We see that the first principal component explains 62.0 % of the variance
#in the data, the next principal component explains 24.7 % of the variance,
#and so forth. We can plot the PVE explained by each component, as well
#as the cumulative PVE, as follows:

plot(pve , xlab=" Principal Component ", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component ", ylab="
     Cumulative Proportion of Variance Explained ", ylim=c(0,1),
     type='b')


##########################################################################

###CLUSTERING###
library(cluster)
library(HSAUR)
library(fpc)

# Kmeans on original PCA of USArrests
clust <- kmeans(pr.out$x[,1:2], centers=2)
clusplot(pr.out$x[,1:2], clust$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
#########################################################################
