#####7. CLUSTERING#####

install.packages("ggfortify")
library(ggfortify)

#Load iris data into a dataframe
iris
df <- iris[c(1, 2, 3, 4)]

#Do PCA on the data frame and plot the first 2 components
autoplot(prcomp(df))

#Plot the PCA by color coding the data points by the flower species
autoplot(prcomp(df), data = iris, colour = 'Species')

#Labeling the data points with record number
autoplot(prcomp(df), data = iris, colour = 'Species', label = TRUE, label.size = 3)

autoplot(prcomp(df), data = iris, colour = 'Species', label = FALSE, label.size = 3)

#Plot the loadings of features learnt from PCA
autoplot(prcomp(df), data = iris, colour = 'Species', loadings = TRUE)

#Plotting all the information simultaneously
autoplot(prcomp(df), data = iris, colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

#Clustering on principal components of Iris Data
library(cluster)

autoplot(clara(iris[-5], 3))

#Clustering with 3 clusters and visualizing the cluster assignment
autoplot(fanny(iris[-5], 3), frame = TRUE)

#Clustering with n=3 and visualizing clusters with different frame
autoplot(pam(iris[-5], 3), frame = TRUE, frame.type = 'norm')

