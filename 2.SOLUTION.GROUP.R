#########################################
#####Problem Set II: Beyond Linearity#####
#########################################

                ###DONE BY###

                #Maike Steffen        501684
                #Popova Evgeniia      411751
                #Johannes Hofheinz    453293


#install the R packages GLMsData, ggplot2, class, 
#colorspace, caret, glmnet, leaps, ISLR, ggplot2, 
#randomForest, gbm, tree, e1071 and mlr

install.packages(c("GLMsData", "ggplot2", "class", 
                   "colorspace", "caret", "glmnet", 
                   "leaps", "tidyverse", "ISLR", 
                   "randomForest", "gbm", "tree", "e1071","mlr",
                   "rpart", "rpart.plot", "ModelMetrics"))

############################
#Task 1: Tree-Based Methods#
############################


library(ISLR)
library(tree)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(gbm)
library(e1071)
library(mlr)

data("Carseats")


#1.) Split the data set into a training set and a test set 
# (each consisting of 200 observations); use the random seed 815

set.seed(815)
tr <-  sample(nrow(Carseats), nrow(Carseats) / 2)
cstr <-  Carseats[tr, ]
cste <-  Carseats[-tr, ]


#2.) Fit a regression tree to the training set. Plot the tree, and 
# interpret the results. What is the test error rate?

reg.tree <- tree(Sales~.,data = cstr)
summary(reg.tree)

#Plotting results
plot(reg.tree)
text(reg.tree,pretty = 0)

# Interpretation
# The plot shows the regression tree for our training data. It splits the data
# using a series of splitting rules. At the top, the data gets seperated by 
# Shelveloc, in the categorie bad or medium and good. The algorithm further 
# devides the data using different characteristics. On the bottom of the tree,
# we can see the predicted sales for each branch. E.g. the branch on the right 
# predicts sales of 7.8 given ShelveLoc=good, Price >= 113 and Age>60.

#Calculating the test error
yhat_reg.tree <- predict(reg.tree,newdata = cste)

library(ModelMetrics)
mse(cste$Sales, yhat_reg.tree)

# The test error rate is about 4.52.


### 1.3
# Use cross-validation in order to determine the optimal level of 
# tree complexity. Does pruning the tree improve the test MSE?

cv <- cv.tree(reg.tree, FUN = prune.tree)
plot(cv$size, cv$dev, type = "b")
opt <- which.min(cv$dev)

pruned.tree <- prune.tree(reg.tree, best=5)
plot(pruned.tree)
text(pruned.tree)

yhat_pruned.tree <- predict(pruned.tree,newdata = cste)

library(ModelMetrics)
mse(cste$Sales, yhat_pruned.tree)

# No, pruning the tree does not improve MSE in this case; it increases from
# 4.52 to 5.07.


### 1.4
# Make use of the resampling command from the mlr library and the following 
# R code [adjust the third line with the name you gave your training data set]:

library(mlr) 
lrn.carseats <- makeLearner("regr.rpart") 
traintask.carseats <- makeRegrTask(data = cstr, target = "Sales") 
set.seed(111) 
resample.res <- resample(lrn.carseats, traintask.carseats, resampling = cv10, 
                         measures = "mse", show.info = FALSE) 
resample.res$measures.test

# What do these values tell you about the consistency of your results?


### 1.5
# Use the bagging approach in order to analyse the data. Get the test error 
# and identify the three most important variables with the importance() function. 

library(randomForest)
set.seed(815)
bagging <- randomForest(Sales~.,data=cstr, mtry = 10, importance = TRUE)
yhat_bagging <- predict(bagging, newdata=cste)
MSE_bagging <- mean((yhat_bagging-cste$Sales)^2)
MSE_bagging

#The MSE (test error) reduces to 2.718382

#Identifying the tree most important variables
importance(bagging)
# variable importance plot:
varImpPlot(bagging)

# The variables ShelveLoc, Price and CompPrice are the strongest predictors
# for sales.


### 1.6
# Use random forests to analyse the data. What test MSE do you obtain? 
# Use the importance() function to determine which variables are most important.

mse.vec <- NA
for (a in 1:10){
  rf.carseats <-  randomForest(Sales ~ . , data=cstr, 
                               mtry=a, ntree=500, importance=TRUE)
  rf.pred <-  predict(rf.carseats, cste)
  mse.vec[a] <- mean((cste$Sales - rf.pred)^2)
}

# best model
which.min(mse.vec)

# 7

# test MSE
mse.vec[which.min(mse.vec)]

# The test error is 2.65.

rf.carseats <-  randomForest(Sales ~ . , data = Carseats, 
                             mtry=7, ntree=500, importance=TRUE)
importance(rf.carseats)

# variable importance plot:
varImpPlot(rf.carseats)


# The variables ShelveLoc, Price and CompPrice are the most important.


# What is the effect of mtry on the error rate?

# mtry specifies the number of variables samples at each split. The optimal
# value of mtry=7 decreases the test error compared to the bagging approach
# from 2.72 to 2.65. The random forest forest approach is thus preferable.


### 1.7
# Use boosting to create a regression tree. How does it perform in terms of 
# test MSE compared to the previous models?

library(gbm)
set.seed(815)
boost <- gbm(Sales~., data=cstr, distribution="gaussian",
                 n.trees=1000, interaction.depth=4)


yhat.boost <- predict(boost, newdata=cste, n.trees=1000)
mean((yhat.boost-cste$Sales)^2)

# The test error decreases 2.27. Hence, this model performs better than the 
# previous models. 


###################################
# Task 2: Support Vector Machines #
###################################

library(ISLR)
library(ggplot2)
library(randomForest)
library(gbm)
library(tree)
library(e1071)
library(mlr)

load(url("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/ESL.mixture.rda")) 
#names(ESL.mixture)
#prob gives probabilites for each class when the true density functions are known 
#px1 and px2 are coordinates in x1 (length 69) and x2 (length 99) 
#where class probabilites are calculated rm(x,y)
attach(ESL.mixture)
dat=data.frame(y=factor(y),x) 
xgrid=expand.grid(X1=px1,X2=px2) 
#Create a data frame from all combinations of the supplied vectors or factors
par(pty="s")
plot(xgrid, pch=20,cex=.2) 
points(x,col=y+1,pch=20)
contour(px1,px2,matrix(prob,69,99),level=0.5,add=TRUE,col="blue",lwd=2) 
#optimal boundary


### 2.1
# Briefly define the terms Bayes classifier, Bayes decision boundary 
# and Bayes error rate in your own words [ISLR, pp.37-39] 
# and use them to explain the graph that results from the R code above.

#   Bayes Classifier:
# The Bayes Classifier predicts the probablility of an object belonging to 
# a certain class, conditional on its characteristics.
#
#   Bayes Decision Boundary:
# The Bayes decision boundary is used in the case of two classes. 
# It contains the set of points at which the probability of belonging
# to either class is exactly 50%. It thereby assignes the observations 
# to the classes.
#
#   Bayes Error Rate:
# Bayes error rate is the test error rate. It is minimised, because the Bayes
# Classifier assignes an object to whatever class has the highest conditional
# probability.
#   
# In the graph produced by the code above, we see in red observations that 
# belong to one class and in black observations of the other class.
# The blue line is supposedly the Bayes decision boundary that marks
# the border at which the conditional probablility of belonging to one class
# is 50%. Thus, The region above blue line reflects the set of points for which
# the probability of being assigned to "red" class is over 50%. 
# This classification minimizes the test error, even though we can se
# a few misclassifications.

### 2.2
# In cases where we know the Bayes decision boundary, 
# do we still need a test set?

# For other methods, we need the test data set to be able to compare test errors
# in order to identify the best model.
# When using the Bayes decision boundary, the test error is always minimised.
# We do therefore not need a test data set and it is better to use all 
# available data to train the algorithm.



library(e1071)
# support vector classifier
svcfits=tune(svm,factor(y)~.,data=dat,scale=FALSE,kernel="linear",
             ranges=list(cost= c(1e-2,1e-1,1,5,10))) 
summary(svcfits)
svcfit=svm(factor(y)~.,data=dat,scale=FALSE,kernel="linear",cost=0.01) 
# support vector machine with radial kernel 
set.seed(4268)
svmfits=tune(svm,factor(y)~.,data=dat,scale=FALSE,kernel="radial",
             ranges=list(cost= c(1e-2,1e-1,1,5,10),gamma=c(0.01,1,5,10))) 
summary(svmfits) 
svmfit=svm(factor(y)~.,data=dat,scale=FALSE,kernel="radial",cost=1,gamma=5)

# the same as in a - the Bayes boundary par(pty="s")
plot(xgrid, pch=20,cex=.2) 
points(x,col=y+1,pch=20)
contour(px1,px2,matrix(prob,69,99),level=0.5,add=TRUE,
        col="blue",lwd=2) #optimal boundary

# decision boundaries from svc and svm added 
svcfunc=predict(svcfit,xgrid,decision.values=TRUE) 
svcfunc=attributes(svcfunc)$decision
contour(px1,px2,matrix(svcfunc,69,99),level=0,add=TRUE,col="red") 
#svc boundary 
svmfunc=predict(svmfit,xgrid,decision.values=TRUE) 
svmfunc=attributes(svmfunc)$decision
contour(px1,px2,matrix(svmfunc,69,99),level=0,add=TRUE,col="orange") 
#svm boundary


### 2.3
# What is the difference between a support vector classifier and 
# a support vector machine?

#   Support Vector Classifier:
# The aim of the support vector classifier is to draw a hyperplane that is based
# on a linear function into a multidimensional space to seperate classes.
# It maximises a soft margin, i.e. the distance between the hyperplane and 
# the data points, while allowing for individual datapoints to be within 
# the margin.
#   Support Vector Machine:
# A support vector machine comes to use, if the data cannot be accurately 
# classified by a linear boundary. Based on the inner products of observation
# vectors, it provides a non-linear classification.


### 2.4 
# What are parameters for the support vector classifier and the support vector 
# machine? How are they chosen above?

#   Support Vector Classifier:
# SVC has the tuning parameter C (that determines the amount of the
# violations to the margin that we tolerate). That parameter C
# is chosen by cross-validation (tune function). If C is small 
# the margins are narrow and are rarely violated. If C is large
# the margin is wider and we allow more violations to it
# (fitting the data not so well and potentially more biased classifier, 
# but less variance)

# As the SVC uses a linear boundary between two classes the argument
# in the svm() function is defined as kernel="linear", 
# cost=c(1e-2,1e-1,1,5,10) defines the tuning parameter of SVC

#   Suport Vector Machine:
# uses argument kernel="radial" - the resulting SVM has a non-linear boundary 


### 2.5
# How would you evaluate the support vector machine decision boundary compared 
# to the Bayes decision boundary?

# The support vector machine uses interactions between the characteristics,
# whereas the Bayes decision boundary trests the characteristics as independant
# from each other.
# It thus depends on the nature of our data, which methods performs better.
# The support vector machine is preferable, when the characteristics are
# correlated with each other.


############################################
#Task 3: Unsupervised Learning (Clustering)#
############################################


#Here we will perform hierarchical clusteringon the states
#in the US Arrests data. 

library(ISLR)
data("USArrests")

# 3.1 

#Cluster  the  states.  Use hierarchical  clustering
#with  complete  linkage  and  Euclidean distance

data.frame <- USArrests

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#Clustering observaitions using complete linkage and euclidian distance

hierarchical_clustering <- hclust(dist(data.frame, method = "euclidean"), method = "complete")

#Plot the dendrogram, the numbers at the bottom identify each obs

par(mfrow=c(1,1))
plot(hierarchical_clustering, main="Hierarchical clustering using complete linkage",
     xlab = "", sub = "", cex=.9)

# 3.2

#Cut  the  dendrogram  at  a  height resulting in five distinct
#clusters. Which  of  the  five clusters is the smallest?
#Which states does it contain?

#To determine the cluster labels associated
#with a given cut on the dendrogram 

five_clusters<-cutree(hierarchical_clustering, 5)
five_clusters

names(five_clusters)
plot(five_clusters)
table(five_clusters)

#The smalles cluster is cluster number 4, it contains only
#two states "Florida" and "North Carolina"

par(mfrow=c(1,1))
plot(hierarchical_clustering)
abline(h=100, col="green") #the argument h=100 plots the horizontal line at height 100 on the dendrogram
#this is the height that results in five distinct clusters

#3.3

#Hierarchically cluster the states using complete linkage 
#and Euclidean distance, after scaling  the  variables
#to  have  standard  deviation  one. Redo  exercise  2
#and  describe the effect of scaling the variables on the hierarchical
#clustering.

#Redo the Exercise using the scaling

library(ISLR)
data("USArrests")

#As we don't want the clustering algorithm to depend 
#to an arbitrary variable unit, we start by scaling/standardizing
#the data using function scale

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

data.frame <- USArrests
data.frame <- scale(data.frame)
head(data.frame)

#Clustering observaitions using complete linkage and euclidian distance

hierarchical_clustering_2 <- hclust(dist(data.frame, method = "euclidean"), method = "complete")

#Plot the dendrogram, the numbers at the bottom identify each obs

par(mfrow=c(1,1))
plot(hierarchical_clustering_2, main="Hierarchical clustering using complete linkage",
     xlab = "", sub = "", cex=.9)

five_clusters_2<-cutree(hierarchical_clustering_2, 5)
five_clusters_2

names(five_clusters_2)
plot(five_clusters_2)
table(five_clusters_2)

#Now the smallest cluster is claster number 2, which contains
#only one state "Alaska"

par(mfrow=c(1,1))
plot(hierarchical_clustering_2)
abline(h=3, col="green") #now we have to draw a horizontal line at the height 3
#as the data has been rescaled

# 3.4

#Using  the dendrogram  from  ex.  3:  Which  states  are
#the  two  most  similar  ones  (with regard  to  the  data 
#in USArrests)?  Calculate  the  Euclidean  distance  between  
#these two states.

#Iowa and New Hampshire are the most similar states

dist(data.frame)

# Confirm it with the code below
#First, we want to find the two points that are closest 
#together, i.e. to find the smallest non-zero entry in the 
#distance matrix.

m <- as.matrix(dist(data.frame))
 
  # Remove the diagonal from consideration
  diag(m) <- diag(m) + 100000

  # Find the index of the points with minimum distance
  ind <- which(m == min(m), arr.ind = TRUE)
  ind
  
  dist(m)
  m[29,15]
  
  #The Euclidean distance between the two states is 0.2058539
