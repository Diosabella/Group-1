#######################
#####PROBLEM SET I#####
#######################

                      ###DONE BY###
          
            #Maike Steffen        501684
            #Popova Evgeniia      411751
            #Johannes Hofheinz    453293

#install the R packages GLMsData, ggplot2, class, 
#colorspace, caret, glmnet and leaps

install.packages(c("GLMsData", "ggplot2", "class", 
                   "colorspace", "caret", "glmnet", "leaps", "tidyverse"))

############################
#MULTIPLE LINEAR REGRESSION#
############################

#We model lung  capacity.
#We use the data  set lungcap (a part ofther R package
#GLMsData)  that  gives information  on body variables
#and  on  smoking  habits  of  a sample of 654 youths,
#aged between 3 and 19, in the area of East Boston
#during middle to late 1970s.

#We will predict forced expiratory volume FEV,
#a measure of lung capacity.

#For better interpretability, we transform the height
#from inches to cm (One inch corresponds to 2.54cm)
#Then we fit a multiple normal linear regression model
#to the data set with log(FEV) as response and the 
#other variables as covariates. 

library(GLMsData)
data("lungcap")
lungcap$Htcm=lungcap$Ht*2.54
lung_model= lm(log(FEV) ~ Age + Htcm + Gender + Smoke, data=lungcap)
summary(lung_model)

# (1) Write down the equation for the fitted lung_model
#
#     log(FEV)_hat = beta0_hat + beta1_hat*Age + beta2_hat*Htcm + beta3_hat*Gender + beta4_hat*Smoke
#
#In the regression model the variables Gender and Smoke are both 
#binary and take the value of 1, meaning that
#the log(FEV) is predicted for "males" (opposed to "females") and 
#persons who "smoke" (opposed to persons who do not smoke)'

# (2) Why is log(FEV) used as a response instead of
#     FEV? To answer this question, plot FEV against the other
#     variables - e.g. against Htcm - and argue that there is
#     a non-linear association

library(tydiverse)
library(ggplot2)

ggplot(data=lungcap, aes(x=Htcm, y=FEV, na.rm=TRUE)) +
  labs(x = "the_height_in_cm", y= "forced_expiratory_volume")+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "auto", col ="red", se = FALSE)

'We see that automatically generated regrerssion line is curved upwards. 
This displays an exponential - non-linear - relationship. '

#log(FEV) is used to capture this non-linearity
#Using log(FEV) allows to preserve the linear model
#while the effective relationship between the dependent and independent
#variables is non-linear
#
#Compare to log(FEV), which produces the following plot:

ggplot(data=lungcap, aes(x=Htcm, y=log(FEV), na.rm=TRUE)) +
  labs(x = "the_height_in_cm", y= "log_forced_expiratory_volume")+
  geom_point()+
  geom_smooth(method = "auto", col ="red", se = FALSE)

# (3) Explain with words and numerical examples what the
#     following in the summary - output mean. Discuss one continuous
#     and one dummy regressor for the first two aspects

#We have a log-linear model here

#   Estimate:
#                 The estimated beta coefficient
#                 A one unit increase in X will produce an increase in log(Y) of 
#                 expected Beta holding all other variables constant.
#                 Because we have a log-inear model, the estimate mutliplied with 100 give us the 
#                 percentage change in lung volume per additional unit of the explaining variable.

#     
#                 Continuous Regressor: a one unit-change in Htcm corresponds to
#                                       an expected increase in FEV of approximately 1.6% 
#                         
#                 Dummy Regressor: being a male corresponds to an 
#                                  increased forced expiratory volume of
#                                  approximately 2.9%  
#
#   Std.Error:
#                 The standard error is an estimator of the standard deviation of the sampling distribution of the estimator.
#                 It is used to check the accuracy of predictions made with the regression line.
#                 This means, that it gives an estimate of the expected difference in case we ran the model again and again.
#                 We can say that, for the standard error of the estimate for Htcm, the expected log(FEV) can vary by 0.000661. 
#                 For the standard error of the estimate for the binary variable Gender, the expected log(FEV) can vary by 0.011719. 
#
#   Residual standard error: 
#                            A fitted regression model uses the parameters to generate point
#                            estimate predictions which are the means of observed responses 
#                            if you were to replicate the study with the same X values an infinite 
#                            number of times (and when the linear model is true). The difference between
#                            these predicted values and the ones used to fit the model are called "residuals".
#                             
#                            The observed residuals are then used to subsequently estimate the variability in 
#                            these values and to estimate the sampling distribution of the parameters. When 
#                            the residual standard error is exactly 0 then the model fits the data perfectly (likely due to overfitting).
#
#   F-statistic:
#                 An f-statistic is used to test whether the coefficients are 
#                 jointly significantly different from zero.
#                 Null hypothesis: all coefficients are equal to zero
#                 Ho: Beta2 = Beta3 = ...BetaN = 0
#                 The alternative hypothesis says that your model fits the data better than the intercept-only model.
#                 H1: Beta2 not equal 0 or Beta3 not equal zero or BetaN not equal zero
#                 If the value of the F-statistic is large, the null hypothesis can be rejected.
#                 Here, we have a value of 694.6 and thus reject the null hypothesis.
#
# (4) What is the proportion of variability explained
#     by the fitted lung_model?
#
#This is given by the Adjusted R-squared value 
#in this case of 0.8095, which is quite high and means
#that 80.95% of variability in Y is explained by 
#the fitted model

# (5) Consider a 14-year-old-male. He is 175cm tall and not
#     smoking. What is your best guess for his log(FEV)?
#
#     log(FEV) = Intercept +  0.023387*Age +  0.016849*Htcm 
#                          +  0.029319*GenderM +  (-0.046067)*Smoke
#     log(FEV) = (-1.943998) + 0.023387*14 + 0.016849*175
#                            + 0.029319*1 + (-0.046067)*0

a<--1.943998 + 0.023387*14 + 0.016849*175 + 0.029319*1 - 0.046067*0

#     Answer: log(FEV)=1.361314
#
#     Construct a 95% prediction interval for his forced 
#     expiratory volume (remember to inverse the logarithm)
#     Do you find this prediction interval useful? Comment

exp(1.361314)
CI_upper = exp(1.361314 + 0.1455*qnorm(0.975))
CI_lower = exp(1.361314 - 0.1455*qnorm(0.975))

# (6) Redo the multiple linear regression, but add an interaction
#     term between Age and Smoke and print the results. Call this 
#     model lung_model2. 

lung_model2= lm(log(FEV) ~ Age + Htcm + Gender + Smoke + Smoke*Age, data=lungcap)
summary(lung_model2)

#What is the meaning of the estimate for the interaction term? 

#The coefficient is the difference between the effect of Age 
#on the expected value of log(FEV) when a person does smoke, 
#and the effect when a person does not smoke.

#Is the interaction term statistically significant?

#No, Pr(>|t|) = 0.16863 

#What is the effect of the inclusion of the interaction term
#on the statistical significance of Smoke and Age?

#The coefficient on Age remains statistically significant
#whereas the coefficient on Smoke is not statistically significant
#anymore and counterintuitively positive.

########################################
#Task 2: Classification (Logit/LDA/KNN)#
########################################

library(GLMsData)
library(class)
library(colorspace)
library(caret)
library(glmnet)
library(ggplot2)
library(tidyverse)
library(leaps)

# Our goal is to predict the outcome of a match (success or failure for player 1) 
# just using the quality statistics x1 from player 1 and x2 from player 2.

t_raw = read_csv("tennisdata.csv")
tn = na.omit(data.frame(y=as.factor(t_raw$Result),
                        x1=t_raw$ACE.1-t_raw$UFE.1-t_raw$DBF.1,
                        x2=t_raw$ACE.2-t_raw$UFE.2-t_raw$DBF.2))
set.seed(4268) # for reproducibility
tr = sample.int(nrow(tn),nrow(tn)/2)
trte=rep(1,nrow(tn))
trte[tr]=0
tennis=data.frame(tn,"istest"=as.factor(trte))
ggplot(data=tennis,aes(x=x1,y=x2,colour=y,group=istest,shape=istest))+
  geom_point()+theme_minimal()

## 2.1
# Get a general overview of the data frame M. How many observations are there? 

# t_raw -> 943 obs.
# tn -> 787 obs. (because NAs have been removed)

# what are the median values of quality statistics?

summary(tn)

# x1:  Median :-24.00  x2: Median :-24.00  

# Display the matrix of pairwise scatterplots and briefly comment on 
# the relationship between y and x1 and resp. between y and x2.

pairs(tennis[,1:3], pch = 19,  cex = 0.5, col=tennis$y)

# We cannot see a linaer relationship between x1 and x2 and y, 
# since y is a binary variable. 
# The produced parwise scatterplot is thus uninformative in this case.
# We would expect that x1 is positvely correlated with y and x2 
# is negatively correlated with y. 

## 2.2
#Perform a logistic regression with the full data set and 
# print the numerical results. 
# Are the estimated coefficients for x1 and x2 statistically significant? 
# In the first match, player 1 has a quality of -25 and player 2's quality 
# is -20. What is the predicted probability that player 1 wins this match? 
# What is the actual result?

logit_tennis <- glm(y ~ x1 + x2, data = tennis, family = "binomial")
summary(logit_tennis)

# both estimates are highly statistically significant with p-values near zero

predictmatch <- function(x1, x2) {
  1/(1 + exp(-(0.028949 + 0.087624*(x1) - 0.084219*(x2))))
}

x1=-25
x2=-20

predictmatch(x1, x2)

# The predicted probablitiy of player 1 winning the first match is 38.29%.
# The real outcome is player 2 winning the match (i.e. y = 0).

## 2.3
library(caret)

new_tennis <- tennis #create a copy "new_tennis" of the tennis dataframe 
predicted <- predict.glm(logit_tennis, new_tennis) #predict the y value using logit regression 

#to save it as column we use:

library(tidyverse)
new_tennis <- mutate(new_tennis, predicted)

predicted <- 
  ifelse(predicted > 0.5, 1, 0) #define a threshold at the 0.5 percent level, 
#"predicted" will take 1 if predicted>0.5
#and 0 otherwise

# make "predicted" take the same level as y
predicted <- factor(predicted, levels = levels(new_tennis$y)) 
confusionMatrix(predicted, new_tennis$y)

(338+227)/(338+227+168+54)

# The classification has a sensitivity of 0.8622
# and a specificity of 0.5747.
# 71.79% of all matches were predicted correclty (accuracy).

392/787
null_rate <- summary(tennis$y)[1]/length(tennis$y)
null_rate

# Compared to the null rate of 49.81%, our classification predicts a 
# higher percentage of the matches correctly.

## 2.4
# Perform a logistic regression with only the training data set 
# and compute the confusion matrix for the test data set. 

tennis_training <- tennis %>% filter(istest ==0)
tennis_test <- tennis %>% filter(istest ==1)

logit_training <- glm(y ~ x1 + x2, data = tennis_training, family = "binomial")
summary(logit_training)

#confusion matrix:
predicted_test <- predict.glm(logit_tennis, newdata = tennis_test, type = "response")
predicted_test <- ifelse(predicted_test > 0.5, 1, 0)
predicted_test <- factor(predicted_test, levels = levels(tennis_test$y))
confusionMatrix(predicted_test, tennis_test$y)

# What is the misclassification error?
library(InformationValue)
predicted_test2 <- as.numeric(as.character(predicted_test))
misClassError(tennis_test$y, predicted_test2, threshold = 0.5)
detach("package:InformationValue", unload=TRUE)

misclasserror_logit <- as.numeric(1 - confusionMatrix(predicted_test, tennis_test$y)$overall["Accuracy"])
misclasserror_logit
# We have a missclassification error of 0.2335.


##2.5
#Perform a linear discriminant analysis (LDA) with only the training data set 
# and compute the confusion matrix for the test data set. 

library(MASS)
lda_fit <- lda(y ~ x1 + x2, tennis_training) #lda model
predicted_lda <- predict(lda_fit, tennis_test)$class #predict y with lda model
confusionMatrix(predicted_lda, tennis_test$y)

# What is the misclassification error?
library(InformationValue)
predicted_lda2 <- as.numeric(as.character(predicted_lda))
misClassError(tennis_test$y, predicted_lda2, threshold = 0.5)
detach("package:InformationValue", unload=TRUE)

misclasserror_lda <- as.numeric(1 - confusionMatrix(predicted_lda, tennis_test$y)$overall["Accuracy"])
misclasserror_lda
# The misclassification error is 0.2335.


## 2.6
# Suppose I know about both players' quality in a specific match 
# in the test data set but I do not know the result. 
# According to LDA, how many matches (from the test data set) can I predict 
# with a probability larger than 80%?

predlda <- predict(lda_fit, tennis_test)$posterior #get prediction pobabilities
predlda80 <- subset(predlda, predlda[,1] > 0.8 | predlda[,2] > 0.8) #filter
nrow(predlda80) #count

# 125 matches can be predicted with a probability larger than 80%.


## 2.7
# Use the following R code to compute the misclassification error for the 
# training data and the test data using KNN classification for all K in ks=1:30. 
# The code saves the error rates of the training and test sets as train.e
# and test.e, each vectors of length 30:

ks = 1:30 
yhat = sapply(ks, function(k){ 
  class::knn(train=tn[tr,-1], cl=tn[tr,1], test=tn[,-1], k = k) 
  #test both train and test 
})
train.e = colMeans(tn[tr,1]!=yhat[tr,]) 
test.e = colMeans(tn[-tr,1]!=yhat[-tr,])

# The optimal K can be chosen by cross-validation. 
# Consider the code below where we divide the training data into 5 folds using
# createFolds from the R package caret.

set.seed(0) 
ks = 1:30 # Choose K from 1 to 30. 
idx = createFolds(tn[tr,1], k=5) 
# Divide the training data into 5 folds. 
# "Sapply" is a more efficient for-loop. 
# We loop over each fold and each value in "ks" 
# and compute error rates for each combination. 
cv = sapply(ks, function(k){ 
  sapply(seq_along(idx), 
         function(j) { 
           yhat = class::knn(train=tn[tr[ -idx[[j]] ], -1], 
                             cl=tn[tr[ -idx[[j]] ], 1], 
                             test=tn[tr[ idx[[j]] ], -1], k = k)
           mean(tn[tr[ idx[[j]] ], 1] != yhat) 
         }) 
})


# Look at the matrix cv. What is the meaning of the stored numbers? 
cv

# The matrix has 30 colums for the values from 1 to 30 that K can take.
# It has 5 lines for each of the 5 folds in which we divided the data.
# So, each of the values is a misclassification error for a certain K and fold.

# Compute the average cv.e (use the colMeans function) 
# and the standard error cv.se of the average CV error over all five folds ..

cv.e <- colMeans(cv)

cv.se <- (apply(cv, 2, sd)/sqrt(5))

# Which K corresponds to the smallest CV error?
which.min(cv.e)
min(cv.e)

# K=22 corresponds to the smallest CV error.

## 2.8
# Plot the misclassification errors using the code below. 

library(colorspace) 
co = rainbow_hcl(3)
par(mar=c(4,4,1,1)+.1, mgp = c(3, 1, 0)) 
plot(ks, cv.e, type="o", pch = 16, ylim = c(0, 0.7), col = co[2], 
     xlab = "Number of neighbors", ylab="Misclassification error")
arrows(ks, cv.e-cv.se, ks, cv.e+cv.se, angle=90, length=.03, code=3, col=co[2]) 
lines(ks, train.e, type="o", pch = 16, ylim = c(0.5, 0.7), col = co[3]) 
lines(ks, test.e, type="o", pch = 16, ylim = c(0.5, 0.7), col = co[1]) 
legend("topright", legend = c("Test", "5-fold CV", "Training"), lty = 1, col=co)

# What can you say about bias in y^(x) and variance if K increases ?

# higher K (smaller flexibility) -> higher bias
# smaller K (higher flexibility) -> higher variance

# When K increases, we get a less flexible classification and therefore a 
# higher bias and lower variance.



## 2.9
# Run the code below and briefly explain the graph that it creates.

k = 50
size = 100
xnew = apply(tn[tr,-1], 2, function(X) seq(min(X), max(X), length.out=size)) 
grid = expand.grid(xnew[,1], xnew[,2]) 
grid.yhat = knn(tn[tr,-1], tn[tr,1], k=k, test=grid) 
np = 300
par(mar=rep(2,4), mgp = c(1, 1, 0)) 
contour(xnew[,1], xnew[,2], z = matrix(grid.yhat, size), levels=.5, 
        xlab=expression("x"[1]), ylab=expression("x"[2]), axes=FALSE, 
        main = paste0(k,"-nearest neighbors"), cex=1.2, labels="")
points(grid, pch=".", cex=1, col=grid.yhat) 
points(tn[1:np,-1], col=factor(tn[1:np,1]), pch = 1, lwd = 1.5) 
legend("topleft", c("Player 1 wins", "Player 2 wins"), 
       col=c("red", "black"), pch=1)
box()

# We see 300 datapoints that were randomly chosen from the tn dataset.
# A classification of those datapoints using 30 nearest neighbors is depicted.
# For the area above the classification line, player 2 is predicted to win 
# and vice versa.
# In the middle of the figure, Where we see many datapoints, the classification
# results in a connected line.


## 2.10
# Run the code again, but choose k=1, k=50 and k=300 in the first line. 
# Compare the emerging graphs. Which of these three models is the most flexible? 
# Explain in one sentence what happens if you set k=500.

# For K = 1, we get several unconnected classification areas. 
# This is the most flexible model.
# For K = 50, the classification line is completely connected and smooth, 
# also for very small/large values of x1 and x2.
# For K = 300, the area above the line, i.e. the area in which player 2 is
# predicted to win, gets larger. 
# When we set K = 500, we do not get a useful classification, since the number
# of nearest neighbors would exceed the number of observations.

############################################
#Task 3: Cross-Validation and Bootstrapping#
############################################

## 3.1
# Fit a multiple linear regression model lm.fit_cs that uses Price, Urban, 
# and US to predict Sales. Print the results.

library(ISLR)
data("Carseats")

lm.fit_cs <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm.fit_cs)


## 3.2
# Using the validation set approach; estimate the test error of this model. 
# In order to do this, perform the following steps:

#3.2a
# Split the sample set into a training set and a validation set, 
# each encompassing half of the data. Use the random seed 2019.

set.seed(2019) 
tr = sample.int(nrow(Carseats),nrow(Carseats)/2)
trte=rep(1,nrow(Carseats))
trte[tr]=0
cs = data.frame(Carseats,"istest"=as.factor(trte))

# 3.2b
# Fit a multiple linear regression model lm.fit_cstr using only the training 
# observations. Briefly compare the results with lm.fit_cs (with regard to 
# estimates, standard errors and p-values of the coefficients).

cstr <- subset(cs, istest == 0)
cste <- subset(cs, istest == 1)


lm.fit_cstr <- lm(Sales ~ Price + Urban + US, data = cstr)
summary(lm.fit_cstr)

# The the estimate for price does not change much and stays highly significant.
# The standard error and p-value are only slightly higher for the training data.
# The estimate for Urban changes from slightly negtive to positive, but is for 
# both estimations not significant.
# We see a more drastic change for the estimate for US: For the whole dataset, 
# it has a value 1.2 and is significant, but when we only use the training data,
# the estimate becomes smaller, the standard error becomes larger and
# increases from nearly zero to 0.032.

# 3.2.c
# Predict the response for all 400 observations and calculate the 
# mean squared error (MSE) of the observations in the validation set.

predicted_cs <- predict(lm.fit_cstr, cs)
#predicted_cste <- predict(lm.fit_cstr, cste)

library(ModelMetrics)
mse(cste$Sales, predicted_cs)

# 3.2.d
# How does your answer to c. change if you use the random seeds 2018 or 2020 
# instead of 2019 to split the data set?

set.seed(2019) 
tr = sample.int(nrow(Carseats),nrow(Carseats)/2)
trte=rep(1,nrow(Carseats))
trte[tr]=0
cs = data.frame(Carseats,"istest"=as.factor(trte))
cstr <- subset(cs, istest == 0)
cste <- subset(cs, istest == 1)
lm.fit_cstr <- lm(Sales ~ Price + Urban + US, data = cstr)
summary(lm.fit_cstr)
predicted_cstr <- predict(lm.fit_cstr, cs)
predicted_cste <- predict(lm.fit_cstr, cste)
library(ModelMetrics)
mse(cste[[1]], predicted_cs)

# When using random seed 2018, the mse increases to 9.805767.
# For the random seed 2020, the mse is even higher (11.8333).

# 3.2.e
# Compute the LOOCV estimate for the MSE using the cv.glm function.
# seed 2019 used

glm.fit_cstr <- glm(Sales ~ Price + Urban + US, data = cstr)

library(boot)
cv.glm(cstr, glm.fit_cstr)$deltas

#6.993081

# The first component of delta is the average mean-squared error 
# that you obtain from doing K-fold CV.
# The second component of delta is the average mean-squared error that you 
# obtain from doing K-fold CV, but with a bias correction. 

## 3.3
# Use the regsubsets function to find the best subset consisting of three 
# variables to predict Sales, excluding ShelveLoc. 
# What do the chosenpredictors indicate? 
# Fit a multiple linear regression using these three variables and 
# compute the LOOCV estimate for the MSE. Compare with your answer to 2e.

library(leaps) #regsubsets is part of the leaps library 
library(MASS) 
library(ggplot2)
re <- regsubsets(Sales ~ CompPrice + Income + Advertising + Population + Price + Age + Education + Urban + US, data = cstr, nvmax = 3)
summary(re)

glm.fit_subs <- glm(Sales ~ Price + CompPrice + Advertising, data = cstr)

library(boot)
cv.glm(cstr, glm.fit_subs)$deltas

# The mse decreased due to the best subset selection. 

## 3.4
# Compute the mean of the Sales variable. In a next step, 
# we want to understand the potential distribution of the mean. 
# For this purpose, create 20 bootstrapped replicates of the data, 
# using random seed 1. Then apply the mean function to each bootstrapped 
# replicate. Enter the command 20 times and write down the range of the mean, 
# i.e. lowest and the largest number.

mean(cstr$Sales)
nboot <- 20 # number of bootstrap samples
nreps <- 20 # number of repetitions
bootstrap.means <- matrix(rep(NA, nboot), ncol = nreps, nrow = nboot)
set.seed(1)

# Nested for loop

for(i in 1:nreps){
  for (j in 1:nboot){
    bootstrap.means[j,i] <- mean(sample(cstr$Sales, replace=TRUE))
  }
}
print(bootstrap.means)

range(bootstrap.means,na.rm=FALSE)

#Mean ranges from 7.27255 to 8.35565

## 3.5
# Repeat the above analysis by using boot(). 
# In addition, what are the confidence intervals?

library(boot)
set.seed(1)
function.means <- function(cstr, index){
  return(mean(cstr[index]))
}

bootstrap<-boot(data=cstr$Sales, statistic=function.means,R=20)
view(bootstrap)
bootstrap

confidence_interval = c(bootstrap$t0-1.96*0.1661277, bootstrap$t0+1.96*0.1661277) 
confidence_interval 

##############################################
# Tesk 4: MODEL SELECTION AND REGULARIZATION # 
##############################################

library(ggplot2)

set.seed(1007) # for reproducibility 
sm = sample.int(nrow(diamonds),nrow(diamonds)/10) 
smbi=rep(1,nrow(diamonds)) 
smbi[sm]=0
diamonds2=data.frame(diamonds,"issmall"=as.factor(smbi)) 
small = (diamonds2$issmall==0) 
diamondssm=diamonds2[small,]

## 4.1
# Get an overview of the diamondssm data. 
# What are three highest prices in the dataset? 
# How many carats do those diamonds weigh? 
# What is the mean weight? Which colour is the most prevalent?
# Plot price against carat as well as their logged forms against each other.

library(tidyverse)
head(diamondssm)
summary(diamondssm)

diamondssm %>%
  arrange(desc(price)) %>%
  top_n(3) %>%
  slice(1:3)

# The three highes prices are 18795, 18779 and 18759
# They wheigh 2.00, 2.06 an 2.00 carats respectively.

diamondssm %>%
  summarise(mean(carat))

# The mean weight is 0.8002299 carats.

diamondssm %>%
  count(color) %>%
  arrange(desc(n))

# The most prevalent color is G.

ggplot(diamondssm, aes(x=carat, y=price)) + 
  geom_point(col="darkblue") + 
  geom_smooth(method = "loess", se=FALSE) 

ggplot(diamondssm, aes(x=log(carat), y=log(price))) + 
  geom_point(col="darkblue") + 
  geom_smooth(method = "lm")

diamondssm=data.frame(diamondssm,"logprice"=log(diamondssm$price), 
                      "logcarat"=log(diamondssm$carat))
diamonds3 <- subset(diamondssm, select = -c(price, carat, issmall))

## 4.2
# Perform forward and backward stepwise selection to choose the best subset of 
# predictors for logprice, using the diamonds3 dataset. 
# Compare the two results. 

# define the full model:
lm.full <- lm(logprice ~ ., data = diamonds3) #model with all possible regressors

# backwise stepwise selection:
library(MASS)
step.backward <- stepAIC(lm.full, direction = "backward", trace=TRUE) 
summary(step.backward) 
step.backward$anova

# define the null model:
lm.null <- lm(logprice ~ 1, data = diamonds3)

# forward stepwise selection:
step.forward <- stepAIC(lm.null, direction = "forward", trace=FALSE, 
                        scope = ~ cut + color + clarity + depth + table +
                          x + y + z + logcarat)
summary(step.forward) 
step.forward
step.forward$anova

# With backward stepwise selection, the variable "table" gets removed from the 
# final model . WIth forward stepwise selection, the algorithm adds all 
# variables of the full model, except for "table", ordered by their relevance.
# Thus, both techniques yield to price being regressed on logcarat, clarity, 
# color, cut, x, depth, y and z.

step.both <- stepAIC(lm.full, direction = "both", trace=FALSE) 
step.both$anova

# Using adjusted R² as criterion, 
# how large is the best subset from the backward stepwise selection?

library(leaps)
models <- regsubsets(logprice ~ ., method=c("backward", "adjR"), data = diamonds3, nvmax = 8)
summary(models)

# Using the adjusted R squared as a criterion, the backward stepwise selection 
# will define the best subset as follows:
#
#"cut","color","clarity", "logcarat"

## 4.3
# What are the main differences between using adjusted R² for model selection 
# and using cross-validation (with mean squared test error MSE)?

#Rsquared is a measure that gives the proportion of the variance of a dependent variable that is
#explained by an independent variable, or the measure of "fit" of the model
#whereas cross-validation uses MSE as criterion, which is the estimate of variance of residuals,
#or "non-fit" of the model

## 4.4

library(glmnet)

#Ridge regression

a<-model.matrix(logprice~.,diamonds3)[,-1]
b<-diamonds3$logprice
grid<-10^seq(10,-2,length=100) ## for values of lambda
ridge.regression<-glmnet(a,b,alpha=0, lambda=grid)
dim(coef(ridge.regression)) ## 24  100

ridge.regression$lambda[70]
coef(ridge.regression)[,70]
MLR<-lm(logprice~., diamonds3)
coef(MLR)

#The values of the estimates of the ridge regression
#seem to be smaller than the values provided by multiple
#linear regression model 

#Lasso regression
lasso.regression<-glmnet(x,y,alpha=1)
lasso.regression$lambda[20]
coef(lasso.regression)[,20]
MLR<-lm(logprice~., diamonds3)
coef(MLR)

#We see that in a Lasso regression most coefficients are close
#to zero (except for logcarat and intercept value)
#in contrast to the multiple linear regression midel
