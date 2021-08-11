library(ggplot2)
library(tidyverse)
library(gridExtra)
library(MASS)
library(car)
library(faraway)
library(broom)
library(corrplot)
library(ROCR)
library(leaps)

setwd("/Users/nicholaslandi/Desktop/MSDS/DS6021/DS6021CODE")

#DATA SETUP
#############################################################
white_wine <- read.csv('wineQualityWhites.csv')
red_wine <- read.csv('wineQualityReds.csv')

#Distribution of quality by color (histogram)
plt1 <- ggplot(data = red_wine, aes(y=quality)) +
  geom_histogram(fill='indianred1', color='indianred4', alpha=0.75)
plt2 <- ggplot(data = white_wine, aes(y=quality)) +
  geom_histogram(fill='lightgoldenrod1', color='lightgoldenrod4', alpha=0.75)
grid.arrange(plt1, plt2, ncol=2)

white_wine <- dplyr::select(white_wine, -free.sulfur.dioxide)
red_wine <- dplyr::select(red_wine, -free.sulfur.dioxide)

red_wine <- red_wine %>%
  mutate(color = 0) #red is 0
white_wine <- white_wine %>%
  mutate(color = 1) #white is 1

red_wine <- red_wine%>%
  mutate(quality_cat = ifelse(quality>=7, 1, 0))
white_wine <- white_wine%>%
  mutate(quality_cat = ifelse(quality>=7, 1, 0))

wine_combined <- rbind(white_wine, red_wine) #combine datasets 

table(red_wine$quality_cat)
table(wine_combined$quality_cat)

#change color to factor
#reds
red_wine$color<-factor(red_wine$color)
levels(red_wine$color)
levels(red_wine$color) <- c("red") 
levels(red_wine$color)

#white
white_wine$color<-factor(white_wine$color)
levels(white_wine$color)
levels(white_wine$color) <- c("white") 
levels(white_wine$color)

#combined wine
wine_combined$color<-factor(wine_combined$color)
levels(wine_combined$color)
levels(wine_combined$color) <- c("red","white") 
levels(wine_combined$color)

#change quality_cat to factor 
#reds
red_wine$quality_cat<-factor(red_wine$quality_cat)
levels(red_wine$quality_cat)
levels(red_wine$quality_cat) <- c("low","high") 
levels(red_wine$quality_cat)

#white
white_wine$quality_cat<-factor(white_wine$quality_cat)
levels(white_wine$quality_cat)
levels(white_wine$quality_cat) <- c("low","high") 
levels(white_wine$quality_cat)

#combined wine
wine_combined$quality_cat<-factor(wine_combined$quality_cat)
levels(wine_combined$quality_cat)
levels(wine_combined$quality_cat) <- c("low","high") 
levels(wine_combined$quality_cat)

#split data into train and test data
set.seed(1) #set seed 
#white
sample_white<-sample.int(nrow(white_wine), floor(.80*nrow(white_wine)), replace = F)
train_white<-white_wine[sample_white, ] #training data 
test_white<-white_wine[-sample_white, ] #testing data

#red
sample_red<-sample.int(nrow(red_wine), floor(.80*nrow(red_wine)), replace = F)
train_red<-red_wine[sample_red, ] #training data 
test_red<-red_wine[-sample_red, ] 

#combined
train_combined <- rbind(train_white, train_red)
test_combined <- rbind(test_white, test_red) 

#remove X and quality from train_red
train_red<- subset(train_red, select = -c(X,quality,color))

#remove X and quality from train_combined 
train_combined <- subset(train_combined, select = -c(X,quality))
#############################################################

#MODEL SELECTION
#############################################################
#Based on EDA, separate models for white and red, perform all possible regression for red data set
#all possible regression WITH RED WINE DATA ONLY
allreg_red <- regsubsets(quality_cat ~., data=train_red, nbest=1, nvmax=10)
summary(allreg_red)
#best 7-predictor model: alcohol, volatile.acidity, sulphates, fixed.acidity, density, residual.sugar, chlorides
#citric.acid, fixed.acidity

#R2
which.max(summary(allreg_red)$adjr2)
#model 9 is the best based on R2
#CP
which.min(summary(allreg_red)$cp)
#model 8 is the best based on Mallow's CP
#BIC
which.min(summary(allreg_red)$bic)
#model 7 based on BIC

#all possible regression model, based on R2 and Mallow's Cp -> 7-predictor model
model1_red <- glm(quality_cat~ alcohol+volatile.acidity+sulphates+fixed.acidity+density+residual.sugar+chlorides, 
                  family='binomial', data=train_red)
summary(model1_red) #AIC score 738.12

#EQUATION for 7-predictor red wine model:
#log(pi(quality)/1+pi(quality)) = 285.10221 + (0.72351)alcohol + (-2.82651)volatile.acidity +
#(3.23950)sulphates +  (0.36775)fixed.acidity + (-299.78128)density + (0.17416)residual.sugar +
#(-7.54886)chlorides

#based on wald's test all predictors appears to be sig 
#####################################################

#CHECKING ALL ASSUMPTIONS FOR 7-PRED MODEL
#####################################################
#1)The outcome is a binary or dichotomous variable like yes vs no, positive vs negative, 1 vs 0.
#2)There is a linear relationship between the logit of the outcome and each predictor variables. 
#Recall that the logit function is logit(p) = log(p/(1-p)), where p is the probabilities of the outcome.
#3)There is no influential values (extreme values or outliers) in the continuous predictors
#4)There is no high intercorrelations (i.e. multicollinearity) among the predictors.

probabilities <- predict(model1_red, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

#ASSUMPTION 1) Linearity assumption
# Select only numeric predictors
model_red <- train_red%>%
  dplyr::select(is.numeric) 
predictors <- colnames(model_red)
#Bind the logit and tidying the data for plot
model_red <- model_red %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(model_red, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
#scatterplots identify linear relationship between continuous predictor variables and the logit of the outcome

#observations 
#1) alcohol, sulphates, volatile.acidity appear to be linearly associated with quality_cat outcome in logit scale
#2)chlorides, density, pH, residual.sugar, fixed acidity, total.sulfur.dioxide, citric.acid do not appear to have an 
#association with quality_cat outcome in logit scale
#(weak predictors?)
#Therefore, no need to transform, move onto next assumption

#best 7-predictor model: alcohol, volatile.acidity, total.sulfur.dioxide, sulphates, chlorides,
#citric.acid, fixed.acidity

#ASSUMPTION 2 -> outliers and inluential variables 
#residuals -> used to help detect outliers 
res<-model1_red$residuals
#studentized residuals 
student.res<-rstandard(model1_red)
#externally studentized residuals
ext.student.res<-rstudent(model1_red)

res.frame<-data.frame(res,student.res,ext.student.res)
res.frame

par(mfrow=c(1,2))
plot(model1_red$fitted.values,student.res,
     main="Studentized Residuals",
     ylim=c(-4.5,4.5))
plot(model1_red$fitted.values,ext.student.res,
     main="Externally Studentized Residuals",
     ylim=c(-4.5,4.5))
#plots look very similar, suggests that there are no outliers 

#leverage
p <- 7 #7 predictor model
n<-dim(train_red)[1]
lev<-lm.influence(model1_red)$hat 
##identify high leverage points 
lev[lev>2*p/n]
#high leverage observations are data points that are likely to be influential 

#influential 
#cooks distance
COOKS<-cooks.distance(model1_red)
COOKS[COOKS>qf(0.5,p,n-p)]
#no influential points based on COOK's distance

##dffits
DFFITS<-dffits(model1_red)
DFFITS[abs(DFFITS)>2*sqrt(p/n)]
#shows influential points, but we are concerned with prediction, rely more heavily on cook's distance

#dfbetas
DFBETAS<-dfbetas(model1_red)
abs(DFBETAS)>2/sqrt(n)
#shows influential points, again more concerned with prediction, rely more on cook's distance 

#ASSUMPTION 3
#mutlicollinearity 
vif(dplyr::select(train_red, c(alcohol,volatile.acidity,sulphates,fixed.acidity,density,residual.sugar,chlorides)))
#VIF score above 10 indicates high correlation, cause for concern, VIF scores less than 10 across the board
#############################################################

#TESTING MODEL 7-PRED MODEL
#############################################################
#Test that 7-predictor model is useful
##test if coefficients for all 7 predictors are 0 
#H0: beta1=beta2=beta3=beta4=beta5=beta6=beta7=0
#HA: at least one of the coefficients in HO is not zero
##test stat
TS<-model1_red$null.deviance-model1_red$deviance
TS
#p-value 
1-pchisq(TS,7)
#p-value is 0
#So we reject the null hypothesis. The 7-predictor model is chosen over the intercept-only model; 
#the 7-predictor model is useful.

#test if 7-predictor model more useful then full (10 predictor model)
#H0: beta8=beta9=beta10=0
#HA: at least one of the coefficients in HO is not zero
full_red<-glm(quality_cat ~ ., family = "binomial", data=train_red)
summary(full_red) #10-predictor model

TS2<-model1_red$deviance-full_red$deviance
TS2  #1.502435
#pvalue
1-pchisq(TS2,3)
#p-value is  0.003918644
#so we reject the null hypothesis, 10-predictor model more useful than 7-predictor model
#CONSTRUCT ROC curve/AUC/confusion matrix for full model -> confusion matrix at 0.5 threshold 
#performs same as 7-predictor model -> adding complexity/real world costs for no benefit in context of question 

#check full model multicollinearity 
vif(dplyr::select(train_red, c(alcohol,volatile.acidity,sulphates,fixed.acidity,density,residual.sugar,chlorides, pH,
                               citric.acid, total.sulfur.dioxide)))

summary(full_red)
#no multicollinearity, pH and citric.acid appear to not be significant predictors (based on 
#wald test scores), can drop, end up with 8-predictor model

#8-predictor model
model2_red <- glm(quality_cat~alcohol+volatile.acidity+sulphates+fixed.acidity+density+residual.sugar+chlorides+
                  total.sulfur.dioxide, family = "binomial", data=train_red)
summary(model2_red)
#wald test, all predictors sig
#####################################################

#ROC/AUC/CONFUSION MATRIX TO COMPARE 7 AND 8-PRED MODELS 
#####################################################
##ROC AND AUC FOR 7-PREDICTOR MODEL##
##predicted survival rate for test data based on training data
preds<-predict(model1_red, newdata=test_red, type="response")
##transform the input data into a format that is suited for the
##performance() function
rates<-prediction(preds, test_red$quality_cat, label.ordering = c("low", "high"))
##store the true positive and false positive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Red Wine Quality Prediction")
lines(x = c(0,1), y = c(0,1), col="red")

auc<-performance(rates, measure = "auc")
auc@y.values #0.8896574, suggests are model classify observations better than random guessing 

table(test_red$quality_cat, preds>0.50)
#####################################################


#####################################################
##ROC AND AUC FOR 8-predictor MODEL##
preds_2<-predict(model2_red, newdata=test_red, type="response")
##transform the input data into a format that is suited for the
##performance() function
rates_2<-prediction(preds_2, test_red$quality_cat, label.ordering = c("low", "high"))
##store the true positive and false positive rates
roc_result_2<-performance(rates_2,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result_2, main="ROC Curve for Red Wine Quality Prediction")
lines(x = c(0,1), y = c(0,1), col="red")

auc_full<-performance(rates_2, measure = "auc")
auc_full@y.values #0.8978096, suggests are model classify observations better than random guessing 

table(test_red$quality_cat, preds_2>0.50)
#####################################################

#goal min ratio of false positives to true positives
#FP/TP ratio better for 7-predictor model than 8-predictor model, conclude that 7-predictor model for red wines

#OVERALL WORK FLOW
#1) start with 7-predictor model produced by all possible regression
#2) check assumptions for 7-predictor model
#3) test 7-predictor model against intercept only and full model
#4) find full model is more useful, run multicollinearity and see that pH/citric.acid not linearly associated 
#with response, drop -> end up with 8-predictor model
#5) generate ROC/AUC and confusion matrices for 7 and 8-predictor model, see that 7-predictor model works better
#in context of our main goal 