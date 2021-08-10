library(ggplot2)
library(tidyverse)
library(gridExtra)

white_wine <- read.csv('wineQualityWhites.csv')
red_wine <- read.csv('wineQualityReds.csv')

white_wine <- dplyr::select(white_wine, -free.sulfur.dioxide)
red_wine <- dplyr::select(red_wine, -free.sulfur.dioxide)

red_wine <- red_wine %>%
  mutate(color = 0) #red is 0
white_wine <- white_wine %>%
  mutate(color = 1) #white is 1

#add quality_cat
red_wine <- red_wine %>%
  mutate(quality_cat = ifelse(quality>=7, 1, 0)) #high quality 1, low quality 0
white_wine <- white_wine %>%
  mutate(quality_cat = ifelse(quality>=7, 1, 0)) #high quality 1, low quality 0

#change color to factor
red_wine$color<-factor(red_wine$color)
levels(red_wine$color) <- c("red","white") 
white_wine$color<-factor(white_wine$color)
levels(white_wine$color) <- c("red","white") 

#change quality_cat to factor 
red_wine$quality_cat<-factor(red_wine$quality_cat)
levels(red_wine$quality_cat) <- c("low","high") 
white_wine$quality_cat<-factor(white_wine$quality_cat)
levels(white_wine$quality_cat) <- c("low","high") 

wine_combined <- rbind(white_wine, red_wine) #combine datasets 

#split into train and test data
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


