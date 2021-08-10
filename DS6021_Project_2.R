library(ggplot2)
library(tidyverse)
library(gridExtra)

white_wine <- read.csv('wineQualityWhites.csv')
red_wine <- read.csv('wineQualityReds.csv')

red_wine <- red_wine %>%
  mutate(color = 'red')
white_wine <- white_wine %>%
  mutate(color = 'white')

wine_combined <- rbind(white_wine, red_wine) #combine datasets 

wine_combined <- wine_combined %>%
  mutate(quality_cat = ifelse(quality>=7, "high", "low"))

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
sample_combined<-sample.int(nrow(wine_combined), floor(.80*nrow(wine_combined)), replace = F)
train_combined<-wine_combined[sample_combined, ] #training data 
test_combined<-wine_combined[-sample_combined, ] 


#correlation matrix
res <- cor(wine_combined)
round(res, 2)

library(ggplot2)
# Building histogram for distribution of quality 
ggplot(data=wine_combined, aes(wine_combined$quality)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density() 
#ordinal data, want to categorize wine as low or high quality 



