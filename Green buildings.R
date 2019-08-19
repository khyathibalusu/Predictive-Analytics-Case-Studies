rm(list=ls())
library(MASS)
library(mosaic)
library(tidyverse)

green_build = read.csv('C:/Users/aadia/Downloads/STA380-master/STA380-master/data/greenbuildings.csv')
head(green_build)
attach(green_build)

# The green_build data had outlliers with greater than 100$ rents. 
# Removed them to see the plot more clearly
gb=green_build[green_build[, "Rent"] < 100,]
gb

# Data for one cluster - 16 (One of the largest clusters)
lb=green_build[green_build$Rent <100 & green_build$cluster == 16,]
lb

# Data for one cluster - 20
sb=green_build[green_build$Rent <100 & green_build$cluster == 20,]
sb


Rent
-------------------------------------------------------------------------------
#Age
#Plot of age vs rent for green/non-green
ggplot(data = gb) + 
  geom_point(mapping = aes(x = age, y = Rent, color = as.factor(green_rating)))+
  scale_color_manual(name = "green_rating",
                     values = c("0" = "grey",
                                "1" = "green"),
                     labels = c("Non-green", "Green"))+
  labs(title = "New buildings are more green, but do not seem 
                to make higher rents")

#In order to see the clear difference, plotted it for one of largest clusters (16)
ggplot(data = lb) + 
  geom_point(mapping = aes(x = age, y = Rent, color = as.factor(green_rating)))+
  scale_color_manual(name = "green_rating",
                      values = c("0" = "grey",
                                 "1" = "green"),
                      labels = c("Non-green", "Green"))+
  labs(title = "Clearly, there are older non-green buildings that make
                higher rents than the green building")

#Cluster 20
ggplot(data = sb) + 
  geom_point(mapping = aes(x = age, y = Rent, color = as.factor(green_rating)))+
  scale_color_manual(name = "green_rating",
                     values = c("0" = "grey",
                                "1" = "green"),
                     labels = c("Non-green", "Green"))+
  labs(title = "And the story repeats..
      Older/Non-green buildings make more")

----------------------------------------------------------------------------
#Amenities
ggplot(data = gb) + 
  geom_point(mapping = aes(x = jitter(amenities), y = Rent, color = as.factor(green_rating)))+
  scale_color_manual(name = "green_rating",
                     values = c("0" = "grey",
                                "1" = "green"),
                     labels = c("Non-green", "Green"))+
  labs(title = "Seems like more green buildings have amenities,but we cannot
       clearly see if the rents of green buildings are higher")

ggplot(data = lb) + 
  geom_point(mapping = aes(x = amenities, y = Rent, color = as.factor(green_rating)))+
  scale_color_manual(name = "green_rating",
                     values = c("0" = "grey",
                                "1" = "green"),
                     labels = c("Non-green", "Green"))+
  labs(title = "A little clear...Green building has amenities, but not really
       a higher rent than non-green")


ggplot(data = sb) + 
  geom_point(mapping = aes(x = amenities, y = Rent, color = as.factor(green_rating)))+
  scale_color_manual(name = "green_rating",
                     values = c("0" = "grey",
                                "1" = "green"),
                     labels = c("Non-green", "Green"))+
  labs(title = "Similar observation..Non-green makes higher rent")

# We clearly see that there is no positive effect for green buildings on Rent when
#the variable of Amenities is considered
----------------------------------------------------------------------------
#What happens when both amenities and age of the building comes into play

ggplot(data = gb) + 
  geom_point(mapping = aes(x = age, y = Rent, color = as.factor(green_rating)))+
  scale_color_manual(name = "green_rating",
                     values = c("0" = "grey",
                                "1" = "green"),
                     labels = c("Non-green", "Green"))+
  facet_grid(~ amenities)

#While it looks like the buildings of the same age with amenities would probably have higher rents
#than otherwise, we still do not see that green buildings necessarily
#make more when compared to non-green buildings
-----------------------------------------------------------------------------  

# Linear Regression - Just did this to find the significant variables

fit=lm(Rent~.,data=green_build)
summary(fit)
