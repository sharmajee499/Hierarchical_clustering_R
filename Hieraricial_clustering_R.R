#importing the libraries
library(tidyverse)
library(readxl)

#importing the dataset
uitilities <- read_excel("C:/Users/Sunnation/Desktop/uitilities.xlsx")

#exploring the dataset
str(uitilities)
sum(is.na(uitilities)) #no NA's in data
summary(uitilities)

pairs(uitilities %>% select(-Company)) #viewing the different data points at a glance

#normalization
a<- uitilities[,-c(1,1)]
b<- apply(a,2,mean)
c<- apply(a,2,sd)
a<- scale(a,b,c)

#calculate the Ecuclidean distance
distance<- dist(a)

#clustering dindogram
#the defult clustering is "complete" based on the maximum distance between two data points

h_cluster<- hclust(distance, method="complete")
plot(h_cluster, labels = uitilities$Company) #labelled with the state names

#clustering into different members
member1<- cutree(h_cluster, h=3)

