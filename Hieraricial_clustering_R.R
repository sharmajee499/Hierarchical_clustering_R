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
member1<- cutree(h_cluster, h=6) #making two clusters
data.mem.1<-uitilities%>% mutate(cluster=member1)
summ_mem_1<- data.mem.1 %>% group_by(cluster) %>% summarise(total=n(), median_ROR=median(RoR),median_cost=median(Cost),
                                               median_sales=median(Sales))
                                                
#conclusion: We clustered the data into 2 groups. After that, we compared the median of the different-
#clustered group. 

