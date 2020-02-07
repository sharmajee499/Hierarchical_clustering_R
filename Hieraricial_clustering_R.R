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
#clustered group. We can cluster using different method.

#**************************************************************************************************************************
#**************************************************************************************************************************
#**************************************************************************************************************************


#k-means clustering using USArrest dataset

#importing the libraries and dataset
install.packages("factoextra")
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
df<- USArrests
sum(is.na(df)) #no NAs

#scaling/standardizing/ normalizing the data
df<- scale(df)

# computing the k-means clustering
k_clust<- kmeans(df,centers = 2, nstart = 25)
k_clust
fviz_cluster(k_clust,df) #visualizing the clusters

#determining optimal cluster
#1. Elbow method
fviz_nbclust(df,kmeans, method="wss")

#2.Average Silhouette Method
fviz_nbclust(df,kmeans, method="silhouette")

#3.Gap Statistic Method
set.seed(123)
gap_stat<- clusGap(df,FUN=kmeans, nstart=25, K.max = 10, B=50)
fviz_gap_stat(gap_stat)
gap_stat

#again computing k means with 4 clusters
k_clust_4<- kmeans(df, 4, nstart=25)
k_clust_4
fviz_cluster(k_clust_4, df)

#mutating the cluster column to the original dataset
USArrests<- USArrests %>% mutate(cluster=k_clust_4$cluster)

#summerizing the cluster conclusion
USArrests%>% group_by(cluster) %>% summarize_all("mean")
# we can compare the differnt means of the cluster and overall analyze the data from cluster. 