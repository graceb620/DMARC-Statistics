rm(list=ls()) 
#load necessary libraries
library(tidyverse)
library(RColorBrewer)
library(reshape2)

set.seed(13032025)

hh_23<-read.csv('Data/hh_data23.csv',stringsAsFactors=FALSE)

View(hh_23)

#K-means Clustering ---------

#creating a subset with only numerical categories
hh_23X <-subset(hh_23,select=c(n_people_in_household,
                               income_first,income_last,income_2023,
                               household_income_median,income_max,income_min,
                               fed_poverty_level_first,fed_poverty_level_2023,
                               fed_poverty_level_avg,fed_poverty_level_max,
                               fed_poverty_level_min))
summary(hh_23X)

#standardize the units 
hh_23Xstand<-apply(hh_23X,2,function(x){(x-mean(x))/sd(x)}) #2 applies function to column, 1 applies function to rows
#how many standard deviation from the mean is it
summary(hh_23Xstand)

#use an elbow plot to choose the number of clusters
max<-15 #the maximum number of clusters that you think would be useful
#we just want to learn more about our data
wss<-(nrow(hh_23Xstand)-1)*sum(apply(hh_23Xstand,2,var))
for(i in 2:max){
  wss[i]<-sum(kmeans(hh_23Xstand,centers=i)$withinss)
}
#elbow plot
ggplot()+
  geom_line(aes(x=1:max,y=wss))+
  geom_point(aes(x=1:max,y=wss))+
  scale_x_continuous(breaks=1:max)
#where is our marginal gains really dropping off?
#7 seems like a reasonable cutoff point
#let's fit our k-means with k=7 clusters

hh_23X_kmeans<-kmeans(hh_23Xstand,centers=4)

#show how distributions of each of the features vary across clusters
#first, add a column of clusters onto sb_X

hh_23X$km_clusters<-as.factor(hh_23X_kmeans$cluster)

#transposing - using the reshape2 package
hh_23X_long<-melt(hh_23X,id.vars=c("km_clusters"))

ggplot(data=hh_23X_long)+
  geom_boxplot(aes(x=km_clusters,y=value,fill=km_clusters))+
  facet_wrap(~variable,scales="free")+
  scale_fill_brewer(palette="Dark2")
#change y and x axis if you would be presenting it

hh_23$km_cluster<-as.factor(hh_23X_kmeans$cluster)

#Cluster 3: Group with lowest federal poverty level in 2023
summary(filter(hh_23,km_cluster=="1")) 
#35% of this group had their first visit in 2023

#Cluster 4: Group with highest federal poverty level in 2023
summary(filter(hh_23,km_cluster=="4")) 
#50% of this group had their first visit in 2023

#Cluster 1: Group with highest #of people in household
summary(filter(hh_23,km_cluster=="3")) 
#10% of this group had their first visit in 2023
