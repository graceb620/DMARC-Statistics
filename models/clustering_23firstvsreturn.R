rm(list=ls()) 
#load necessary libraries
library(tidyverse)
library(RColorBrewer)
library(reshape2)
library(dplyr)

set.seed(434367540)

hh_23<-read.csv('Data/hh_data23.csv',stringsAsFactors=FALSE)
hh_first23<-read.csv('Data/hh_first23.csv',stringsAsFactors=FALSE)
hh_return23<-hh_23%>%filter(year(first_visit) != 2023)

#K-means Clustering 
## ONLY 2023 returners --------------
View(hh_return23)

#K-means Clustering

#creating a subset with only numerical categories
hh_return23X <-subset(hh_return23,select=c(n_people_in_household,
                                         income_first,income_last,income_2023,
                                         household_income_median,income_max,income_min,
                                         fed_poverty_level_first,fed_poverty_level_2023,
                                         fed_poverty_level_avg,fed_poverty_level_max,
                                         fed_poverty_level_min))
summary(hh_return23X)

#standardize the units 
hh_return23Xstand<-apply(hh_return23X,2,function(x){(x-mean(x))/sd(x)}) #2 applies function to column, 1 applies function to rows
#how many standard deviation from the mean is it
summary(hh_return23Xstand)

#use an elbow plot to choose the number of clusters
max<-15 #the maximum number of clusters that you think would be useful
#we just want to learn more about our data
wss<-(nrow(hh_return23Xstand)-1)*sum(apply(hh_return23Xstand,2,var))
for(i in 2:max){
  wss[i]<-sum(kmeans(hh_return23Xstand,centers=i)$withinss)
}
#elbow plot
ggplot()+
  geom_line(aes(x=1:max,y=wss))+
  geom_point(aes(x=1:max,y=wss))+
  scale_x_continuous(breaks=1:max)
#where is our marginal gains really dropping off?
#7 seems like a reasonable cutoff point
#let's fit our k-means with k=7 clusters

hh_return23X_kmeans<-kmeans(hh_return23Xstand,centers=3)

#show how distributions of each of the features vary across clusters
#first, add a column of clusters onto sb_X

hh_return23X$km_clusters<-as.factor(hh_return23X_kmeans$cluster)

#transposing - using the reshape2 package
hh_return23X_long<-melt(hh_return23X,id.vars=c("km_clusters"))

ggplot(data=hh_return23X_long)+
  geom_boxplot(aes(x=km_clusters,y=value,fill=km_clusters))+
  facet_wrap(~variable,scales="free")+
  scale_fill_brewer(palette="Dark2") + labs(title="Clusters for 2023 First Visitors")
#change y and x axis if you would be presenting it

hh_return23$km_cluster<-as.factor(hh_return23X_kmeans$cluster)

#Cluster 1: Group with highest federal poverty level in 2023
summary(filter(hh_return23,km_cluster=="3")) 
highest_povertylevelreturn<-filter(hh_return23,km_cluster=="3")
median(highest_povertylevelreturn$income_2023) #40000 median
length(highest_povertylevelreturn$afn)/length(hh_return23$afn) #9.4% of returners 

#Cluster 2: Group with lowest federal poverty level in 2023
summary(filter(hh_return23,km_cluster=="2")) 
lowest_povertylevelreturn<-filter(hh_return23,km_cluster=="2")
median(lowest_povertylevelreturn$income_2023) #0 median
length(lowest_povertylevelreturn$afn)/length(hh_return23$afn) #46.5% of returners


## ONLY 2023 first-visitors --------------

hh_first23<-read.csv('Data/hh_first23.csv',stringsAsFactors=FALSE)

View(hh_first23)

#K-means Clustering

#creating a subset with only numerical categories
hh_first23X <-subset(hh_first23,select=c(n_people_in_household,
                               income_first,income_last,income_2023,
                               household_income_median,income_max,income_min,
                               fed_poverty_level_first,fed_poverty_level_2023,
                               fed_poverty_level_avg,fed_poverty_level_max,
                               fed_poverty_level_min))
summary(hh_first23X)

#standardize the units 
hh_first23Xstand<-apply(hh_first23X,2,function(x){(x-mean(x))/sd(x)}) #2 applies function to column, 1 applies function to rows
#how many standard deviation from the mean is it
summary(hh_first23Xstand)

#use an elbow plot to choose the number of clusters
max<-15 #the maximum number of clusters that you think would be useful
#we just want to learn more about our data
wss<-(nrow(hh_first23Xstand)-1)*sum(apply(hh_first23Xstand,2,var))
for(i in 2:max){
  wss[i]<-sum(kmeans(hh_first23Xstand,centers=i)$withinss)
}
#elbow plot
ggplot()+
  geom_line(aes(x=1:max,y=wss))+
  geom_point(aes(x=1:max,y=wss))+
  scale_x_continuous(breaks=1:max)
#where is our marginal gains really dropping off?
#7 seems like a reasonable cutoff point
#let's fit our k-means with k=7 clusters

hh_first23X_kmeans<-kmeans(hh_first23Xstand,centers=3)

#show how distributions of each of the features vary across clusters
#first, add a column of clusters onto sb_X

hh_first23X$km_clusters<-as.factor(hh_first23X_kmeans$cluster)

#transposing - using the reshape2 package
hh_first23X_long<-melt(hh_first23X,id.vars=c("km_clusters"))

ggplot(data=hh_first23X_long)+
  geom_boxplot(aes(x=km_clusters,y=value,fill=km_clusters))+
  facet_wrap(~variable,scales="free")+
  scale_fill_brewer(palette="Dark2") + labs(title="Clusters for 2023 First Visitors")
#change y and x axis if you would be presenting it

hh_first23$km_cluster<-as.factor(hh_first23X_kmeans$cluster)

#Cluster 3: Group with highest federal poverty level in 2023
summary(filter(hh_first23,km_cluster=="3")) 
highest_povertylevelfirst<-filter(hh_first23,km_cluster=="3")
median(highest_povertylevelfirst$income_2023) #59,000 income
length(highest_povertylevelfirst$afn)/length(hh_first23$afn) #5.9% of first timers

#Cluster 2: Group with lowest federal poverty level in 2023
summary(filter(hh_first23,km_cluster=="2")) 
lowest_povertylevelfirst<-filter(hh_first23,km_cluster=="2")
median(lowest_povertylevelfirst$income_2023) #0
length(lowest_povertylevelfirst$afn)/length(hh_first23$afn) #60% of first timers

