rm(list=ls()) 
#load necessary libraries
library(tidyverse)
library(RColorBrewer)
library(reshape2)
library(dplyr)

set.seed(434367540)

hh_23<-read.csv('Data/hh_data23.csv',stringsAsFactors=FALSE)

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

#Cluster 1: Group with lowest federal poverty level in 2023
summary(filter(hh_23,km_cluster=="1")) 
#35% of this group had their first visit in 2023

#Cluster 4: Group with highest federal poverty level in 2023
summary(filter(hh_23,km_cluster=="4")) 
#50% of this group had their first visit in 2023

#Cluster 3: Group with highest #of people in household
summary(filter(hh_23,km_cluster=="3")) 
#10% of this group had their first visit in 2023


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

#Cluster 1: Group with highest federal poverty level in 2023
summary(filter(hh_first23,km_cluster=="1")) 
highest_poverty23<-filter(hh_first22,km_cluster=="1")

#Cluster 2: Group with lowest federal poverty level in 2023
summary(filter(hh_first23,km_cluster=="2")) 
lowest_poverty23<-filter(hh_first22,km_cluster=="2")

# ONLY 2022 first visitors --------

hh_first22<-read.csv('Data/hh_first22.csv',stringsAsFactors=FALSE)

View(hh_first22)

#K-means Clustering 

#creating a subset with only numerical categories
hh_first22X <-subset(hh_first22,select=c(n_people_in_household,
                                         income_first,income_last,income_2022,
                                         household_income_median,income_max,income_min,
                                         fed_poverty_level_first,fed_poverty_level_2022,
                                         fed_poverty_level_avg,fed_poverty_level_max,
                                         fed_poverty_level_min))
summary(hh_first22X)

#standardize the units 
hh_first22Xstand<-apply(hh_first22X,2,function(x){(x-mean(x))/sd(x)}) #2 applies function to column, 1 applies function to rows
#how many standard deviation from the mean is it
summary(hh_first22Xstand)

#use an elbow plot to choose the number of clusters
max<-15 #the maximum number of clusters that you think would be useful
#we just want to learn more about our data
wss<-(nrow(hh_first22Xstand)-1)*sum(apply(hh_first22Xstand,2,var))
for(i in 2:max){
  wss[i]<-sum(kmeans(hh_first22Xstand,centers=i)$withinss)
}
#elbow plot
ggplot()+
  geom_line(aes(x=1:max,y=wss))+
  geom_point(aes(x=1:max,y=wss))+
  scale_x_continuous(breaks=1:max)
#where is our marginal gains really dropping off?
#7 seems like a reasonable cutoff point
#let's fit our k-means with k=7 clusters

hh_first22X_kmeans<-kmeans(hh_first22Xstand,centers=3)

#show how distributions of each of the features vary across clusters
#first, add a column of clusters onto sb_X

hh_first22X$km_clusters<-as.factor(hh_first22X_kmeans$cluster)

#transposing - using the reshape2 package
hh_first22X_long<-melt(hh_first22X,id.vars=c("km_clusters"))

ggplot(data=hh_first22X_long)+
  geom_boxplot(aes(x=km_clusters,y=value,fill=km_clusters))+
  facet_wrap(~variable,scales="free")+
  scale_fill_brewer(palette="Dark2") + labs(title="Clusters for 2022 First Visitors")
#change y and x axis if you would be presenting it

hh_first22$km_cluster<-as.factor(hh_first22X_kmeans$cluster)

#Cluster 2: Group with highest federal poverty level in 2022
summary(filter(hh_first22,km_cluster=="2")) 
highest_poverty22<-filter(hh_first22,km_cluster=="2")

#Cluster 3: Group with lowest federal poverty level in 2022
summary(filter(hh_first22,km_cluster=="3")) 
lowest_poverty22<-filter(hh_first22,km_cluster=="3")

# ONLY 2021 first visitors ------

hh_first21<-read.csv('Data/hh_first21.csv',stringsAsFactors=FALSE)

View(hh_first21)

#K-means Clustering 

#creating a subset with only numerical categories
hh_first21X <-subset(hh_first21,select=c(n_people_in_household,
                                         income_first,income_last,income_2021,
                                         household_income_median,income_max,income_min,
                                         fed_poverty_level_first,fed_poverty_level_2021,
                                         fed_poverty_level_avg,fed_poverty_level_max,
                                         fed_poverty_level_min))
summary(hh_first21X)

#standardize the units 
hh_first21Xstand<-apply(hh_first21X,2,function(x){(x-mean(x))/sd(x)}) #2 applies function to column, 1 applies function to rows
#how many standard deviation from the mean is it
summary(hh_first21Xstand)

#use an elbow plot to choose the number of clusters
max<-15 #the maximum number of clusters that you think would be useful
#we just want to learn more about our data
wss<-(nrow(hh_first21Xstand)-1)*sum(apply(hh_first21Xstand,2,var))
for(i in 2:max){
  wss[i]<-sum(kmeans(hh_first21Xstand,centers=i)$withinss)
}
#elbow plot
ggplot()+
  geom_line(aes(x=1:max,y=wss))+
  geom_point(aes(x=1:max,y=wss))+
  scale_x_continuous(breaks=1:max)
#where is our marginal gains really dropping off?
#7 seems like a reasonable cutoff point
#let's fit our k-means with k=7 clusters

hh_first21X_kmeans<-kmeans(hh_first21Xstand,centers=3)

#show how distributions of each of the features vary across clusters
#first, add a column of clusters onto sb_X

hh_first21X$km_clusters<-as.factor(hh_first21X_kmeans$cluster)

#transposing - using the reshape2 package
hh_first21X_long<-melt(hh_first21X,id.vars=c("km_clusters"))

ggplot(data=hh_first21X_long)+
  geom_boxplot(aes(x=km_clusters,y=value,fill=km_clusters))+
  facet_wrap(~variable,scales="free")+
  scale_fill_brewer(palette="Dark2") + labs(title="Clusters for 2021 First Visitors")
#change y and x axis if you would be presenting it

hh_first21$km_cluster<-as.factor(hh_first21X_kmeans$cluster)

#Cluster 2: Group with highest federal poverty level in 2021
summary(filter(hh_first21,km_cluster=="1"))
highest_poverty21<-filter(hh_first21,km_cluster=="1")

#Cluster 3: Group with lowest federal poverty level in 2021
summary(filter(hh_first21,km_cluster=="3")) 
lowest_poverty21<-filter(hh_first21,km_cluster=="1")


# Visualizations - comparisons ------

hh_data <- read.csv('Data/hh_data.csv',stringsAsFactors=FALSE)

hh_data1 <- hh_data %>% mutate(first_year=year(first_visit)) %>% filter(first_year > 2020 & first_year<2024) %>%
  filter(household_income_median < 200000&household_income_median != 0)

hh_data2 <- hh_data %>% mutate(first_year=year(first_visit)) %>% filter(first_year > 2020 & first_year<2024) %>%
  filter(fed_poverty_level_first_visit < 200000&household_income_median != 0)

# median income of households
ggplot(hh_data1, aes(x=first_year, y = household_income_median, group=first_year)) + 
  geom_boxplot() + xlab("First Visit") + ylab("Median Household Income") +
  labs(title="Comparison of median household income of visitors in 2023 below $200,000 and above $0",
       subtitle="between households of different first visit year")
#There is a slight change in median income between first visit years, but 
#it does not seem significant

#federal poverty level
ggplot(hh_data1, aes(x=first_year, y = fed_poverty_level_first_visit, group=first_year)) + 
  geom_boxplot() + xlab("First Visit") + ylab("Median federal poverty level at first visit") +
  labs(title="Comparison of federal poverty level",
       subtitle="between households of different first visit year")
#Median federal poverty level increased in 2022 and then decreased in 2023

#hh_size
ggplot(hh_data1, aes(x=first_year, y = n_people_in_household, group=first_year)) + 
  geom_boxplot() + xlab("First Visit") + ylab("Number of people in household") +
  labs(title="Comparison of federal poverty level",
       subtitle="between households of different first visit year")
#Very minimal change


#highest poverty dataset
merged_df <- merge(highest_poverty21,highest_poverty22,highest_poverty23,by = "afn", all = TRUE)

merged_df <- merge(highest_poverty21,highest_poverty22,highest_poverty23,by = "afn", all = TRUE)

#Previous work:

# # There are a few outliers with median income above 200000 - remove them
# hh_23out <- hh_data_2023 %>% 
#   filter(household_income_median < 200000) %>% 
#   mutate(first_visit_2023= ifelse(first_visit_2023==1, "first visit in 2023", "first visit before 2023"))
# # redoing the box plot
# ggplot(hh_23out, aes(x = first_visit_2023, y = household_income_median, group=first_visit_2023)) + 
#   geom_boxplot() + xlab("First Visit Year") + ylab("Median Household Income") +
#   labs(title="Comparison of median household income of visitors in 2023 below $200,000",
#        subtitle="between households who first visited in 2023 and who first visited earlier")
# 
# # let's remove the households that do not have any income
# hh_23out <- hh_data_2023 %>% 
#   filter(household_income_median < 200000&household_income_median != 0) %>% 
#   mutate(first_visit_2023= ifelse(first_visit_2023==1, "first visit in 2023", "first visit before 2023"))
# 
# hh_22out <- hh_first22 %>% 
#   filter(household_income_median < 200000&household_income_median != 0) %>% 
# 
# 
# ggplot(hh_23out, aes(x = first_visit_2023, y = household_income_median, group=first_visit_2023)) + 
#   geom_boxplot() + xlab("First Visit") + ylab("Median Household Income") +
#   labs(title="Comparison of median household income of visitors in 2023 above $0 and below $200,000",
#        subtitle="between households who first visited in 2023 and who first visited earlier")
# 
# 
# 
