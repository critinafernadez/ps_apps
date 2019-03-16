library(readr)
library(tidyverse)
library(dplyr)
googleplaystore <- read_csv("google-play-store-apps/googleplaystore.csv")
#user.reviews <- read_csv("google-play-store-apps/googleplaystore_user_reviews.csv")

#First we have a look at the variables 
class(googleplaystore)
colnames(googleplaystore)

head(googleplaystore)

#pasamos campos a factor
sapply(googleplaystore,class)
googleplaystore = googleplaystore %>% mutate(Category = as.factor(Category))%>%
  mutate(Installs = as.factor(Installs)) %>%
  mutate(Type = as.factor(Type))%>%
  mutate(`Content Rating`= as.factor(`Content Rating`))%>%
  mutate(Genres = as.factor(Genres))%>%
  mutate(`Current Ver` = as.factor(`Current Ver`))%>%
  mutate(`Android Ver` = as.factor(`Android Ver`))

#quitamos la cateogria 0 de Installs, no tiene sentido.
table(googleplaystore$Installs)
googleplaystore = googleplaystore %>% filter(Installs != 0)

#NA's
sum(is.na(googleplaystore)==TRUE)
na = googleplaystore %>% filter_all(any_vars(is.na(.))) #1475 NA's
data = data.frame(lapply(googleplaystore,function(x) {
  if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))  %>% drop_na()
#we rememove the rest of missing values with drop_na
sum(is.na(data))
colSums(is.na(data))
summary(data)

#remove category NaN from variable Type
table(data$Type)
data = data[data$Type != "NaN", , drop= TRUE] %>% mutate (Type = factor(Type))

#tranformation of variable size to numeric variable measured in millions
#gsub("M","",data$Size)
sum(data$Size=="Varies with device")
#we want to remove size = "Varies with device because doesn't give us any clear information
data = data %>% filter(Size != "Varies with device") 
data = data[data$Size != "Varies with device", , drop = TRUE] %>% mutate(Size = factor(Size))

#now we take values that end in k and divide them by one million
sizem = data %>% filter(grepl("M",Size)) %>% mutate(Size = gsub("M","",Size)) %>%
  mutate(Size = as.numeric(as.character(Size)))
sizek = data %>% filter(grepl("k",Size)) %>% mutate(Size = gsub("k","", Size)) %>%
  mutate(Size = as.numeric(as.character(Size))) %>% mutate(Size = Size/1000000)

#now we have to concatenate both tables and put the numeric size into a new column
data = rbind(sizem,sizek)

#now we transform the variable price. We remove the $ symbol and mutate to numeric
head(data$Price)

data = data %>% mutate(Price = gsub("\\$","", Price)) %>% 
  mutate(Price = as.numeric(as.character(Price)))
class(data$Price)
