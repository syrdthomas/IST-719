################################################################################
#
# Author: DeAndre Thomas
# Purpose: IST 719 Poster
#
################################################################################
#
# Questions
#
# Question 1: Which variant has been recorded most from 2021-2022?
# Question 2: Were certain variants more prominent during certain seasons of 
#                 the year?
# Question 3: How does the "other" variant affects the state of California in 
#                 comparison to the others? Could this "other" variant in fact 
#                 a false diagnosis of the flu or cold?
#
################################################################################

# Load Libraries

library(RColorBrewer)
library(ggplot2)
library(dplyr)
install.packages("janitor")
library(janitor)
library(data.table)
library(lubridate)

################################################################################

# Load dataframe 
df<- read.csv("C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\covid_data.csv")
# View(df)

################################################################################

# Explore Dataset

dim(df)
colnames(df)
str(df)
summary(df)
colnames(df)
################################################################################

# Clean Data

## Finding NaN values in df
is.na(df)
### specimens_7d_avg and percentage_7d_avg were columns with NaN values

## Replacing NaN values with mean values
df$specimens_7d_avg[is.na(df$specimens_7d_avg)]<- mean(df$specimens_7d_avg
                                                       , na.rm = TRUE)
df$percentage_7d_avg[is.na(df$percentage_7d_avg)]<- mean(df$percentage_7d_avg
                                                       , na.rm = TRUE)
sum(is.na(df))
# 0 NaN values
# View(df)

## Removing rows with "total" value in the variant_name column

df1<- df[!(df$variant_name == "Total"), ]
# unique(df1$variant_name)
# View(df1)

## Removing area_type column from df1
df1<- subset(df1, select = -area_type)
# colnames(df1)
################################################################################

# Create Subsets of df1 per month

unique(df1$date)
jan<- df1[df1$date>= "2021-01-01" & df1$date<= "2021-01-31", ]
feb<- df1[df1$date>= "2021-02-01" & df1$date<= "2021-02-28", ]
march<- df1[df1$date>= "2021-03-01" & df1$date<= "2021-03-31", ]
april<- df1[df1$date>= "2021-04-01" & df1$date<= "2021-04-30", ]
may<- df1[df1$date>= "2021-05-01" & df1$date<= "2021-05-31", ]
jun<- df1[df1$date>= "2021-06-01" & df1$date<= "2021-06-30", ]
july<- df1[df1$date>= "2021-07-01" & df1$date<= "2021-07-31", ]
aug<- df1[df1$date>= "2021-08-01" & df1$date<= "2021-08-31", ]
sep<- df1[df1$date>= "2021-09-01" & df1$date<= "2021-09-30", ]
oct<- df1[df1$date>= "2021-10-01" & df1$date<= "2021-10-31", ]
nov<- df1[df1$date>= "2021-11-01" & df1$date<= "2021-11-30", ]
decem<- df1[df1$date>= "2021-12-01" & df1$date<= "2021-12-31", ]
jan2<- df1[df1$date>= "2022-01-01" & df1$date<= "2022-01-31", ]
# feb2<- df1[df1$date>= "2022-02-01" & df1$date<= "2022-02-05", ]
# View(df1)

## Removing feb2 subset and deleting feb2 rows from df1, because there are only 45 
#   data points in this section. 

# df2<- df1[-(3961:4010), ]
# df2<- subset(df1, date >= "2022-02-01" & date<= "2022-02-05")
# df2<- df1[df1$date>= "2022-02-01" & df1$date<= "2022-02-05", ]
# df2<- df1[-c(3961:4010), ]
# df2<- df1[-c(40:nrow(df1)), ]
## None of the above lines of code worked for removing the dates
# rm(df2)

df2<- slice(df1, 1:(n() - 45))
# WINNER!
View(df2)
################################################################################

# Question 1: Which variant has been recorded most from 2021-2022?

unique(df2$variant_name)
# "Beta" "Lambda" Gamma" "Mu" "Epsilon" "Delta" "Omicron" "Other" "Alpha"
sum(df2$specimens)
# 289695

vcount<- df2

# aggregate df so that unique variant_names have sum values of specimens
vsum<- aggregate(specimens ~ variant_name, data = vcount, sum)
vsum
vframe<- data.frame(vsum)
vf<- vframe

# barplot of variant frame
ggplot(vf) + aes(x = variant_name, y = specimens, fill = variant_name) + 
  geom_bar(stat = "identity") + 
  ggtitle("Total Number of Specimens per Variant") +
  scale_fill_manual(values = c("#800000FF", "#D6D6CEFF", "#FFB547FF"
                               , "#ADB17DFF", "#5B8FA8FF", "#D49464FF"
                               , "#B1746FFF", "#8A8B79FF", "#725663FF"))

# boxplot (will not use)
ggplot(vcount, aes(x = variant_name, y = specimens)) + geom_boxplot() +
  scale_fill_manual(values = c("#800000FF", "#D6D6CEFF", "#FFB547FF"
                               , "#ADB17DFF", "#5B8FA8FF", "#D49464FF"
                               , "#B1746FFF", "#8A8B79FF", "#725663FF"))

# scatter plot (will not use)
ggplot(vcount) + aes(x = variant_name, y = specimens, col = variant_name) + 
  geom_point() 

# Delta
################################################################################

# Question 2: Were certain variants more prominent during certain seasons of 
#                 the year?

# Creating dataframes of seasons
spring<- df1[df1$date>= "2021-03-01" & df1$date<= "2021-05-31", ]
summer<- df1[df1$date>= "2021-06-01" & df1$date<= "2021-08-31", ]
fall<- df1[df1$date>= "2021-09-01" & df1$date<= "2021-11-31", ]
winter1<- df1[df1$date>= "2021-01-01" & df1$date<= "2021-02-28", ]
winter2<- df1[df1$date>= "2021-12-01" & df1$date<= "2022-01-31", ]

spsum<- aggregate(specimens ~ variant_name, data = spring, sum)
# spsum
spf<- data.frame(spsum)

susum<- aggregate(specimens ~ variant_name, data = summer, sum)
# susum
suf<- data.frame(susum)

fsum<- aggregate(specimens ~ variant_name, data = fall, sum)
# fsum
ff<- data.frame(fsum)

wsum1<- aggregate(specimens ~ variant_name, data = winter1, sum)
# wsum1
w1f<- data.frame(wsum1)

wsum2<- aggregate(specimens ~ variant_name, data = winter2, sum)
# wsum2
w2f<- data.frame(wsum2)

# Creating the pallette

vpal<- c("#800000FF", "#D6D6CEFF", "#FFB547FF"
         , "#ADB17DFF", "#5B8FA8FF", "#D49464FF"
         , "#B1746FFF", "#8A8B79FF", "#725663FF")

spmid<- mean(spf$specimens)
ggplot(spring) +
  aes(x = variant_name, y = percentage, color = specimens) +
  geom_point() + ggtitle("Specimen per Variant in Spring of 2021") +

  
    
# values = c("#800000FF", "#D6D6CEFF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#D49464FF, "#B1746FFF", "#8A8B79FF", "#725663FF"))

ggplot(summer) +
  aes(x = variant_name, y = percentage, color = specimens) +
  geom_point() + ggtitle("Specimen per Variant in Summer of 2021")

ggplot(fall) +
  aes(x = variant_name, y = percentage, color = specimens) +
  geom_point() + ggtitle("Specimen per Variant in Fall of 2021")
  
ggplot(winter1) +
  aes(x = variant_name, y = percentage, color = specimens) +
  geom_point() + ggtitle("Specimen per Variant in Winter 2021")

ggplot(winter2) +
  aes(x = variant_name, y = percentage, color = specimens) +
  geom_histogram() + ggtitle("Specimen per Variant in Winter of 2021-2022")

# Winter1: Epsilon
# Spring: Alpha
# Summer: Delta
# Fall: Delta
# Winter2: Omicron
################################################################################

# Question 3: How does the "other" variant affects the state of California in 
#                 comparison to the others? Could this "other" variant in fact 
#                 a false diagnosis of the flu or cold?
# Will not be using this question. 
# vframe


View(vcount)
o<- aggregate(cbind(specimens, percentage) ~ variant_name
          , data = vcount, FUN = mean, na.rm = TRUE)
of<- data.frame(o)
View(of)

o2<- aggregate(cbind(specimens, percentage) ~ variant_name
              , data = vcount, FUN = sum, na.rm = TRUE)
o2f<- data.frame(o2)
View(o2f)

seasons = function(vcount) {
  if(vcount %in%  1:590) return("Winter1")
  if(vcount %in% 591:1510) return("Spring")
  if(vcount %in% 1511:2429) return("Summer")
  if(vcount %in% 2430:3340) return("Fall")
  if(vcount %in% 3341:3960) return("Winter2")
}
 
vcount$season <- sapply(month(vcount$date), seasons)
vcount<- data.frame(vcount)

View(vcount)
################################################################################

# Create map of California

library(tidyverse)
library(ggmap)
install.packages("maps", "mapdata")

states<- map_data("state")
dim(states)
head(states)
p<- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat
                   , fill = region, group = group)
               , color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)
p 

california<- subset(states, region == "california")
ca<- ggplot(data = california) + 
  geom_polygon(aes(x = long, y = lat), fill = "white", color = "black")
ca

ca_base <- ggplot(data = california, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "white")
ca_base + theme_nothing()
# map of california 


################################################################################
ggplot(o2f, aes(x="", y = specimens, fill = variant_name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#800000FF", "#D6D6CEFF", "#FFB547FF"
                               , "#ADB17DFF", "#5B8FA8FF", "#D49464FF"
                               , "#B1746FFF", "#8A8B79FF", "#725663FF")) +
  theme_void()


ggplot(, aes(fill = ))


























