######################################################################
#
#Author: DeAndre Thomas
#Purpose: Week 3 Homework
#
######################################################################

df<- read.csv("C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\covid_data.csv"
              , header = TRUE
              , stringsAsFactors = FALSE)
str(df)
# This dataset has 8 columns and 4010 rows
# (NumberOfColumns*4)*(NumberOfRows/100)>=100
(8*4)*(4010/100)
# 1283.2 > 100

df.2<- df #creating a new df that can be edited
df.2<- df.2[!df.2$variant_name == "Total",] #removing rows with "Total" in Variant Name column
View(df.2)

df.2$specimens_7d_avg[is.na(df.2$specimens_7d_avg)]<- mean(df.2$specimens_7d_avg, na.rm = TRUE)
# replacing NA values of "speciments_7d_avg" column with average of that column

df.2$percentage_7d_avg[is.na(df.2$percentage_7d_avg)]<- mean(df.2$percentage_7d_avg, na.rm = TRUE)
# replacing NA values of "percentage_7d_avg" column with average of that column

#Categorical Columns == date, area, variant_name
#Continuous Columns == specimens, percentage, speciments_7d_avg,
# percentage_7d_avg

df.2<- subset(df.2, select = -c(area, area_type))
# removing Area (= California) and and Area Type (= State) Columns

sum(is.na(df.2))
#no NA's in dataframe

str(df.2)
# This dataset has 6 columns and 3609 rows
# (NumberOfColumns*4)*(NumberOfRows/100)>=100
(6*4)*(3609/100)
# 866.16 > 100

#################################################################
# Creating subset of data per month

jan<- df.2[df.2$date >= "2021-01-01" & df.2$date <= "2021-01-31", ]
range(jan$date)

feb<- df.2[df.2$date >= "2021-02-01" & df.2$date <= "2021-02-28", ]
range(feb)

march<- df.2[df.2$date >= "2021-03-01" & df.2$date <= "2021-03-31", ]
range(march)

apr<- df.2[df.2$date >= "2021-04-01" & df.2$date <= "2021-04-30", ]
range(apr)

may<- df.2[df.2$date >= "2021-05-01" & df.2$date <= "2021-05-31", ]
range(may)

jun<- df.2[df.2$date >= "2021-06-01" & df.2$date <= "2021-06-30", ]
range(jun)

jul<- df.2[df.2$date >= "2021-07-01" & df.2$date <= "2021-07-31", ]
range(jul)

aug<- df.2[df.2$date >= "2021-08-01" & df.2$date <= "2021-08-31", ]
range(aug)
  
sept<- df.2[df.2$date >= "2021-09-01" & df.2$date <= "2021-09-30", ]
range(feb)
  
oct<- df.2[df.2$date >= "2021-10-01" & df.2$date <= "2021-10-31", ]
range(oct) 

nov<- df.2[df.2$date >= "2021-11-01" & df.2$date <= "2021-11-30", ]
range(nov)
  
dec<-  df.2[df.2$date >= "2021-12-01" & df.2$date <= "2021-12-31", ]
range(dec)
  
jan22<- df.2[df.2$date >= "2022-01-01" & df.2$date <= "2022-01-31", ]
range(jan22)
  

#################################################################
library(ggplot2)

c<- ggplot(df.2)
c
class(c)
attributes(c)
c$data
c$layers
c$scales
summary(c)
View(c$data)

ggplot(df.2) + aes(x = variant_name)
range(df.2$variant_name)

c<- ggplot(df.2) + aes(x = variant_name, y = specimens) + geom_point()
# baseline ggplot (first plot) - single dimension

c + geom_point(color = "blue")
# above graph in blue

#box and whisker plot - single dimension 
boxplot(specimens~variant_name, data = df.2, main = "COVID Variants"
        , xlab = "Variant Name", ylab = "Number of Specimen Collected")


barplot(table(df.2$variant_name), maint "Variant Distribution", xlab = "Variant Name" )

ggplot(jan, 
       aes(x = specimens
           , y = date
           , color = variant_name)) +
  geom_point(size = 3
             , shape = .7) +
  labs(title = "Count of Variant Per Day in January")
# multivariate
