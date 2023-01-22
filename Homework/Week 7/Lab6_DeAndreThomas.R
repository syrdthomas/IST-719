########################################################
#
#Author: DeAndre Thomas
# Purpose: Week 6 Lab:: ggplot
# uses sales.csv
#
#######################################################

sales<- read.csv("C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\Lab 3\\sales.csv"
                 , header = TRUE
                 , stringsAsFactors = FALSE)

library(ggplot2)

p<- ggplot(sales)
p
class(p)
attributes(p)
p$data
p$layers
p$scales
summary(p)
View(p$data)

ggplot(sales)+ aes(x = expenses)
range(sales$expenses)

plot(sales$expenses)

ggplot(sales, aes(x = expenses))
ggplot(sales)+ aes(x = expenses) #these two lines create the same visual, blank plot

ggplot(sales) + aes(x = expenses, y = income) + geom_point()
ggplot(sales) + aes(x = sales$expenses, y = sales$income) + geom_point() #same graph

ggplot(sales) + 
  aes(x = sales$expenses, y = sales$income) + 
  geom_point() #plus should always be on the previous line

p<- ggplot(sales)
p + aes(x = sales$expenses, y = sales$income) + 
  geom_point()

p<- ggplot(sales) + aes(x = sales$expenses, y = sales$income)
p + geom_point()

p<- ggplot(sales) + aes(x = sales$expenses, y = sales$income)
p + geom_point(color = "blue")

save(p, file =)

ggplot(sales) + 
  aes(x = sales$expenses, y = sales$income, color = type) + 
  geom_point() #differentiates per type of wine


# Lets color the points by unit cost
ggplot(sales) + 
  aes(x = expenses, y = income, color = unit.price > 14) + 
  geom_point()

ggplot(sales) + 
  aes(x = expenses, y = income) + 
  geom_point(color = ifelse(sales$unit.price > 14, "red", "green")) 

ggplot(sales) + 
  aes(x = expenses, y = income, color = unit.price) + 
  geom_point() 

ggplot(sales) + 
  aes(x = expenses, y = income, color = unit.price, shape = type) + 
  geom_point() #setting shapes for type of wine

ggplot(sales) + 
  aes(x = expenses, y = income
      , color = unit.price
      , shape = type
      , alpha = income) + 
  geom_point()

ggplot(sales) + 
  aes(x = expenses, y = income
      , color = rep.region
      , shape = type
      , alpha = unit.price
      , size = units.sold) + 
  geom_point()

p1<- ggplot(sales)
p2<- ggplot(sales) + aes(y = income, x = expenses, shape = rep.region)

summary(p1)
summary(p2)

attributes(p1)
attributes(p2)

p1$labels
p2$labels

p1$mapping
p2$mapping

#################################################################
# geoms
#################################################################

ggplot(sales) + aes(y = income, x = expenses) + geom_point() + geom_rug()
#adds one tick per observation. 

ggplot(sales) + aes(y = income, x = expenses) + geom_point() + geom_rug()

income.pred<- predict(lm(sales$income~sales$expenses))

ggplot(sales) + aes(y = income, x = expenses) + geom_point() + 
  geom_line(aes(y = income.pred), color = "red", lwd =3) #plots with line

ggplot(sales) + aes(y = income, x = expenses) + geom_point() + 
  aes(y = income.pred) + geom_line(color = "red", lwd =3) #draws line

ggplot(sales) + aes(y = income, x = expenses) + geom_point(color = "pink") + 
  geom_rug() +
  geom_line(aes (y = income.pred)) +
  geom_line(aes (y = income.pred + 150)) +
  geom_vline(xintercept = 10, color = "blue") + 
  geom_hline(yintercept = 500, color = "orange") + 
  geom_abline(intercept = 50, slope = 100, "red", lty =3, lwd = 2)

ggplot(sales) + aes(y = income, x = expenses) + geom_point() + 
  geom_smooth(method = "loess") #creates something similar to trend line/predictive line
  
ggplot(sales) + aes(y = income, x = expenses) + geom_point() + 
  geom_smooth() #smoothens line of the above code

ggplot(sales) + aes(y = income, x = expenses) + geom_bin2d(bins = 50)

price<- ifelse(sales$unit.price > 14, "expensive", "moderate")
price[sales$unit.price < 9]<- "cheep"

ggplot(sales) + aes(y = income, x = expenses, color = price) + 
  geom_bin2d(bins = 50)

########################################################################
# other geoms
########################################################################

df<-aggregate(sales$units.sold, list(year = sales$year), sum)
df2<- aggregate(sales$units.sold
                , list(year = sales$year, region = sales$rep.region), sum)

ggplot(sales) + aes(x = income) + geom_blank()
ggplot(sales) + aes(x = income) + geom_histogram()
ggplot(sales) + aes(x = income) + geom_histogram(binwidth = 10)

ggplot(sales) + aes(x = income) + 
  geom_histogram(binwidth = 10, fill = "orange") +
  geom_vline(aes(xintercept = mean(income)), 
             color = "blue", linetype = "dashed", size = 1)

ggplot(sales) + aes(x = income) + 
  geom_histogram(binwidth = 10, fill = "orange", alpha = .9) +
  aes(y = ..density..) +
  geom_density(alpha= .3, fill = "blue", color = "blue")

ggplot(sales) + aes(x = rep.region, y = income) + geom_boxplot()

df
ggplot(df) + aes(x = year, y = x) + geom_line() + ylim(c(0,40000))

ggplot(df) + aes(x = year, y = x) + geom_step() + ylim(c(0,40000))

ggplot(df) + aes(x = year, y = x) + 
  geom_ribbon(ymin = df$x - 1000, ymax = df$x + 1000, fill = "yellow") +
  geom_line() + ylim(c(0, 40000))
# Error: Either ymin or ymax must be given as an aesthetic.


df2
ggplot(df2) + aes(x = year, y = x, color = region) +
  geom_line() + ylim(c(0, 10000))

#################################################################
# geom_bar
#################################################################


df<- aggregate(sales$units.sold, list(region = sales$rep.region), sum)
colnames(df)[2]<- "sales"

ggplot(sales) + aes(x = rep.region) + geom_bar(fill = "orange", width = .5) +
  ggtitle("Number of Sales by Region")

ggplot(sales) + aes(x = rep.region, fill = type) + 
  geom_bar(position = "fill")
# fill does percentage


ggplot(df) + aes(x = region, y = sales, fill = region) + 
  geom_bar(stat = "identity")
# identity = don't do any tabling; everything is 1:1

ggplot(df) + aes(x = "", y = sales, fill = region) +
  geom_bar(width = .3, stat = "identity") +
  coord_polar("y", start = 45)

hist(sales$income)

p<- ggplot(sales) + aes(x = income)
p + geom_histogram() + stat_bin(binwidth = 20)

p + stat_density() #density plot

ggplot(sales) + aes(y = income) + geom_boxplot() + stat_boxplot()

ggplot(sales) + aes(y = income) + stat_boxplot()

ggplot(sales) + aes(x = sales$expenses, y = income) + stat_bin2d() +
  stat_density2d(col = "red")

ggplot(sales) + aes(x = rep.region) + geom_bar
ggplot(sales) + aes(x = rep.region) + stat_count() #does the same as the above

df

ggplot(df) + aes(x = region, y = sales) + geom_bar(stat = "identity")

ggplot(df) + aes(x = region, y = sales) + geom_bar() + stat_identity()
# Error: stat_count() can only have an x or y aesthetic.

ggplot(sales) + aes(x = income) +
  geom_histogram(aes(fill = ..count..)) +
  aes(y = ..density..) +
  geom_density(fill = "yellow", alpha = .1)




































































































