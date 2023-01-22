#
# Author DeAndre Thomas
# Purpose: Lab 2, data interogration or data exploration
#           and distributions
#

fname<- file.choose()
tips<- read.csv(file = fname
                , header = TRUE
                , stringsAsFactors = FALSE) #factors are categorical;treat
#                                             strings as strings
# 8 columns and 244 rows
colnames(tips) #column names
fix(tips) #edit data in spreadsheet view
View(tips) #spreadsheet view
str(tips) #structure

tips[1, ] #first row
tips[ ,1] #first column
tips[3,3] #3rd row, 3rd column
tips[1:3, ] #first 3 rows

length(tips[1:3, 2]) #first 3 rows of second column

dim(tips) #number of dimensions in dataset
dim(tips)[1]

tips$time #shows values in column
tips[ , "time"] #""

plot(tips$total_bill) #a way to look at distribution
plot(sort(tips$total_bill)) #a way to look at distribution
boxplot(tips$total_bill)#a formal way to look at distribution
hist(tips$total_bill)# a formal way to look at distribution 
d<- density(tips$total_bill) #shape of the data
plot(d)
polygon(d, col = "orange") #this did not create plot 

par(mfrow = c(2,2)) #if asked for four different ways to look at distribution
boxplot(tips$total_bill)
hist(tips$total_bill)
d<- density(tips$total_bill)
plot(d)
polygon(d, col = "orange")

install.packages("vioplot")
library(vioplot) #violin plot, showing central tendency; similar to boxplot 
vioplot(tips$total_bill)

unique(tips$sex)

tips.m<- tips[tips$sex == "Male", ] #create a set that only includes male data
View(tips.m)

tips.f<- tips[tips$sex == "Female", ]

par(mfrow = c(2,1), mar = c(2,3,1,2))
boxplot(tips.f$tip, horizontal = T, ylim = c(1, 10))
boxplot(tips.m$tip, horizontal = T, ylim = c(1, 10))

fname<- "C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\Lab 2\\tweet.formated.json"
library(jsonlite)

raw.tweet<- fromJSON(fname, flatten = FALSE)

str(raw.tweet)

View(raw.tweet) #does not work with JSON files
names(raw.tweet)

raw.tweet$text
raw.tweet$user$followers_count #exercising lists, which is diff from vector

raw.tweet[["user"]]
raw.tweet[["user"]]$followers_count
raw.tweet[["user"]][["followers_count"]]

fname<- "C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\Lab 2\\tweets5814.json"
con<- file(fname, open = "r")

tweets<- stream_in(con)
close(con)

dim(tweets)

tweets$text[1:3]
boxplot(log10(tweets$user$followers_count), horizontal = TRUE)

task.time<- c(rnorm(n = 30, mean = 30, sd = 2.25)
              , rnorm(n = 30, mean = 25, sd = 1.5))
hist(task.time)
status<- c(rep("AMA", 30), rep("PRO", 30))
df<- data.frame(time = task.time, status = status)

View(df)
df.grouped<- aggregate(df$time, list(df$status), mean) #aggregate returns things as df
colnames(df.grouped) <- c("stat", "time")
df.grouped

class(df.grouped)

barplot(df.grouped$time, names.arg = df.grouped$stat)

M.grouped<- tapply(df$time, list(df$status), mean) #tapply delivers matrix or vector
class(M.grouped)

tapply(df$time, list(df$status), range)

range(task.time)
summary(task.time)

aggregate(df$time, list(df$status), summary)

table(df$status) #counts and does not group
table(round(df$time, 2))

df$sex<- sample(c("M", "F"), 60, replace = T)

aggregate(df$time, list(df$status, df$sex), mean)
M<- tapply(df$time, list(df$status, df$sex), mean)

M<- tapply(df$time, list(df$sex, df$status), mean)

barplot(M, beside = TRUE)

###############################################
#
# reshaping data with tidyr
#
###############################################

# gather() makes "wide" data longer
# spread() makes "long" data wider
# separate() splits a single column into multiple columns
# unite() combines multiple columns into a single column

library(tidyr)

n<- 5
year<- 2001: (2000 + n)

q1<- runif(n = n, min = 100, max = 120)
q2<- runif(n = n, min = 103, max = 130)
q3<- runif(n = n, min = 105, max = 140)
q4<- runif(n = n, min = 108, max = 150)

df.wide<- data.frame(year, q1, q2, q3, q4)

gather(df.wide, qt, sales, q1:q4) #makes data longer

df.wide %>% gather(qt, sales, q1:q4) #same as above; %>% take thing from left to right

df.long<- df.wide %>% gather(qt, sales, q1:q4)

o<- order(df.long$year, df.long$qt)
df.long<- df.long[o, ]

gather(df.wide, qt, sales, q1, q2, q3, q4)
gather(df.wide, qt, sales, 2:5)

df<- data.frame(cat = rep(c("tap", "reg", "zed", "vum"), 3)
                , group = rep(letters[7:9], 4)
                , x = 1:12)

spread(df, cat, x) #widens df or matrix format




################################################
#
# using rect functions to build custom plot
# builds rectangles to build custom plot
################################################
install.packages("plotrix")
library(plotrix) 

n<- 7000
age.min<- 1
age.max<- 90
age.range<- c(age.min, age.max)
m<- round(rescale(rbeta(n, 5, 2.5), age.range), 0) #rbeta, random distribution in beta format
hist(m)
f<- round(rescale(rbeta(n, 5, 2.0), age.range), 0)
x<- age.min:age.max
f.y<- m.y<- rep(0, length(x))


m.tab<- table(m)
m.y[as.numeric((names(m.tab)))]<- as.numeric(m.tab)

f.tab<- table(f)
f.y[as.numeric((names(f.tab)))]<- as.numeric(f.tab)

age.freqs<- data.frame(ages = x, males = m.y, females = f.y)

max.x<- round(1.2 * max(age.freqs[ ,2:3]), 0)
plot(c(-max.x, max.x), c(0,100), type = "n", bty = "n", xaxt = "n"
     , ylab = "age", xlab = "freq", main = "sample age distribution")


grid() #puts grid on plot space
last.y<- 0
  for (i in 1:90) {
    i<- 1
    rect(xleft = 0, ybottom = last.y, xright = -age.freqs$males[i]
         , ytop = age.freqs$ages[i], col = "lightblue2", border = NA)
    #draws a rectangle
    rect(xleft = 0, ybottom = last.y, xright = -age.freqs$females[i]
         , ytop = age.freqs$ages[i], col = "lightpink", border = NA)
    
    last.y<- age.freqs$ages[i]
}


































