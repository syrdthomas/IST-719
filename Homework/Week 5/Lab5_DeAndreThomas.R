###############################################################
#
# Author: DeAndre Thomas
# Purpose: Week 5 Lab:: Working with Twitter Data
# Uses: ClimateTweets_UseForLecture_25k.csv
#
###############################################################

tweets<- read.csv("C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\Lab 5\\climatetweets_useforlecture_25k.csv"
                  , header = TRUE
                  , quote = "\""
                  , stringsAsFactors = FALSE)

View(tweets)

my.media<- tweets$media
table(my.media)
my.media[my.media == ""] <- "text only"

my.media<- gsub("\\|photo", "", my.media)

pie(100 * round(table(my.media)/sum(table(my.media)), 4))


tweets$created_at[1:3]
#"Wed Jul 06 03:35:37 +0000 2016"; convert from string to date
conversion.string<-"%a %b %d %H:%M:%S +0000 %Y"

tmp<- strptime(tweets$created_at[1:3], conversion.string)
  #takes all parts of string version of time and converts to dates
class(tmp)

any(is.na(tmp)) #no na's in vector

rm(tmp) #remove from environment
tweets$date<- strptime(tweets$created_at, conversion.string)

tmp<- "10AM and 27 minutes, on June 22, 1999"
str

strptime(tmp, "%H%p and %M minutes, on %B %d, %Y")

min(tweets$date)
max(tweets$date)
range(tweets$date)
summary(tweets$date)

difftime(min(tweets$date), max(tweets$date))
difftime(min(tweets$date), max(tweets$date), units = "min")
difftime(min(tweets$date), max(tweets$date), units = "weeks")


install.packages("lubridate")
library(lubridate)

wday(tweets$date[1:3], label = TRUE, abbr = TRUE)


barplot(table(wday(tweets$date[1:3], label = TRUE, abbr = TRUE)))
barplot(table(wday(tweets$date, label = TRUE, abbr = TRUE)))

tmp<- tweets$user_utc_offset

tweets$date[7:10] + tmp[7:10]

known.times<- tweets$date + tweets$user_utc_offset #many na values

index<- which(is.na(known.times)) #index of all Na values
known.times<- known.times[-index] #only contains reasonable values

barplot(table(hour(known.times)))


start.date<- as.POSIXct("2016-06-24 23:59:59")
end.date<- as.POSIXct("2016-06-26 00:00:00")

index<- which((tweets$date> start.date) & (tweets$date < end.date))

tweets.25th<- tweets$date[index]

format.Date(tweets.25th, "%Y%m%d%H%M")
tmp.date<- as.POSIXct(strptime(format.Date(tweets.25th, "%Y%m%d%H%M")
                               , "%Y%m%d%H%M")) 

plot(table(tmp.date))

length(table(tmp.date))
24*60


tmp.tab<- table(tmp.date)

plot(as.POSIXct(names(tmp.tab)), as.numeric(tmp.tab), type = "h")
class(names(tmp.tab))

x<- seq.POSIXt(from = start.date + 1, to = end.date - 1, by = "min")
length(x)
y<- rep(0, length(x))
y[match(names(tmp.tab), as.character(x))]<- as.numeric(tmp.tab)

plot(x,y, type = "p", pch = 16, cex = .4)
plot(x,y, type = "p", pch = ".", cex = .4)
plot(x,y, type ="l")

##################################################################### 
# hashtag word cloud
##################################################################### 

tweets$text[5:10] #funny text b/c during conversion, some characters dont appear in R well

library(stringr)
tags<- str_extract_all(tweets$text, "\\S+", simplify = FALSE)

tags<- tags[lengths(tags) > 0]
tags<- unlist(tags)

tags<- tolower(tags)
tags<- gsub("#|[[:punct:]]", "", tags)

tag.tab<- sort(table(tags), decreasing = TRUE)
tag.tab[1:10]

zap<- which(tag.tab < 3)
tag.tab<- tag.tab[-zap]

boxplot(as.numeric(tag.tab))
plot(as.numeric(tag.tab))

df<- data.frame(words = names(tag.tab), count = as.numeric(tag.tab)
                , stringsAsFactors = FALSE)

par(mfrow = c(3,3))
plot(df$count, main = "raw")
y<- df$count/max(df$count)
plot(y, main = "0 - 1")
plot(df$count^2, main = "^2")
plot(df$count^(1/2), , main = "^(1/2)")
plot(df$count^(1/5), , main = "^(1/5)")
plot(log10(df$count), main = "log10")
plot(log(df$count), main = "log")

######################################################################
# hashtag wordcloud
######################################################################

install.packages("ColorBrewer")
library(wordcloud)

myPal<-colorRampPalette(c("gold", "red", "orange"))

gc()

df
wordcloud(df$words, df$count, scale = c(5, .5), min.freq = 1
          , max.words = Inf, random.order = FALSE
          , random.color = FALSE, ordered.colors = TRUE)

index<- which(df$count > 10)
wordcloud(df$words[index], df$count[index], scale = c(5, .5)
          , min.freq = 1
          , max.words = 1, random.order = FALSE
          , random.color = FALSE, ordered.colors = TRUE
          , rot.per = 0, colors = myPal(length(df$words[index])))


par(mar=c(0,0,0,0), bg = "black")
mycounts<- (df$count[index])^(1/2)


########################################################################

sales<- read.csv("C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\Lab 3\\sales.csv"
                 , header = TRUE
                 , stringsAsFactors = FALSE)

install.packages("alluvial")
library(alluvial)
dat<- as.data.frame(Titanic, stringsAsFactors = FALSE)
alluvial(dat[,1:4], freq = dat$Freq)

alluv.df<- aggregate(sales$units.sold
                     , list(sales$rep.region, sales$type)
                     , sum)

colnames(alluv.df)<- c("reg", "type", "units.sold")

alluvial(alluv.df[,1:2], freq = alluv.df$units.sold)
my.cols<- rep("gold", nrow(alluv.df))
my.cols[alluv.df$type == "red"]<- "red"

alluvial(alluv.df[ , 1:2], freq = alluv.df$units.sold, col = my.cols)

alluvial(alluv.df[ , 1:2], freq = alluv.df$units.sold
         , col = ifelse(alluv.df$type == "red", "red", "gold"))

options(stringsAsFactors = FALSE)

alluv.df<- aggregate(sales$units.sold
                     , list(sales$rep.region
                            , sales$type
                            ,sales$wine)
                     , sum)

colnames(alluv.df)<- c("reg", "type", "wine", "units.sold")

alluvial(alluv.df[, 1:3], freq = alluv.df$units.sold
         , col = ifelse(alluv.df$type == "red", "red", "gray"))

library(RColorBrewer)
install.packages("treemap")
library(treemap)

colnames(sales)

treemap(sales, index = c("rep.region")
        , vSize = "income"
        , fontsize.labels = 18
        , palette = "Greens")

treemap(sales, index = c("rep.region")
        , vSize = "income"
        , vColor = "units.sold"
        , type = "dens"
        , fontsize.labels = 18
        , palette = "Greens")

treemap(sales, index = c("rep.region", "sales.rep", "type")
        , vSize = "income"
        , vColor = "units.sold"
        , type = "index"
        , fontsize.labels = 18
        , palette = brewer.pal(8, "Set1"))

#########################################################

install.packages("riverplot")
library(riverplot)

river<- riverplot.example()
par(mfrow = c(2,1), mar = c(1,1,1,1))
plot(river, srt = 90, lty = 1)
class(river)

x<- river
x$edges #connections between nodes in the plot
x$nodes

x$edges$Value[2]<- 45
x$edges$Value[1]<- 15
x$nodes$x[5]<- 5
plot(x)


df<- aggregate(sales$income
               , list(type = sales$type, wine = sales$wine)
               , sum)

df<- df[order(df$type, df$x), ]

node.name<- c("wine", unique(df$type), df$wine)
node.position<- c(1, 2,2, 3,3,3,3,3,3)
node.color<- rep("gray", length(node.name))
node.color<- c("deepskyblue", "red", "yellow"
               , "brown4", "firebrick3", "deeppink4"
               , "khaki1", "lightgoldenrod1", "gold", "goldenrod1")

node<- data.frame(ID = node.name
                  , x = node.position
                  , col = node.color
                  , stringsAsFactors = FALSE)

parent.nodes<- c("wine", "wine", df$type)
child.nodes<- c("red", "white", df$wine)

value<- c(sum(df$x[df$type == "red"]), sum(df$x[df$type == "white"]), df$x)
edges<- data.frame(N1 = parent.nodes, N2 = child.nodes, Value = value)
r<- makeRiver(node, edges)
par(mar=c(0,0,0,0))
plot(r)

#########################################################
# R Plots and Word
#########################################################

dat<- tapply(sales$units.sold, list(sales$type, sales$rep.region), sum)

barplot(dat, beside = TRUE, col = c("brown","gold")
        , main = "Units Sold by Region by Type")









