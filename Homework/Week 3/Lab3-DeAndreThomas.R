######################################################################
#
#Author: DeAndre Thomas
#Purpose: Week 3 Lab: setting colors in R
#
######################################################################

library(RColorBrewer)
my.dir<- "C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\Lab 3\\"
sales<- read.csv(file=paste0(my.dir, "sales.csv")
                 , header = TRUE
                 , stringsAsFactors = FALSE)

colnames(sales)
display.brewer.all()

rand.data<- replicate(8, rnorm(35, 35, sd = 1.5))
boxplot(rand.data, col = brewer.pal(8, "Set1"))

num.colors<- 8
FUN<- colorRampPalette(c("blue", "red", "green"))
my.cols<- FUN(num.colors)
boxplot(rand.data, col = my.cols)

plot(sales$expenses, sales$income, pch = 16, cex = 1, col = "orange")

col.vec<- rep("orange", nrow(sales))
plot(sales$expenses, sales$income, pch = 16, cex = 1, col = col.vec)

col.vec<- rep(rgb(30, 144, 255, maxColorValue = 255), nrow(sales))
plot(sales$expenses, sales$income, pch = 16, cex = 1, col = col.vec)

hist(sales$unit.price)
col.vec[sales$unit.price > 14]<- rgb(255, 64, 64, maxColorValue = 255)
plot(sales$expenses, sales$income, pch = 16, cex = 1, col = col.vec)

col.vec<- rep(rgb(30, 144, 255, maxColorValue = 255), nrow(sales))
col.vec[sales$type == "red"]<- rgb(255, 64, 64, maxColorValue = 255)
plot(sales$expenses, sales$income, pch = 16, cex = 1, col = col.vec)


######################################################################
#
# Overplotting & transparency 
#
######################################################################

col.vec<- rep(rgb(.8, .15, .15), nrow(sales))
plot(sales$expenses, sales$income, pch = 16, cex = 1, col = col.vec)

col.vec<- rep(rgb(.8, .15, .15, alpha = .2), nrow(sales))
plot(sales$expenses, sales$income, pch = 16, cex = 1, col = col.vec)

col.vec<- rep(rgb(.8, .15, .15, alpha = .1), nrow(sales))
plot(sales$expenses, sales$income, pch = 16, cex = .3, col = col.vec)
# cex is the expansion size of a color

smoothScatter(sales$expenses, sales$income
              , colramp = colorRampPalette((c("black", "cyan", "pink", "red"))))

smoothScatter(sales$expenses, sales$income
              , colramp = colorRampPalette((c("white", "cyan", "pink", "red"))))

install.packages("aplack")
install.packages("aplpack")
library(aplpack)

bagplot(sales$expenses, sales$income
        , show.whiskers = FALSE
        , col.loophull = "#aaccff"
        , col.looppoints = "#3355ff"
        , col.baghull = "#7799ff"
        , col.bagpoints = "#000088"
        , transparency = T)

my.alpha <- 100
col.vec<- rep(rgb(30, 144, 255, maxColorValue = 255, alpha = my.alpha)
             , nrow(sales))

col.vec[sales$unit.price > 14] <- rgb(255, 64, 64
                                      , maxColorValue = 255
                                      , alpha = my.alpha)

plot(sales$expenses, sales$income, col = col.vec)

            
#Overplotting is the case where there's so many points plotted in a small space 
# that you can't actually see much about any patterns about the data.This can
# be combatted by making colors more transparent and/or changing point size for 
# the plot. 

n<- 1000
x<- rnorm(n)
y<- x^2 + rnorm(n, mean = 1, sd = .25)
plot(c(x, -1.5, 1.5, 0), c(y, 14, 14, 0))

A<- sample(c("here", "there", "nowhere", "everywhere"), size = n, replace = T)
B<- sample(c("now", "later"), size = n, replace = T)
barplot(table(B, A), beside = T)

pie(table(A))

















