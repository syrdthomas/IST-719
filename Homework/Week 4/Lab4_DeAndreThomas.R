##########################################
#
# Author: DeAndre Thomas
# Purpose: Week 4 Lab:: Layouts in R
#
##########################################

sales<- read.csv("C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\Lab 4\\sales.csv"
                 , header = TRUE
                 , stringsAsFactors = FALSE)

dat.1<- tapply(sales$units.sold, list(sales$wine), sum)
dat.2<- tapply(sales$income, list(sales$wine), sum)

par(mfrow = c(2,1))
par(mar = c(.5,5,4,1), cex.lab = .8)
barplot(dat.2, xaxt = "n", las = 2)
mtext(text = "income", side = 2, line = 4, adj = 0)
mtext(text = "income on units sold", side = 3, line = 1, cex = 1.3,adj = 0)
par(mar = c(6,5,0,1), cex.lab = .8)
bar.out<- barplot(dat.1, xaxt = "n", las = 2)
axis(side = 1, at = bar.out, labels = gsub(" ", "\n", names(dat.2)), las = 2)

M<- matrix(
  c(1,1,1
    ,1,1,1
    ,2,2,2)
  , nrow = 3, byrow = T
)

layout(M)
layout.show(2)

par(mar = c(.5,5,4,1), cex.lab=.8)
barplot(dat.2, xaxt = "n", las = 2)
mtext(text = "income", side = 2, line = 4, adj = 0)
mtext(text = "income on units sold", side = 3, line = 1, cex = 1.3,adj = 0)
par(mar = c(6,5,0,1), cex.lab = .8)
bar.out<- barplot(dat.1, xaxt = "n", las = 2)
axis(side = 1, at = bar.out, labels = gsub(" ", "\n", names(dat.2)), las = 2)


M<- matrix(
  c(1,1,1,3
    ,1,1,1,4
    ,2,2,2,5)
  , nrow = 3, byrow = T
)

layout(M)
layout.show(5)

par(mar = c(.5,5,4,1), cex.lab=.8)
barplot(dat.2, xaxt = "n", las = 2)
mtext(text = "income", side = 2, line = 4, adj = 0)
mtext(text = "income on units sold", side = 3, line = 1, cex = 1.3,adj = 0)
par(mar = c(6,5,0,1), cex.lab = .8)
bar.out<- barplot(dat.1, xaxt = "n", las = 2)
axis(side = 1, at = bar.out, labels = gsub(" ", "\n", names(dat.2)), las = 2)

par(mar = c(1,1,1,1))
pie(dat.3)
pie(dat.4)
pie(dat.5)

###############################################################

dat.3<- tapply(sales$units.sold, list(sales$type), sum)
dat.4<- tapply(sales$units.sold, list(sales$rep.region), sum)
dat.5<- tapply(sales$units.sold, list(sales$year), sum)

split.screen(figs = c(2,1))
screen(1)
pie(dat.1)

screen(2)
pie(dat.2)

screen(1, new = F)
mtext("Jeff", side = 3, line = 1)

screen(2, new = F)
mtext("here", side = 3, line = 1)

close.screen(1:2)

split.screen(c(2,2))
screen()
pie(dat.3)


############################################################
#
# Purpose: Week 4 Lab:: Fonts
#
############################################################

n<- 500
x<- abs(rnorm(n, 6,2))
y<- x^2 +rnorm(n, 0,2*x)

my.par<- par()

my.par$adj
my.par$family

plot(x,y)

my.par$font

plot(x,y, main = "Fiddling with Fonts", xlab = "some x lab"
     , ylab = "ylab text")

plot(x,y, main = "Fiddling with Fonts", xlab = "some x lab"
     , ylab = "ylab text", font = 2) # bold

plot(x,y, main = "Fiddling with Fonts", xlab = "some x lab"
     , ylab = "ylab text", font = 3) #italics

plot(x,y, main = "Fiddling with Fonts", xlab = "some x lab"
     , ylab = "ylab text", font = 4) #italics bold

plot(x,y, main = "Fiddling with Fonts", xlab = "some x lab"
     , ylab = "ylab text", font = 5) #serif font 

my.par$font.axis 
my.par$font.lab

plot(x,y, main = "Fiddling with Fonts", xlab = "some x lab"
     , ylab = "ylab text", font.axis = 2, font.lab = 3, font.main = 1)


plot(x,y, main = "Fiddling with Fonts", xlab = "some x lab"
     , ylab = "ylab text", family = "HersheyGothicEnglish")


par(family = "mono")
plot(x,y, main = "Fiddling with Fonts", xlab = "some x lab"
     , ylab = "ylab text")

par(family = "serif")
plot(x,y, main = "Fiddling with Fonts", xlab = "some x lab"
     , ylab = "ylab text")

par(family = "sans")
plot(x,y, main = "Fiddling with Fonts", xlab = "some x lab"
     , ylab = "ylab text")

plot(1:10, 1:10, type = "n") #creates the plot
windowsFonts(
  A = windowsFont("Arial Black"),
  B = windowsFont("Bookman Old Style"),
  C = windowsFont("Comic Sans MS"),
  D = windowsFont("Symbol")
)

text(2,2, "Hello World Default")
text(3,3, "Hello World Arial", family = "A")
text(4,4, "Hello World Bookman", family = "B")
text(5,5, "Hello World Comic", family = "C")
text(6,6, "Hello World Symbol", family = "D")

library(extrafont) #gives extra fonts
loadfonts(device = "win")
fonts() #recieves "Null" message when line ran






































