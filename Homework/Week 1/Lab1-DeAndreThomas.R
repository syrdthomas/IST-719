#
# Author: DeAndre Thomas
# Purpose: Lab 1, Week 1
# IST 719 Information Visualization
# Prof. Gary Krudys
#

pie(c(7, 4, 2, 12))
pie(c(7, 6, 7.2, 12))

x<- c(7, 6, 7.2, 12)
x
pie(x)

pie(x, main = "DeAndre's Pie")

pie(x, main = "DeAndre's Pie", col = c("red", "orange", "tan", "yellow"))

pie(x, main = "DeAndre's Pie", col = c("red", "orange", "tan", "yellow")
  , labels = c("a", "b", "c", "d"))

pie(x
    , main = "DeAndre's Pie"
    , col = c("red", "orange", "tan", "yellow")
    , labels = c("a", "b", "c", "d")
    )

plot(c(1,3,6,4))

plot(c(1,3,6,4), pch = 8)

plot(c(1,3,6,4), pch = 16, col = c("red", "orange", "tan", "yellow"))

plot(c(1,3,6,4), pch = 16, col = c("red", "orange", "tan", "yellow")
     , cex = 3
     )

# Variables

x<- "deandre"
x

x<- rnorm(n = 10)

plot(x)
plot(x, type = "l")
plot(x, type = "p")
plot(x, type = "h")
plot(x, type = "h", lwd = 5)
plot(x, type = "h", lwd = 5, lend = 2)
plot(x, type = "h", lwd = 5, lend = 2, col = "orange")
plot(x, type = "h", lwd = 5, lend = 2, col = "orange"
     , main = "change in net worth"
     , xlab = "time in years"
     , ylab = "in millions"
)
plot(x, type = "h", lwd = 5, lend = 2, col = "orange"
     , main = "change in net worth"
     , xlab = "time in years"
     , ylab = "in millions"
     , bty = "n"
)
par()

par(bg = "white")
plot(x, type = "h", lwd = 20, col = c("blue", "orange")
     , bty = "n", lend = 2)

my.par<- par()
par(my.par)

n<- 27

my.letters<- sample(letters[1:3], size = n, replace = T)

letters[7]
letters[7:9]
letters[c(8, 3, 1)]

my.letters[3]
my.letters[2]
tab<- table(my.letters)

barplot(tab)
barplot(tab, col = "brown")
barplot(tab, col = c("brown", "tan", "orange"))
barplot(tab, col = c("brown", "tan", "orange")
        , names.arg = c("sales", "ops", "it"))
barplot(tab, col = c("brown", "tan", "orange")
        , names.arg = c("sales", "ops", "it"))
barplot(tab, col = c("red", "tan", "orange")
        , names.arg = c("sales", "ops", "it")
        , border = "green")
barplot(tab, col = c("red", "tan", "orange")
        , names.arg = c("sales", "ops", "it")
        , border = "black")
barplot(tab, col = c("red", "tan", "orange")
        , names.arg = c("sales", "ops", "it")
        , border = "white")
barplot(tab, col = c("red", "tan", "orange")
        , names.arg = c("sales", "ops", "it")
        , border = "white"
        , xlab = "departments"
        , ylab = "employees")
barplot(tab, col = c("red", "tan", "orange")
        , names.arg = c("sales", "ops", "it")
        , border = "white"
        , xlab = "departments"
        , ylab = "employees"
        , main = "Company Employees")
barplot(tab, col = c("red", "tan", "orange")
        , names.arg = c("sales", "ops", "it")
        , border = "white"
        , xlab = "departments"
        , ylab = "employees"
        , main = "Company Employees"
        , horiz = TRUE)
barplot(tab, col = c("red", "tan", "orange")
        , names.arg = c("sales", "ops", "it")
        , border = "white"
        , xlab = "departments"
        , ylab = "employees"
        , main = "Company Employees"
        , horiz = TRUE
        , las = 1)
barplot(tab, col = c("red", "tan", "orange")
        , names.arg = c("sales", "ops", "it")
        , border = "white"
        , xlab = "departments"
        , ylab = "employees"
        , main = "Company Employees"
        , horiz = TRUE
        , las = 1
        , density = 20
        , angle = 45)

x<- rnorm(n = 1000, mean = 10, sd = 1)
hist(x, main = "What is the distribution of x?")

boxplot(x, horizontal = T)

x<- rlnorm(n = 1000, meanlog = 1, sdlog =1)

par(mfrow = c(2,1))
boxplot(x, horizontal = T)
hist(x)

hist(x)
hist(log10(x))
x^2



















