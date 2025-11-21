# 1) Change the default directory
setwd("C:\\Users\\theoj\\Documents\\Courses\\STAT457\\Lab1") #sets the working directory
getwd() #shows the current working directory

#2) Arithmetic: +, -, *, ^, log(), exp(), log10(), abs()
11+5/2^3
sqrt(11-2)
(log(5) + exp(6)) * log10(123)/abs(-9) #parenthesis matter

#3) Variables
a <- 3 #same as a = 3
a = 3 #same as a <- 3
a #print value of variable

aa <- sqrt(13*3)
aa #print value of variable
b #nothing has been stored in b yet

long.name.is.valid = 3 # valid variable names have to start with a letter, and words can be separated with "." or "_"
a == aa # logicals to compare objects (==, !=, <, >)

b = 1:100 # vector of values from 1 to 100
z = qnorm(0.95) # 0.95 quantile of N(0,1) distribution
b <- seq(2,100,2)

# 4) Vectors
x = c(2,3,5,1,4,4)
z = seq(2,1000,2)

x[3] #3rd element of a vector
x[2:4] #elements 2 through 4 (2nd, 3rd, 4th)
x[c(1:4,6)] #elements 1 through 4, and 6th element
x[-5] #all elements excluding 5th element
x[-c(2:4)] # all elements excluding elements 2 through 4

#functions that work with vectors
# mean(), var(), sd(), median(), sum(), min(), max()

# 5) Obtaining Help: Use ? or help()
?max
?sum
?plot
help(plot)

y = c(1,2,3,6,3,4)
plot(x,y)
y=c(1,2,4,6)


