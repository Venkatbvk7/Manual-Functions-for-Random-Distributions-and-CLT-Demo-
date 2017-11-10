# 1. Random Number Generator.

randUniform <- function(n, a = 0, b = 1)
{
	# FUNCTION
	#Creating a seed value which is initialized to 4711
    if(!exists("lcg.seed")) lcg.seed <<- 4711
      #Setting widely used M,C,A parameters
    r.lcg = rep(0, times = n)
    A = 12345
    C = 1103515245
    M = 2^31-1
    for (i in 1:n) {
	#Substituting the values in the formula provided
        lcg.seed <<- (A*lcg.seed + C) %% M 
        r.lcg[i] = lcg.seed/M }
    r.lcg = (b-a)*r.lcg + a

	# RETURN
    r.lcg
}

ran1000<-randUniform(1000,0,1)
#Calculating the mean and the variance of the sample generated.
mean(ran1000)
var(ran1000)

#Theoretical Mean
0+1/2
#Theoretical Variance
((1-0)^2)/12


# CLT Demo

CLTdemo <- function(n,a,b)
{
	#FUNCTION
  z <- rep(0,n)
#Calling randUniform() function to generate the values inside a for loop
  for(i in 1:n)
  {
   u <- randUniform(n,a,b)
   z[i] <- mean(u)
  }
	#RETURN
  return(z)
}

#Using the CLTdemo function to create 1000 means with the provided a and b
x<-CLTdemo(1000,0,1)

#Generating a QQ plot for x
qqnorm(x, main="QQ plot for the 1000 means")
#Plotting a linear line through the QQ plots
qqline(x, col="red")


# Random Normal

randNormal<-function(n, mu = 0, sd = 1)
{   
	#FUNCTION 
a<-(qnorm(randUniform(n = n, a = 0, b = 1)) - mu)/sd^2
#a<-abs(a)
	#RETRUN
return(a)
}
