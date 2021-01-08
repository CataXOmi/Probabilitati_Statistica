#Teme de laborator
#1

library(discreteRV)

#Pb1

funcdens1 <- function(x)
{
  f <- rep(0,length(x))
  for(i in 1:length(x))
    if(x[i]>=0 && x[i]<= pi/2)
        f[i] <- cos(x[i])
    else 
      f[i]=0
  return(f)
}

tseq1 <- seq(-pi, 2*pi, 0.001)
plot(tseq1,funcdens1(tseq1),col="orange")

funcrep1 <- function(x)
{
  f <- rep(0,length(x))
  for(i in 1:length(x))
    if(x[i]>=0 && x[i]<= pi/2)
      f[i] <- sin(x[i])
    else
      if(x[i]>pi/2)
        f[i]=1
      else
        f[i]=0
  return(f)
}

plot(tseq1,funcrep1(tseq1),col="orange")

#Pb2

funcdens2 <- function(x)
{
  f <- rep(0,length(x))
  for(i in 1:length(x))
    if(x[i] >= 0 && x[i]<= 2)
      f[i] <- (exp(1)/(2*(exp(2)-1)))*(exp(-x[i]/2)+exp(x[i]/2))
    else 
      f[i]=0
    return(f)
}

tseq2 <- seq(-4, 5, 0.001)
plot(tseq2,funcdens2(tseq2),col="orange")

funcrep2 <- function(x)
{
  f <- rep(0,length(x))
  for(i in 1:length(x))
    if(x[i] >= 0 && x[i]<= 2)
      f[i] <- (exp(1)/(exp(2)-1))*(exp(x[i]/2)-exp(-x[i]/2))
    else
      if(x[i]>2)
        f[i]=1
      else
        f[i]=0
  return(f)
}

plot(tseq2,funcrep2(tseq2),col="orange")

#Pb3

funcdens3 <- function(x)
{
#Din cauza erorii object 'name' not found vom considera a=2, b=3
  a=2
  b=3
  f <- rep(0,length(x))
  for(i in 1:length(x))
    if(x[i] > 0 && x[i]< 1)
      f[i] <- (gamma(a+b)/(gamma(a)*gamma(b)))*(x[i]^(a-1))*((1-x[i])^(b-1))
    else 
      f[i]=0
  return(f)
}

tseq3 <- seq(-2, 8, 0.001)
plot(tseq3,funcdens3(tseq3),col="orange")

funcrep3 <- function(x)
{
  f <- rep(0,length(x))
  for(i in 1:length(x))
    if(x[i] > 0 && x[i]< 1)
      f[i] <- (x[i]^2)*(3*(x[i]^2) - 8*(x[i]) + 6)
    else
      if(x[i]>=1)
        f[i]=1
      else
        f[i]=0
      return(f)
}

plot(tseq3,funcrep3(tseq3),col="orange")

#Pb4

funcdens4 <- function(x)
{
  #Din cauza erorii object 'name' not found vom considera a=2
  a=2
  f <- rep(0,length(x))
  for(i in 1:length(x))
    if(x[i] >= 0)
      f[i] <- (1/(a^2))*x[i]*exp(-((x[i]^2)/(2*(a^2)))) 
  else 
    f[i]=0
  return(f)
}

tseq4 <- seq(-2, 5, 0.001)
plot(tseq4,funcdens4(tseq4),col="orange")

funcrep4 <- function(x)
{ 
  #Din cauza erorii object 'name' not found vom considera a=2
  a=2
  f <- rep(0,length(x))
  for(i in 1:length(x))
    if(x[i] >= 0)
      f[i] <- 1-exp(-((x[i]^2)/(2*(a^2))))
    else
        f[i]=0
    return(f)
}

plot(tseq4,funcrep4(tseq4),col="orange")

#Pb5

funcdens5 <- function(x,n)
{
  f <- rep(0, length(x))
  for (i in 1:length(x))
    if (x[i] >= 0)
      f[i] <- (1/((3^(n/2))*gamma(n/2)))*(x[i]^(n/2-1))*(exp(-x[i]/3))
    else
      f[i]=0
  return(f)
}

tseq5 <- seq(-8,21,0.001)
plot(tseq5, funcdens5(tseq5,4))
for(i in c(6,9,12)) lines(tseq5,funcdens5(tseq5,i), col=i+5, lwd=5)

#2
#Pb1

#P(X<pi/3)
(Pb1P1 <- integrate(funcdens1,-Inf,pi/3))

#P(X<pi/4|X>pi/6)
(Pb1P21 <- integrate(funcdens1,pi/6,pi/4))
(Pb1P22 <- integrate(funcdens1,pi/6,Inf))
(Pb1P2 <- Pb1P21$value/Pb1P22$value)

#Pb2

#P(X<1/2|X>1/4)
(Pb2P11 <- integrate(funcdens2,1/4,1/2))
(Pb2P12 <- integrate(funcdens2,1/4,Inf))
(Pb2P1 <- Pb2P11$value/Pb2P12$value)

#Pb3

#P(X<1/2)
(Pb3P1 <- integrate(funcdens3,-Inf,1/2))

#P(X>1/3)
(Pb3P2 <- integrate(funcdens3,1/3,Inf))

#P(X<=1/2|X>1/4)
(Pb3P11 <- integrate(funcdens3,1/4,1/2))
(Pb3P12 <- integrate(funcdens3,1/4,Inf))
(Pb3P1 <- Pb2P11$value/Pb2P12$value)

#Pb4

#P(X<2a)
#a a fost considerat egal cu 2
(Pb4P1 <- integrate(funcdens4,-Inf,4))

#P(X>a)
(Pb4P2 <- integrate(funcdens4,2,Inf))

#P(X<=4a|X>2a)
(Pb4P31 <- integrate(funcdens4,4,8))
(Pb4P32 <- integrate(funcdens4,4,Inf))
(Pb4P3 <- Pb4P31$value/Pb4P32$value)

#P(0<X<2SQRT(2PI))
(Pb4P4 <- integrate(funcdens4,0,2*sqrt(2*pi)))

#Dupa cum putem observa, valorile de pe foaie nu difera de cele din RStudio, decat in cazul aproximarilor
