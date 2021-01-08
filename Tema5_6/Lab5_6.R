#Cerinte de laborator 1)

library(discreteRV)
library(MASS)

(X <- RV(c(-3,6),c(1/8,7/8)))
(Y <- RV(c(exp(1),exp(3)),c(1/4,3/4)))

outcomes(X)
probs(X)
outcomes(Y)
probs(Y)

#Outcomes si probs sau direct 
#1)
(X_2rv <- RV((-1)*outcomes(X)+2,probs(X)))
(X_2 <- 2-X)
(X_3rv <- RV(outcomes(X)^3,probs(X)))
(X_3 <- X^3)
(X_cosrv <- RV(cos((pi/6)*outcomes(X)),probs(X)))
(X_cos <- cos((pi/6)*X))
(Y_1rv <- RV(outcomes(Y)^(-1),probs(Y)))
(Y_1 <- Y^(-1))
(lnYrv <- RV(log(outcomes(Y)),probs(Y)))
(lnY <- log(Y))

#Rezultate obtinute pe foaie
(X_2rvsem <- RV(c(5,-4),c(1/8,7/8)))
(X_3rvsem <- RV(c(-27,216),c(1/8,7/8)))
(X_cosrvsem <- RV(c(0,-1),c(1/8,7/8)))
(Y_1rvsem <- RV(c(1/exp(1),1/exp(3)),c(1/4,3/4)))
(Y_lnrvsem <- RV(c(1,3),c(1/4,3/4)))

#Rezultatele s-au protrivit, exceptand cazul lui cos de pi pe 2, r nelucrand cu un pi simbolic, ci cu o aproximare a sa ofera un rezultat foarte apropiat de 0 de ordinul -17

#2)
X*Y
X/Y
X*(Y^(-1))
(XY2ABS <- abs(X-Y^2))

#Pentru a realiza inmultirea a doua variabile aleatoare ca pe foaie vom crea o functie
finmva <- function(va1,va2)
  {
    val1 <- outcomes(va1)
    val2 <- outcomes(va2)
    valinmVA <- c()
    
    for(i in val1)
    {
      for(j in val2)
        valinmVA <- c(valinmVA,i*j)
    }
    
    prob1 <- probs(va1)
    prob2 <- probs(va2)
    probinmVA <- c()
    
    for(i in prob1)
      {
        for(j in prob2)
          probinmVA <- c(probinmVA,i*j)
    }
    
    vainm <- RV(valinmVA,probinmVA)
    return(vainm)
  }
(XstarY <- finmva(X,Y))
(XstarY_1 <- finmva(X,Y_1rv))

#Rezultate obtinute pe foaie
(XYsem <- RV(c(-3*exp(1),-3*exp(3),6*exp(1),6*exp(3)),c(1/32,3/32,7/32,21/32)))
(XYlamin1sem <- RV(c(-3*exp(-1),-3*exp(-3),6*exp(-1),6*exp(-3)),c(1/32,3/32,7/32,21/32)))
(x_y2modsem <- RV(c(exp(2)+3,exp(6)+3,exp(2)-6,exp(6)-6),c(1/32,3/32,7/32,21/32)))

#Rezultatele se potrivesc mai putin in cazurile ce urmeaza
#In cazul in care facem inmultirea directa, r neavand o functie prestabilita o sa calculeze probabilitatile cum trebuie, dar valorile dintre constanta si numarul lui euler n-o sa se efectueze
#In cazul in care facem X/Y vom obtine doar 2 rezultate, iar in cazul X*(Y^(-1)) se va intampla ca in comentariul de  mai sus

#4)

(P1 <- fractions(P(XYsem <= exp(4))))
(XYjo <- jointRV(list(outcomes(X),outcomes(Y)),c(1/32,3/32,7/32,21/32)))

Xmarg <- marginal(XYjo,1) 
Ymarg <- marginal(XYjo,2)

(P2V1 <- fractions(P(XYsem >= 7|Xmarg < 0)))
(P2V2 <- fractions(P((XYsem >= 7)%AND%(Xmarg < 0))/P(Xmarg < 0)))
(P3V1 <- P((XYsem < 9) | (Y > 3)))
(P3V2 <- fractions(P((XYsem<9)%AND%(Y>3))/P(Y>3)))
(P4 <- fractions(P(XYlamin1sem < 1)))
(P5 <- fractions(P(x_y2modsem >= 3)))

independent(XYlamin1sem,x_y2modsem)

(P6v1 <- fractions(P(XYlamin1sem < x_y2modsem)))#aceasta varianta nu functioneaza, asa ca vom folosi 2 metode
#jointrv
#Vom calcula variabila aleatoare prin  inmultirea celor 2 variabile aleatoare pentru a obtine probabilitatile pentru joint
#(YstarX_1 <- finmva(Y))
vapartial <- finmva(YstarX_1,x_y2modsem)
(XYjo2 <- jointRV(list(outcomes(XYlamin1sem),outcomes(x_y2modsem)),probs(vapartial)))

Xmarg2 <- marginal(XYjo2,1) 
Ymarg2 <- marginal(XYjo2,2)
(P6v1fin <- P(Xmarg2 < Ymarg2)) 
#Varianta de mai sus pierde niste valori
#mutam membrul in partea stanga
(P6v2 <- fractions(P(XYlamin1sem - x_y2modsem < 0)))

(Xexc3 <- RV(c(-1,1),c(7/8,1/8)))
(Yexc3 <- RV(c(0,1),c(1/8,7/8)))

#Cerinte de laborator 2)
plot(X,col="magenta")
plot(Y,col="green") 
plot(X_2rvsem,col="blue")
plot(X_2,col="green")
plot(X_2rv,col="magenta")
plot(X_3rvsem,col="red")
plot(X_3rv,col="magenta")
plot(X_3,col="green")
plot(X_cosrvsem,col="yellow")
plot(X_cos,col="magenta")
plot(X_cosrv,col="green")
plot(Y_1rvsem,col="orange")
plot(Y_1rv,col="magenta")
plot(Y_1,col="green")
plot(Y_lnrvsem,col="gray")
plot(lnY,col="magenta")
plot(lnYrv,col="green")
plot(XYsem,col="brown")
plot(XstarY,col="magenta")
plot(XYlamin1sem,col="purple")
plot(XstarY_1,col="green")
plot(x_y2modsem,col="black")
plot(XY2ABS,col="blue")
plot(Xmarg,col="green")
plot(Ymarg,col="magenta")
plot(Xexc3,col="magenta")
plot(Yexc3,col="green")

#Cerinte de laborator 3)

fmasa <- function(vam)
{
  vout <- outcomes(vam);

  first <- vout[1];
  last <- vout[length(vout)];

  t <- seq(1,length(vout),1);
  fmas <- c();
  for(i in t)
    for(j in t)
      {
        if(vam[i]==vout[j])
        {
          fmas <- c(fmas,probs(vam)[i])
        }
        # daca vom lasa else-ul respectiv functia crapa deoarece o sa fie foarte multe valori de 0 adaugate fara rost
        # else
      }
  plot(t, fmas, col = "red")  
}

frepartitie <- function(var)
{
  vout <- outcomes(var);
  vprob <- probs(var);
  
  first <- vout[1];
  last <- vout[length(vout)];
  
  #Exista o problema in cazul in care valorile nu sunt ordonate spre exemplu avem 5 si -4 si functia da un grafic necorespunzator
  if(first > last)
  {
    temp <- first
    first <- last
    last <- temp
  }
  
  t <- seq(first-2,last+2,0.01);#daca nu scad si adun la primul, respectiv ultimul element, graficul este o singura linie in marea majoritate a cazurilor si am folosit 0.01 deoarece dura foarte mult compilarea pe anumite grafice precum modulul
  frep <- c();
  for(i in t)
  {
    frep <- c(frep,P(var <= i))
  }
  plot(t, frep, col = "blue")  
}

fmasa(X)
frepartitie(X)
fmasa(Y)
frepartitie(Y)
fmasa(X_2rvsem)
frepartitie(X_2rvsem)
fmasa(X_2)
frepartitie(X_2)
fmasa(X_2rv)
frepartitie(X_2rv)
fmasa(X_3rvsem)
frepartitie(X_3rvsem)
fmasa(X_3)
frepartitie(X_3)
fmasa(X_3rv)
frepartitie(X_3rv)
fmasa(X_cosrv)
frepartitie(X_cosrv)
fmasa(X_cos)
frepartitie(X_cos)
fmasa(X_cosrvsem)
frepartitie(X_cosrvsem)
fmasa(Y_1rvsem)
frepartitie(Y_1rvsem)
fmasa(Y_1rv)
frepartitie(Y_1rv)
fmasa(Y_1)
frepartitie(Y_1)
fmasa(Y_lnrvsem)
frepartitie(Y_lnrvsem)
fmasa(lnY)
frepartitie(lnY)
fmasa(lnYrv)
frepartitie(lnYrv)
fmasa(XYsem)
frepartitie(XYsem)
fmasa(XstarY)
frepartitie(XstarY)
fmasa(XYlamin1sem)
frepartitie(XYlamin1sem)
fmasa(XstarY_1)
frepartitie(XstarY_1)
fmasa(x_y2modsem)
frepartitie(x_y2modsem)
fmasa(XY2ABS)
frepartitie(XY2ABS)
fmasa(Xmarg)
frepartitie(Xmarg)
fmasa(Ymarg)
frepartitie(Ymarg)
fmasa(Xexc3)
frepartitie(Xexc3)
fmasa(Yexc3)
frepartitie(Yexc3)

#Cerinte de laborator 4)

generator <- function(n)
{
  valmon <- c("H","T")
  valprob <- c(1/2,1/2)
  experiment <- sample(valmon,size = n,replace = T,prob=valprob)
  
  Z <- list()
    
  for(k in 1:n)
  {
    if(experiment[k]=="H")
      Z[[k]] <- X
    else
      Z[[k]] <- Y
  }
  return(Z)
}
n <- 10
rep <- generator(n)
#Da, dar va trebui sa apelam functiile pentru fiecare element din lista si astfel vom obtine chiar functiile variabilelor noastre X si Y
for(i in 1:n)
{
  fmasa(rep[[i]])
  frepartitie(rep[[i]])
}
#Daca vom incerca sa apelam functia pe toata lista aceasta nu va sti cum sa parcurga si ce elemente sa compare si astfel vom primi eroare