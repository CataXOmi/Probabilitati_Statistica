
library(discreteRV)

#Pb1

#b)
Repcom <- jointRV(list(c(-1,2),c(-2,0,2)),c(0.2,0.3,0.1,0.1,0.1,0.2)) 
(Xmarg <- marginal(Repcom,1))
(Ymarg <- marginal(Repcom,2))

#c)
independent(Xmarg,Ymarg)

#d)
(At = Xmarg + Ymarg)
(B1 = Xmarg*Ymarg)

#Pentru a realiza adunarea a doua variabile aleatoare ca pe foaie vom crea o functie
faddva <- function(va1,va2)
{
  val1 <- outcomes(va1)
  val2 <- outcomes(va2)
  valaddVA <- c()
  
  for(i in val1)
  {
    for(j in val2)
      valaddVA <- c(valaddVA,i+j)
  }
  
  prob1 <- probs(va1)
  prob2 <- probs(va2)
  probaddVA <- c()
  
  for(i in prob1)
  {
    for(j in prob2)
      probaddVA <- c(probaddVA,i)
    probaddVA <- c(probaddVA,j)
  }
  
  vaadd <- RV(valaddVA,probaddVA)
  return(vaadd)
}

#Pentru a realiza inmultirea a doua variabile aleatoare ca pe foaie vom crea o functie
finmva <- function(va3,va4)
{
  val3 <- outcomes(va3)
  val4 <- outcomes(va4)
  valinmVA <- c()
  
  for(i in val3)
  {
    for(j in val4)
      valinmVA <- c(valinmVA,i*j)
  }
  
  prob3 <- probs(va3)
  prob4 <- probs(va4)
  probinmVA <- c()
  
  for(i in prob3)
  {
    for(j in prob4)
      probinmVA <- c(probinmVA,j)
    probinmVA <- c(probinmVA,i)
  }
  
  vainm <- RV(valinmVA,probinmVA)
  return(vainm)
}

(Bt <- finmva(Xmarg,Ymarg))

#Dupa cum putem observa variabilele aleatoare din cadrul acestei 
#probleme sunt dependente ceea ce fara o functie pentru suma si inmultire, 
#altfel va returna un rezultat gresit deoarece RStudio le va aplica formula pentru independente.
#Asadar am decis sa scriu valorile lui A si B de la tastatura.

(A = RV(c(-3,-1,0,1,2,4),c(0.2,0.3,0.1,0.1,0.2)))
(B = RV(c(-4,-2,0,2,4),c(0.1,0.1,0.4,0.2,0.2)))


#e)
E(Xmarg)
E(Ymarg)
V(Xmarg)
V(Ymarg)

#f)
(covarianta = E(B) - E(Xmarg)*E(Ymarg))
(coefcorel = covarianta / sqrt(V(Xmarg)*V(Ymarg)))

#g)
(XY2 <- Xmarg | (Ymarg == 2))
(YX_1 <- Ymarg | (Xmarg == -1))
E(XY2)
E(YX_1)

#Pb2

#a)
#Consideram K=3/20
K=3/20
#In acest caz functia independent va returna FALSE
Repcom2 <- jointRV(list(c(-1,1),c(-1,0)),c(1/5-K,1/20+K,K,3/4-K))
(Xmarg2 <- marginal(Repcom2,1))
(Ymarg2 <- marginal(Repcom2,2))

#In acest caz functia independent va returna TRUE
X <- RV(c(-1, 1), c(1/4, 3/4))
Y <- RV(c(-1, 0), c(1/5, 4/5))
repXY <- jointRV(list(outcomes(X), outcomes(Y)), c(1/5-K,1/20+K,K,3/4-K))

#b)
(XY <- Xmarg2*Ymarg2)

#Pentru a face inmultirea variabilelor aleatoare vom folosi o functie
finmva2 <- function(va5,va6)
{
  val5 <- outcomes(va5)
  val6 <- outcomes(va6)
  valinmVA2 <- c()
  
  for(i in val5)
  {
    for(j in val6)
      valinmVA2 <- c(valinmVA2,i*j)
  }
  
  prob5 <- probs(va5)
  prob6 <- probs(va6)
  probinmVA2 <- c()
  
  for(i in prob5)
  {
    for(j in prob6)
      probinmVA2 <- c(probinmVA2,j)
    probinmVA2 <- c(probinmVA2,i)
  }
  
  vainm2 <- RV(valinmVA2,probinmVA2)
  return(vainm2)
}

(XinmY <- finmva2(Xmarg2,Ymarg2))

(covarianta2 = E(XinmY) - E(Xmarg2)*E(Ymarg2))
(coefcorel2 = covarianta2 / sqrt(V(Xmarg)*V(Ymarg)))

#Exista o exceptie. Daca K nu este 3/20 atunci variabilele sunt dependente, 
#caz in care se va face fie o functie pentru X*Y, fie le vom introduce de la tastatura.

(XstarY=RV(c(-1,0,1),c(K,4/5,1/5-K)))

(covarianta2dep = E(XstarY) - E(Xmarg2)*E(Ymarg2))
(coefcorel2dep = covarianta2dep / sqrt(V(Xmarg)*V(Ymarg)))

#c)
#Ca sa fie necorelate coeficientul de corelatie este 0, deci K este cel ales mai devreme si anume 3/20 
(P21=P((Xmarg2 == -1)%AND%(Ymarg2 == -1)))

(P22 <- P(Xmarg2 == -1))
(P23 <- P(Ymarg2 == -1))

(P24 <- P22*P23)

(P25=P((Xmarg2 == -1)%AND%(Ymarg2 == 0)))

(P26 <- P(Xmarg2 == -1))
(P27 <- P(Ymarg2 == 0))

(P28 <- P26*P27)

(P29=P((Xmarg2 == 1)%AND%(Ymarg2 == -1)))

(P30 <- P(Xmarg2 == 1))
(P31 <- P(Ymarg2 == -1))

(P32 <- P30*P31)

(P33=P((Xmarg2 == 1)%AND%(Ymarg2 == 0)))

(P34 <- P(Xmarg2 == 1))
(P35 <- P(Ymarg2 == 0))

(P36 <- P34*P35)

indep <- function(j,k)
{
  if(j==k)
    ok <- 1
  else
    ok <- 0
  return(ok)
}

if(indep(P21,P24)==1 %AND% indep(P25,P28)==1 %AND% indep(P29,P32)==1 %AND% indep(P33,P36)==1)
  okf <- 1
if(indep(P21,P24)==0 %OR% indep(P25,P28)==0 %OR% indep(P29,P32)==0 %OR% indep(P33,P36)==0)
  okf <- 0
(okf)

independent(Xmarg2,Ymarg2)
independent(X,Y)
