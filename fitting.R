library(nlstools)
library(ggplot2)
tomato <- read.csv("tomato.csv")


formulaExp <- as.formula(A ~ (alpha*Q*Pmax/(alpha*Q+Pmax)-R))


preview(formulaExp, tomato, list(Pmax = 20, alpha = 0.1,R= 2.9))

tomnls <- nls(formulaExp, start = list(Pmax = 20, alpha = 0.1, R = 2.9), data = tomato)


summary(tomnls)


Pgross <- function(Q,alpha,pmax){
  
  R = 0.29
  X1 = Q*alpha *pmax
  X2 = Q*alpha + pmax
  
  Pg = X1/X2 - R
  return (Pg)
  
}

Q = 0:1500
pg1 = Pgross(Q,0.13,18)
pg2 = Pgross(Q,0.5,18)
plot(Q,pg1)+plot
