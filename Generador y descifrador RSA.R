library(numbers)
library(gmp)

primo_relativo <- function (m, t) 
{
  
  while (MCD(t, m) != 1)
  {
    t <- t + 1
  }
  
  return (t)
}

MCD <- function(a, b)
{
  if (a > b) 
  {
    dividendo <- a
    divisor <- b
  }
  else 
  {
    dividendo <- b
    divisor <- a
  }
  
  while (divisor > 0)
  {
    resto <- dividendo %% divisor
    
    dividendo <- divisor
    divisor <- resto
  }
  return (dividendo)
}

euclides <- function (r, m)
{
  x0 <- 1; y0 <- 0
  x1 <- 0; y1 <- 1
  
  r0 <- r
  r1 <- m
  
  while (r1 != 0) {
    cociente <- r0 %/% r1
    r2 <- r0 %% r1
    
    x2 <- x0 - cociente * x1
    y2 <- y0 - cociente * y1
    
    r0 <- r1; r1 <- r2
    x0 <- x1; x1 <- x2
    y0 <- y1; y1 <- y2
  }
  
  return (list(mcd = r0, x = x0, y = y0))
}

modinv2 <- function(r, m)
{
  resultado <- euclides(r, m)
  
  x <- resultado$x
  
  inverso <- x %% m
  
  if (inverso == 0) {
    inverso <- m
  }
  
  return(inverso)
}

siguiente_primo <- function(n)
{
  num <- as.bigz(n)
  
  candidato <- num + 1
  
  while (isprime(candidato) == 0) {
    candidato <- candidato + 1
  }
  
  return(candidato)
}

claves_RSA_grandes <- function(a,b)
{
  p <- siguiente_primo(a)
  q <- siguiente_primo(b)
  
  cat("p = ", format(p,scientific=FALSE), "\n")
  cat("q = ", format(q,scientific=FALSE), "\n")
  
  p <- as.bigz(p)
  q <- as.bigz(q)
  
  n <- mul.bigz(p, q)
  
  m <- (p-1) * (q-1)
  
  r <- primo_relativo(m, a+b)
  s <- modinv2(r, m)
  
  cat("n = ", format(n,scientific=FALSE), "\n")
  cat("r = ",format(r,scientific=FALSE), "\n")
  cat("s = ",format(s,scientific=FALSE), "\n")
  
  return(c(n,r,s))
}

descifrarS <- function(n, r)
{
  nPrimos <- c()
  nPrimos <- factorize(as.bigz(n))
  
  m <- (nPrimos[1]-1) * (nPrimos[2]-1)
  s <- modinv2(as.bigz(r),m)
  
  return(s)
}

claves_RSA_grandes(2634758697353,293756536383)
# n =  773977589210346510018949 
# r =  2928515233739 
# s =  357613632435512419442759 
# [1] 773977589210346510018949 2928515233739

descifrarS("773977589210346510018949", "2928515233739")

