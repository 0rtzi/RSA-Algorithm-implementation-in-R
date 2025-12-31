library(numbers)
library(gmp)

# Primo relativo
# m: entero, t: entero
# Devuelve el menor entero >= t que es primo relativo con m
primo_relativo <- function (m, t) 
{
  while (MCD(t, m) != 1)
  {
    t <- t + 1
  }
  
  return (t)
}

# Máximo común divisor
# a: entero, b: entero
# Devuelve el máximo común divisor de a y b
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

# Algoritmo de Euclides
# r: entero, m: entero
# Devuelve una lista con el mcd de r y m, y los coeficientes x e y de la identidad de Bézout
euclides <- function (r, m)
{
  x0 <- 1; #y0 <- 0
  x1 <- 0; #y1 <- 1
  
  r0 <- r
  r1 <- m
  
  while (r1 != 0) {
    cociente <- r0 %/% r1
    r2 <- r0 %% r1
    
    x2 <- x0 - cociente * x1
    #y2 <- y0 - cociente * y1

    r0 <- r1; r1 <- r2
    x0 <- x1; x1 <- x2
    #y0 <- y1; y1 <- y2
  }
  
  return (list(mcd = r0, x = x0))#, y = y0))
}

# Inverso Modular
# r: entero, m: entero
# Devuelve el inverso modular de r módulo m
inverso_modular <- function(r, m)
{
  resultado <- euclides(r, m)
  
  x <- resultado$x
  
  inverso <- x %% m
  
  if (inverso == 0) {
    inverso <- m
  }
  
  return(inverso)
}

# Siguiente primo
# n: entero
# Devuelve el siguiente número primo mayor que n
siguiente_primo <- function(n)
{
  num <- as.bigz(n)
  
  candidato <- num + 1
  
  while (isprime(candidato) == 0) {
    candidato <- candidato + 1
  }
  
  return(candidato)
}

# Claves RSA Grandes
# a: entero, b: entero
# Devuelve un vector con las claves pública (n, r) y privada (s) generadas
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
  s <- inverso_modular(r, m)
  
  cat("n = ", format(n,scientific=FALSE), "\n")
  cat("r = ",format(r,scientific=FALSE), "\n")
  cat("s = ",format(s,scientific=FALSE), "\n")
  
  return(c(n,r,s))
}

# Descifrar S
# n: entero, r: entero
# Devuelve la clave privada s asociada a la clave pública (n, r)
descifrarS <- function(n, r)
{
  nPrimos <- c()
  nPrimos <- factorize(as.bigz(n))
  
  m <- (nPrimos[1]-1) * (nPrimos[2]-1)
  s <- inverso_modular(as.bigz(r),m)
  
  return(s)
}

# Ejemplo de uso
claves_RSA_grandes(2634758697353, 293756536383)
# n =  773977589210346510018949 
# r =  2928515233739 
# s =  357613632435512419442759 
# [1] 773977589210346510018949 2928515233739

descifrarS("773977589210346510018949", "2928515233739")
# [1] 357613632435512419442759