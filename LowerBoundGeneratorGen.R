
### Lowerbound generator general K&P
source("Packages.R")

lowerboundgeneratorgeneral <- function(p, n, alpha, nrep, theta) {
  delta = log(log(n)) * sqrt(log(p))
  
  Falpha = qchisq(1 - alpha, p)
  
  if (Falpha  > p * delta^2) {
    lower = 0
  } else {
    inornot = rep(0, nrep)
    for (i in 1:nrep) {
      sample = c()
      for (j in 1:p) {
        sample[j] = urnorm(1, mean = sqrt(n) * theta[j], lb = 0, ub = delta)^2
      }
      if (sum(sample) > Falpha) {
        inornot[i] = 1
      }
    }
    propin = mean(inornot)

  }
  
  return(propin)
}

Sys.time()
lowerboundgeneratorgeneral(100, 100, 0.05, 2000, rep(0, 100))
Sys.time()

