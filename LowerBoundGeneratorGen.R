
### Lowerbound generator general K&P

lowerboundgeneratorgeneral <- function(p, n, alpha, nrep, theta) {
  delta = log(log(n)) * sqrt(log(p))
  
  Falpha = qchisq(1 - alpha, p)
  
  if (Falpha  > p * delta^2) {
    lower = 0
  } else {
    inornot = rep(0, nrep)
    for (i in 1:nrep) {
      sample = urchisq(p - numbsparse, df = 1, 
                       lb = Falpha - (p - 1) * delta^2,
                       ub = delta^2)
      sample = c(sample, urnorm(numbsparse, mean = sqrt(n) * scalar, lb = 0, ub = delta)^2)
      if (sum(sample) > Falpha) {
        inornot[i] = 1
      }
    }
    propin = mean(inornot)
    
    lower = propin
  }
  
  return(lower)
}



