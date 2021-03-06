

lowerboundgenerator <- function(p, n, alpha, nrep, scalar, j) {
  delta = log(log(n)) * sqrt(log(p))
  
  Calpha = qchisq(1 - alpha, p) - (1 + sqrt(p)) * j * delta^2
  
  if (Calpha  > p * delta^2) {
    lower = 0
  } else {
    inornot = rep(0, nrep)
    for (i in 1:nrep) {
      sample = urchisq(p, df = 1, 
                        lb = Calpha - (p - 1) * delta^2,
                        ub = delta^2)
      if (sum(sample) > Calpha) {
        inornot[i] = 1
      }
    }
    propin = mean(inornot)
    
    lower = propin * (pchisq(delta^2, df = 1, ncp = scalar^2) - 
                        pchisq(Calpha - (p - 1) * delta^2, df = 1, ncp = scalar^2))^p /
      pchisq(delta^2, df = 1, ncp = scalar^2)^p
  }
  
  return(lower)
}







