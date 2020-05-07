##### In this script we the first basic simulations can be done

# We use the rmvnorm function from the mvtnorm package as random number 
# generator for the multivariate normal distribution
source("Packages.R")

p = 200
h = 5
significance = 0.05

BasicSimulationProbReject = function(p, h, significance, nruns, c, 
                           Sigma = diag(rep(1, p))) {
  # Vector Theta is built up of h one's and p - h zeros 
  Theta = c(rep(1, h), rep(0, p - h))
  
  # We keep track of the rejections using the following vector
  # Per default, all values are zero but if test i rejects we set the ith
  # component equal to 1
  RejectVec = rep(0, nruns)
  
  deltapn = log(log(p)) * sqrt(log(p))
  
  
  # We now loop through the number of runs
  for (i in 1:nruns) {
    epsilon = rmvnorm(1, mean = rep(0, p), sigma = Sigma)
    epsilon = as.vector(epsilon)
    X = Theta + epsilon
    
    
    
    J1 = t(X) %*% X
    pvalue = pchisq(TestStat, df = p, lower.tail = FALSE)
    if (pvalue < significance) {
      RejectVec[i] = 1
    }
  }
  return(mean(RejectVec))
}


BasicSimulationProbReject(20, 0, 0.05, 20000)


