
source("Packages.R")


# In this function the variables denote the following:
# p : number of components in theta
# n : numboer of observations
# h : number of constants not equal to zero in underlying theta, so 
#     choosing h = 0 is the null hypothesis
# nruns : number of replications
# significance :significance level
# sigma : covariance matrix of the errors
# constant: constant in vector theta



SimulationProbRejectKP = function(p, n, h, nruns, significance = 0.05, 
                                     Sigma = diag(rep(1, p)), constant = 0.05) {
  
  
  # Vector Theta is built up of h constants and p - h zeros 
  Theta = c(rep(constant, h), rep(0, p - h))

  # We keep track of the rejections using the following vector
  # Per default, all values are zero but if test i rejects we set the ith
  # component equal to 1
  RejectVecInit = rep(0, nruns)
  RejectVecKP = rep(0, nruns)
  Shatelem = rep(0, nruns)
  
  deltapn = log(log(n)) * sqrt(log(p))
  
  iota = rep(1, n)
  
  # We now loop through the number of runs
  for (i in 1:nruns) {
    # First we generate n error terms for each component
    epsilon = rmvnorm(n, mean = rep(0, p), sigma = Sigma)
    # Data generating process
    X = iota %*% t(Theta) + epsilon
    thetahat = colMeans(X)
    J1 = n * t(thetahat) %*% thetahat
    
    # Now we generate J0
    # We let Shat be a boolean vector
    Shat = sqrt(n) * abs(thetahat) > deltapn
    Shatelem[i] = sum(Shat)
    J0 = sqrt(p) * n * sum(thetahat[Shat]^2)
    
    critval = qchisq(1 - significance, p)
    
    
    
    if (J1 > critval) {
      RejectVecInit[i] = 1
    }
    if (J0 > 0 || J1 > critval) {
      RejectVecKP[i] = 1
    }
    
  }
  returnlist = list("Initial" = mean(RejectVecInit), 
                    "KP" = mean(RejectVecKP))
  print(returnlist)
  }
}
