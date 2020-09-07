### In this script the simulations can be done ############################

# We use the rmvnorm function from the mvtnorm package as random number 
# generator for the multivariate normal distribution
source("Packages.R")


# In this function the variables denote the following:
# p : number of components in theta
# n : numboer of observations
# h : number of constants not equal to zero in underlying theta, so 
#     choosing h = 0 is the null hypothesis
# nruns : number of replications
# significance :significance level
# sigma : covariance matrix of the errors
# diffscalars : is a boolean which can be set to the if you want to run the
#               simulations for the four different values of delta
# constant: constant in vector theta

BasicSimulationProbReject = function(p, n, h, nruns, significance = 0.05, 
                           Sigma = diag(rep(1, p)), diffscalars = F, 
                           constant = 0.05) {
  
  
  # Vector Theta is built up of h constants and p - h zeros 
  Theta = c(rep(constant, h), rep(0, p - h))
  
  if (diffscalars == T) {
    Rejectmatrix = matrix(0, nruns, 5)
    scalars = c(0.9, 1, 1.06, sqrt(1.5))
    
    deltapn = scalars * log(log(n)) * sqrt(log(p))
    iota = rep(1, n)
    
    # We now loop through the number of runs
    for (i in 1:nruns) {
      # First we generate n error terms for each component
      epsilon = rmvnorm(n, mean = rep(0, p), sigma = Sigma)
      # Data generating process
      X = iota %*% t(Theta) + epsilon
      thetahat = colMeans(X)
      J1 = n * t(thetahat) %*% thetahat
      
      critval = qchisq(1 - significance, p)
      
      if (J1 > critval) {
        Rejectmatrix[i, 1] = 1
      }
      
      for (j in 1:4) {
        # Now we generate J0
        # We let Shat be a boolean vector
        Shat = sqrt(n) * abs(thetahat) > deltapn[j]
        J0 = sqrt(p) * n * sum(thetahat[Shat]^2)

        
        if (J0 + J1 > critval) {
          Rejectmatrix[i, j + 1] = 1
        }
      }
    }
    
    rejectmeans = colMeans(Rejectmatrix)
  
    returnlist = list("Initial" = rejectmeans[1], 
                      "PEsq15" = rejectmeans[5],
                      "PE106" = rejectmeans[4],
                      "PE1" = rejectmeans[3],
                      "PE09" = rejectmeans[2])
    print(returnlist)
  
  } else {
  
  # We keep track of the rejections using the following vector
  # Per default, all values are zero but if test i rejects we set the ith
  # component equal to 1
  RejectVecInit = rep(0, nruns)
  RejectVecPE = rep(0, nruns)
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
    if (J0 + J1 > critval) {
      RejectVecPE[i] = 1
    }

  }
  returnlist = list("Initial" = mean(RejectVecInit), 
                    "PE" = mean(RejectVecPE))
  print(returnlist)
  }
}



pvec = c(100, 300, 500)
nvec = c(150, 250, 400)

Sys.time()
for (i in pvec) {
  for (j in nvec) {
    print(paste("p =", i, "and n =", j))
    BasicSimulationProbReject(i, j, 0.03 * i, 2000)
  }
}
Sys.time()



pvec = c(200, 500)
n = c(150, 250, 400)

Sys.time()
for (i in pvec) {
  for (j in nvec) {
    print(paste("p =", i, "and n =", j))
    BasicSimulationProbReject(i, j, 0.25 * i, 2000, diffscalars = T, constant = 0.04)
  }
}
Sys.time()

