### Old script used to find size and power of theoretical analysis, this
### script still used the unconditional cdf

source("Packages.R")

# In this vector we save the values of the lower bound of the actual size
lowerBound = c()
upperBound = c()
difBound = c()
exactValue = c()
mcApprox = c()

# Adjust these booleans corresponding to which values you want to compute
computeLower = T
computeUpper = T
computeExact = F
computeMC = T

alpha = 0.05
p = 3
N = 1:100
deltapn = log(log(N)) * sqrt(log(p))

#Loop through all values of N
for (i in seq_along(N)) {
  
  # Removing cases where deltapn is negative
  if (deltapn[i] <= 0) {
    next
  }
  
  n = N[i]
  
  # Probability of a component being included in S hat
  prob = 2 * pnorm(deltapn[i], lower.tail = F)
  
  # The value corresponding to S hat being empty is known
  sumExact = alpha * dbinom(0, p, prob)
  sumLower = sumExact
  sumUpper = sumExact
  sumMC = sumExact
  
  # We now loop through the number of components which can be included in
  # S hat
  for (j in 1:p) {
    
    # Binomial weight
    probBinom = dbinom(j, p, prob)
    
    
    # Define function over which we need to integrate
    # In this function, x is expected to be a j-dimensional vector where j
    # is the number of elements in S hat
    f1 = function(x, j, p, alpha) {
      if (any(x < deltapn[i]^2)) {
        return(0) # Note that the joint density is zero here
      }
      # If we did not return zero (so we are in the domain), return this
      return(pchisq(qchisq(1 - alpha, p) - sqrt(p) * sum(x), p, lower.tail = F) * 
        prod(dchisq(x, df = 1)) / 
        pchisq(deltapn[i]^2, df = 1, lower.tail = F)^j)
    }
    
    if (computeExact == TRUE) {
      #Now we will apply multidimensional integration where the dimension 
      # is j. This will be done by using adaptIntegrate function from 
      # cubature package
      
      exactIntegral = adaptIntegrate(f1, 
                                     lowerLimit = rep(deltapn[i]^2, j),
                                     upperLimit = rep(Inf, j),
                                     j = j, p = p, alpha = 0.05)$integral
      
      sumExact = sumExact + exactIntegral * probBinom
    }
    
    if (computeMC == TRUE) {
      
      # Now we use Monte Carlo integration to approximate the 
      # multidimensional integral
      
      NMCSim = 30000
      mcIntegral = 0
      
      for (k in 1:NMCSim) {
        # Every sample needs to be of joint chi-square but since these are 
        # independent we can just use the marginal
        mcSample = rchisq(j, df = 1)
        
        mcIntegral = mcIntegral + 
          f1(mcSample, j = j, p = p, alpha = alpha) / 
          (NMCSim * prod(dchisq(mcSample, df = 1)))
        
      }
      sumMC = sumMC + mcIntegral * probBinom
    }
    
    if (computeLower == TRUE) {
      # We only need to compute something for the case where j <= 3
      # The terms after that are skipped
      if (j <= 3) {
        exactIntegral = adaptIntegrate(f1, 
                                       lowerLimit = rep(deltapn[i]^2, j),
                                       upperLimit = rep(Inf, j),
                                       j = j, p = p, alpha = 0.05)$integral
        
        sumLower = sumLower + exactIntegral * probBinom
      } 
    }
    
    if (computeUpper == TRUE) {
      # Compute it exactly for the case where j <= 3
      if (j <= 3) {
        exactIntegral = adaptIntegrate(f1, 
                                       lowerLimit = rep(deltapn[i]^2, j),
                                       upperLimit = rep(Inf, j),
                                       j = j, p = p, alpha = 0.05)$integral
        
        sumUpper = sumUpper + exactIntegral * probBinom
      } else {
      # When j > 3, use that the integral is bounded by 1  
        sumUpper= sumUpper + probBinom
      }
    }
    
    
  }
  
  exactValue[i] = sumExact
  mcApprox[i] = sumMC
  lowerBound[i] = sumLower
  upperBound[i] = sumUpper
  difBound[i] = upperBound[i] - lowerBound[i]
  print(n)
  
}


