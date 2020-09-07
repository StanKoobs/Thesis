##### Computing the power in a simple case #####################################

source("Packages.R")
source("LowerBoundGenSparse.R")

# In this vector we save the values of the lower bound of the actual size
lowerBound = c()
upperBound = c()
exactValue = c()
mcApprox = c()
initialtest = c()

term0 = c()
term1 = c()
term2 = c()

exactIntegral0 = c()
exactIntegral1 = c()
exactIntegral2 = c()

# Adjust these booleans corresponding to which values you want to compute
computeLower = T
computeUpper = T
computeExact = F
computeMC = F
computeInitial = F

# We let the underlying parameter vector be a sparse vector with only the first
# unequal to zero
# scalar is the value of this constant unequal to zero
scalar = 1

alpha = 0.05
p = 10
N = 1:100
deltapn = log(log(N)) * sqrt(log(p))

Sys.time()

#Loop through all values of N
for (i in seq_along(N)) {
  
  # Removing cases where deltapn is negative
  if (deltapn[i] <= 0) {
    next
  }
  
  n = N[i]
  
  # Compute probability of rejection of initial test
  if (computeInitial == TRUE) {
    
    initialtest[i] = pchisq(qchisq(1 - alpha, p), p, ncp = p * scalar^2, 
                            lower.tail = FALSE)
  }
  
  
  # Probability of a component being included in S hat
  prob = pchisq(deltapn[i]^2, df = 1, lower.tail = F)
  
  # The value corresponding to S hat being empty is known
  sumExact = 0 
  sumLower = 0
  sumUpper = 0
  sumMC = 0
  
  # We now loop through the number of components which can be included in
  # S hat
  for (j in 0:(p - 1)) {
    
    # Binomial weight
    probBinomFirstOut = dbinom(j, p - 1, prob)
    probBinomFirstIn = dbinom(j - 1, p - 1, prob) # Returns zero when j = 0
    probfirstinShat = pchisq(deltapn[i]^2, 1, ncp = n * scalar^2, lower.tail = F)
    
    # Define function over which we need to integrate
    # In this function, x is expected to be a p-dimensional vector and j
    # is the number of elements in S hat
    f1 = function(x, j, p, alpha) {
      inShat = c(rep(TRUE, j), rep(FALSE, p - j))
      notShat = c(rep(FALSE, j), rep(TRUE, p - j))
      if (any(x[inShat] < deltapn[i]^2)) {
        return(0) # Note that the joint density is zero here
      }
      if (any(x[notShat] > deltapn[i]^2)) {
        return(0)
      }
      # If we did not return zero (so we are in the domain), return this
      return(ifelse(
        sum(x) > qchisq(1 - alpha, p) - sqrt(p) * sum(x[inShat]),
        1, 0) *
          prod(dchisq(x, df = 1, ncp = scalar^2)) / 
          (pchisq(deltapn[i]^2, df = 1, ncp = scalar^2, lower.tail = F)^j * 
             pchisq(deltapn[i]^2, df = 1, ncp = scalar^2)^(p - j)))
    }
    
    
    
    
    if (computeExact == TRUE) {
      #Now we will apply multidimensional integration where the dimension 
      # is j. This will be done by using adaptIntegrate function from 
      # cubature package
      
      exactIntegral = adaptIntegrate(f1, 
                                     lowerLimit = 
                                       c(rep(deltapn[i]^2, j), rep(0, p - j)),
                                     upperLimit = 
                                       c(rep(Inf, j), rep(deltapn[i]^2, p - j)),
                                     j = j, p = p, alpha = 0.05)$integral
      
      sumExact = sumExact + exactIntegral * probBinom
      
      if (j == 0) {
        term0[i] = exactIntegral * probBinom
      } else if (j == 1) {
        term1[i] = exactIntegral * probBinom
      } else if (j == 2) {
        term2[i] = exactIntegral * probBinom
      }
      
    }
    
    if (computeMC == TRUE) {
      
      # Now we use Monte Carlo integration to approximate the 
      # multidimensional integral
      
      NMCSim = 50000
      mcIntegral = 0
      
      for (k in 1:NMCSim) {
        # Every sample needs to be of joint chi-square but since these are 
        # independent we can just use the marginal
        mcSample = rchisq(p, df = 1, ncp = scalar^2)
        
        mcIntegral = mcIntegral + 
          f1(mcSample, j = j, p = p, alpha = alpha) / 
          (NMCSim * prod(dchisq(mcSample, df = 1, ncp = scalar^2)))
        
      }
      sumMC = sumMC + mcIntegral * probBinom
      
      if (j == 0) {
        term0[i] = mcIntegral * probBinom
      } else if (j == 1) {
        term1[i] = mcIntegral * probBinom
      } else if (j == 2) {
        term2[i] = mcIntegral * probBinom
      }
    }
    
    if (computeLower == TRUE) {
      
      if (j > 10) {
        next
      }
      
      # In this case, both conditional probabilities are outside of the box
      if (qchisq(1 - alpha, p) -  (1 + sqrt(p)) * (j + 1) * deltapn[i]^2 > (p - j - 1) * deltapn[i]^2) {
        exactIntegral1 = 0
        exactIntegral2 = 0
      # It also might be the case that only the first case is outside of the box
      } else if (qchisq(1 - alpha, p) -  (1 + sqrt(p)) * j * deltapn[i]^2 > (p - j) * deltapn[i]^2) {
        exactIntegral1 = 0
        exactIntegral2 = 1 - pchisq(qchisq(1 - alpha, p) -  (1 + sqrt(p)) * (j + 1) * deltapn[i]^2,
                                    df = p - j - 1) / 
          pchisq(deltapn[i]^2, df = 1)^(p - j - 1)
      # If j == 0, we will approximate the probability of the first integral
      } else if (j == 0) {
        exactIntegral1 = lowerboundgeneratorsparse(p, n, alpha, 2000, scalar, j, 1)
        exactIntegral2 = 1 - pchisq(qchisq(1 - alpha, p) -  (1 + sqrt(p)) * (j + 1) * deltapn[i]^2,
                                    df = p - j - 1) / 
          pchisq(deltapn[i]^2, df = 1)^(p - j - 1)
      } else if (j == (p - 1)) {
        exactIntegral1 = 1 - pchisq(qchisq(1 - alpha, p) - (1 + sqrt(p)) * j * deltapn[i]^2,
                                    df = p - j, ncp = n * scalar^2) /
          (pchisq(deltapn[i]^2, df = 1)^(p - j - 1) * 
             pchisq(deltapn[i]^2, df = 1, ncp = n * scalar^2))
        exactIntegral2 = ifelse((1 + sqrt(p)) * p * deltapn[i]^2 > qchisq(1 - alpha, p), 1, 0)
      } else {
        exactIntegral1 = 1 - pchisq(qchisq(1 - alpha, p) - (1 + sqrt(p)) * j * deltapn[i]^2,
                                    df = p - j, ncp = n * scalar^2) /
          (pchisq(deltapn[i]^2, df = 1)^(p - j - 1) * 
             pchisq(deltapn[i]^2, df = 1, ncp = n * scalar^2))
        exactIntegral2 = 1 - pchisq(qchisq(1 - alpha, p) -  (1 + sqrt(p)) * (j + 1) * deltapn[i]^2,
                                    df = p - j - 1) / 
          pchisq(deltapn[i]^2, df = 1)^(p - j - 1)
      }
      
      sumLower = sumLower + exactIntegral1 * probBinomFirstOut * (1 - probfirstinShat)
      sumLower = sumLower + exactIntegral2 * probBinomFirstIn * probfirstinShat
      
      
    }
    
    
    if (computeUpper == TRUE) {
      
      upperf1 = function(x, j, p, alpha) {
        pchisq(qchisq(1 - alpha, p) - (1 + sqrt(p)) * sum(x), 
               df = p - j,
               ncp = n * scalar^2, 
               lower.tail = F) *
          prod(dchisq(x, df = 1))
      }
      
      upperf2 = function(x, j, p, alpha) {
        pchisq(qchisq(1 - alpha, p) - (1 + sqrt(p)) * sum(x), 
               df = p - j - 1,
               lower.tail = F) *
          prod(c(dchisq(x[1], df = 1, ncp = n * scalar^2), dchisq(x[-1], df = 1)))
      }
      
      if (j == 0) {
        exactIntegral1 = pchisq(qchisq(1 - alpha, p), 
                               df = p,
                               ncp = n * scalar^2, 
                               lower.tail = F)
        exactIntegral2 = adaptIntegrate(upperf2, 
                                        lowerLimit = rep(deltapn[i]^2, j + 1),
                                        upperLimit = rep(Inf, j + 1),
                                        j = j, p = p, alpha = 0.05)$integral
        
        sumUpper = sumUpper + exactIntegral1 * probBinomFirstOut * (1 - probfirstinShat)
        sumUpper = sumUpper + exactIntegral2 * probBinomFirstIn * probfirstinShat /  
          pchisq(deltapn[i]^2, df = 1, ncp = n * scalar^2, lower.tail = F)
      }
      
      # Compute it exactly for the case where j <= 2
      else if (j == 1 | j == 2) {
        exactIntegral1 = adaptIntegrate(upperf1, 
                                       lowerLimit = rep(deltapn[i]^2, j),
                                       upperLimit = rep(Inf, j),
                                       j = j, p = p, alpha = 0.05)$integral
        
        exactIntegral2 = adaptIntegrate(upperf2, 
                                        lowerLimit = rep(deltapn[i]^2, j + 1),
                                        upperLimit = rep(Inf, j + 1),
                                        j = j, p = p, alpha = 0.05)$integral
        
        sumUpper = sumUpper + exactIntegral1 * probBinomFirstOut * 
          (1 - probfirstinShat) / pchisq(deltapn[i]^2, df = 1, lower.tail = F)^j
        sumUpper = sumUpper + exactIntegral2 * probBinomFirstIn * probfirstinShat /
          (pchisq(deltapn[i]^2, df = 1, ncp = n * scalar^2, lower.tail = F) * 
             pchisq(deltapn[i]^2, df = 1, lower.tail = F)^j)
      } else {
        # When j > 2, use that the integral is bounded by 1  
        sumUpper = sumUpper + probBinomFirstOut * (1 - probfirstinShat) +
          probBinomFirstIn * probfirstinShat
      }
    }
    
  }  
  
  exactValue[i] = sumExact
  mcApprox[i] = sumMC
  lowerBound[i] = sumLower
  upperBound[i] = sumUpper
  print(n)
  
}

Sys.time()


ggplot() +
  geom_line(aes(x = N, y = upperb)) + 
  geom_line(aes(x = N, y = term0)) +
  geom_line(aes(x = N, y = term1)) +
  geom_line(aes(x = N, y = term2))


ggplot() +
  geom_line(aes(x = N, y = upperBound)) +
  geom_line(aes(x = N, y = lowerBound)) +
  #geom_line(aes(x = N, y = simvec0p1)) +
  #geom_line(aes(x = N, y = oldUpper)) +
  #geom_line(aes(x = N, y = oldLower)) +
  ylim(0, 1)




qchisq(1 - alpha, p) - (1 + sqrt(10)) * 2 * deltapn[3]^2



0.5 * (pchisq(deltapn[i]^2, 1) - pchisq(qchisq(1 - alpha, p) - 
                                          (1 + sqrt(p)) * deltapn[i]^2 - deltapn[i]^2, 1))^2


for (i in 1:10) {
  print(i)
}


simvec0p1 = c()
for (i in 1:100) {
  simvec0p1[i] = BasicSimulationProbReject(10, i, 1, 10000, constant = 0.1)$PE
}





