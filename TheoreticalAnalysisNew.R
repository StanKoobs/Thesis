##### Finding size distortion in simple case (new) ########################

source("Packages.R")
source("LowerBoundGenerator.R")

# In this vector we save the values of the lower bound of the actual size
lowerBound = c()
upperBound1 = c()
upperBound2 = c()
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
computeLower = F
computeUpper1 = F
computeUpper2 = F
computeExact = T
computeMC = F
computeInitial = T

# We let the underlying parameter vector be a scalar multiple of iota_p
# Choose zero for the scalar if you want to analyze the size of the test
# Choose a nonzero value to analyze power
scalar = 1

alpha = 0.05
p = 2
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
  prob = pchisq(deltapn[i]^2, df = 1, ncp = scalar^2, lower.tail = F)
  
  # The value corresponding to S hat being empty is known
  sumExact = 0 
  sumLower = 0
  sumUpper1 = 0
  sumUpper2 = 0
  sumMC = 0
  
  # We now loop through the number of components which can be included in
  # S hat
  for (j in 0:p) {
    
    # Binomial weight
    probBinom = dbinom(j, p, prob)
    
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
      
      NMCSim = 30000
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
      
      if (j == p) {
        exactIntegral = ifelse((1 + sqrt(p)) * p * deltapn[i]^2 > qchisq(1 - alpha, p),
                               1, 0)
      } else if (qchisq(1 - alpha, p) -  (1 + sqrt(p)) * j * deltapn[i]^2 > (p - j) * deltapn[i]^2) {
          exactIntegral = 0
      } else if (j == 0) {
          exactIntegral = lowerboundgenerator(p, n, alpha, 4000, 0, j) 
      } else {
          #if (pchisq(deltapn[i]^2, df = 1, ncp = scalar^2)^(p - j) == 0) {
          #  exactIntegral = 0
          #} else {
            exactIntegral = 1 - pchisq(qchisq(1 - alpha, p) -  (1 + sqrt(p)) * j * deltapn[i]^2,
                                       df = p - j, ncp = (p - j) * scalar^2) / 
              pchisq(deltapn[i]^2, df = 1, ncp = scalar^2)^(p - j)
          #}
        
          #if (exactIntegral < 0) {
          #  exactIntegral = 0
          #} 
      }
          
      sumLower = sumLower + exactIntegral * probBinom  
      
        
      if (j == 0) {
        exactIntegral0[i] = exactIntegral 
      } else if (j == 1) {
        exactIntegral1[i] = exactIntegral 
      } else if (j == 2) {
        exactIntegral2[i] = exactIntegral 
      }
        
    }

     
    if (computeUpper1 == TRUE) {
      
      upperf1 = function(x, j, p, alpha) {
        pchisq(qchisq(1 - alpha, p) - (1 + sqrt(p)) * sum(x), 
               df = p - j,
               ncp = (p - j) * scalar^2, 
               lower.tail = F) *
          prod(dchisq(x, df = 1, ncp = scalar^2)) /
          pchisq(deltapn[i]^2, df = 1, ncp = scalar^2, lower.tail = F)^j
      }
      
      if (j == 0) {
        exactIntegral = pchisq(qchisq(1 - alpha, p), 
                               df = p,
                               ncp = p * scalar^2, 
                               lower.tail = F)
        sumUpper1 = sumUpper1 + exactIntegral * probBinom
      }
      
      # Compute it exactly for the case where j <= 3
      else if (j == 1 | j == 2) {
        exactIntegral = adaptIntegrate(upperf1, 
                                       lowerLimit = rep(deltapn[i]^2, j),
                                       upperLimit = rep(Inf, j),
                                       j = j, p = p, alpha = 0.05)$integral
        
        sumUpper1 = sumUpper1 + exactIntegral * probBinom
      } else {
        # When j > 2, use that the integral is bounded by 1  
        sumUpper1 = sumUpper1 + probBinom
      }
    }
    
    if (computeUpper2 == TRUE) {
      
      upperf2 = function(x, j, p, alpha) {
        ifelse((1 + sqrt(p)) * sum(x) > 
                 qchisq(1 - alpha, p) - (p - j) * deltapn[i]^2,
               1, 
               0) *
          prod(dchisq(x, df = 1, ncp = scalar^2)) /
          pchisq(deltapn[i]^2, df = 1, ncp = scalar^2, lower.tail = F)^j
      }
      
      if (j == 0) {
        exactIntegral = pchisq(qchisq(1 - alpha, p), 
                               df = p,
                               ncp = p * scalar^2, 
                               lower.tail = F)
        sumUpper2 = sumUpper2 + exactIntegral * probBinom
      }
      
      # Compute it exactly for the case where j <= 3
      else if (j == 1 | j == 2) {
        exactIntegral = adaptIntegrate(upperf2, 
                                       lowerLimit = rep(deltapn[i]^2, j),
                                       upperLimit = rep(Inf, j),
                                       j = j, p = p, alpha = 0.05)$integral
        
        sumUpper2 = sumUpper2 + exactIntegral * probBinom
      } else {
        # When j > 2, use that the integral is bounded by 1  
        sumUpper2 = sumUpper2 + probBinom
      }
    
    }
    
  }  
  
  exactValue[i] = sumExact
  mcApprox[i] = sumMC
  lowerBound[i] = sumLower
  upperBound1[i] = sumUpper1
  upperBound2[i] = sumUpper2
  print(n)
  
}

Sys.time()


ggplot() +
  geom_line(aes(x = N, y = exactValue)) + 
  geom_line(aes(x = N, y = term0)) +
  geom_line(aes(x = N, y = term1)) +
  geom_line(aes(x = N, y = term2))


ggplot() +
  geom_line(aes(x = N, y = lowerBound)) + 
  geom_line(aes(x = N, y = upperBound1)) +
  geom_hline(yintercept = 0.05) +
  ylim(0,1)



qchisq(1 - alpha, p) - (1 + sqrt(10)) * 2 * deltapn[3]^2



0.5 * (pchisq(deltapn[i]^2, 1) - pchisq(qchisq(1 - alpha, p) - 
                                            (1 + sqrt(p)) * deltapn[i]^2 - deltapn[i]^2, 1))^2

