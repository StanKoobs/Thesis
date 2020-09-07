### Compute the probability of rejection of the Kock & Preinerstorfer test
# This can be used to compute the size and power of the test

source("Packages.R")
source("LowerBoundGenerator.R")
source("LowerBoundGenSparse.R")
source("LowerBoundGeneratorGen.R")

InitialTest = c()
upperBoundKP = c()
ApproxKP = c()

computeInitial = TRUE
computeUpper = TRUE
computeApprox = TRUE

# Change the underlying vector theta here
theta = rep(0, 100)

alpha = 0.05
p = length(theta)
N = 1:100
deltapn = log(log(N)) * sqrt(log(p))

Sys.time()

for (i in seq_along(N)) {
  
  # Removing cases where deltapn is negative
  if (deltapn[i] <= 0) {
    next
  }
  
  n = N[i]
  
  
  # Compute probability of rejection of initial test
  if (computeInitial == TRUE) {
    
    InitialTest[i] = pchisq(qchisq(1 - alpha, p), p, ncp = n * sum(theta^2), 
                            lower.tail = FALSE)
  }
  
  # Vector with probabilities of being included in Shat
  probvec = pchisq(deltapn[i]^2, df = 1, ncp = n * theta^2, lower.tail = F)
  
  pShatempty = prod(1 - probvec)
  
  if (computeUpper == TRUE) {
    upperBoundKP[i] = 1 - pShatempty * (1 - alpha)
  }
  
  if (computeApprox == TRUE) {
    if (all(theta == 0)) {
      probJ1 = lowerboundgenerator(p, n, alpha, 1000, 0, 0)
    }
    else if (sum(theta == 0) == p - 1) {
      probJ1 = lowerboundgeneratorsparse(p, n, alpha, 1000, theta[1], 0, 1)
    }
    else {
      probJ1 = lowerboundgeneratorgeneral(p, n, alpha, 1000, theta)    
    }
  }
  
  ApproxKP[i] = 1 - pShatempty * (1 - probJ1)

  print(i)
}

Sys.time()







