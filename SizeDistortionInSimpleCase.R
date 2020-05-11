##### Finding size distortion in simple case ##############################

# In this vector we save the values of the lower bound of the actual size
lowerbounds = c()

for (n in 1:100) {
  deltapn = log10(log10(n)) * sqrt(log10(2))
  # We use a significance level of 0.05
  critval = qchisq(0.95, 2)
  
  # First we determine the distribution of S hat
  ProbSempty = (1 - 2 * pnorm(deltapn, lower.tail = F))^2
  ProbS1 = 2 * pnorm(deltapn, lower.tail = F) * 
    (1 - 2 * pnorm(deltapn, lower.tail = F)) 
  ProbS2 = ProbS1
  ProbS12 = (2 * pnorm(deltapn, lower.tail = F))^2
  
  #Now we define the functions over which we need to integrate
  f1 = function(x) {
    pchisq(critval - x, 2, lower.tail = F) * dgamma(x, shape = 1 / 2, 
                                                    scale = 2 * sqrt(2))
  }
  
  f12 = function(x) {
    pchisq(critval - x, 2, lower.tail = F) * dgamma(x, shape = 1, 
                                                    scale = 2 * sqrt(2))
  }
  
  # The actual lower bound is now given by
  lowerbound = 0.05 * ProbSempty +
    integrate(f1, lower = 0, upper = Inf)$value * ProbS1 * 2 +
    integrate(f12, lower = 0, upper = Inf)$value * ProbS12
  
  lowerbounds = c(lowerbounds, lowerbound)
}

source("Packages.R")

ggplot() + geom_point(aes(x = 1:100, y = lowerbounds)) +
  labs(x = "n", y = "Lower bound of actual size")




