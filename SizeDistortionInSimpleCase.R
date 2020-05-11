##### Finding size distortion in simple case ##############################

source("Packages.R")

# Define the functions over which we need to integrate
f1 = function(x, p) {
  pchisq(critval - x, p, lower.tail = F) * dgamma(x, shape = p / 2, 
                                                  scale = 2 * sqrt(2))
}

# In this vector we save the values of the lower bound of the actual size
lowerbound = c()
upperbound = c()

alpha = 0.05
p = 2
N = 1:100
deltapn = log(log(N)) * sqrt(log(p))

for (i in seq_along(N)) {
  
  # removing cases where deltapn is negative
  if (deltapn[i] < 0) {
    next
  }
  
  n = N[i]
  
  # We use a significance level of 0.05
  critval = qchisq(1 - alpha, p)
  prob = 2 * pnorm(deltapn[i], lower.tail = F)
  

  # The actual lower bound is now given by
  sum_lower = alpha * dbinom(0, p, prob)
  sum_upper = sum_lower
  
  for (j in 1:p) {
    terms = choose(p, j)
    prob_binom = dbinom(j, p, prob)
    
    sum_lower = sum_lower + 
      terms * 
      integrate(f1, lower = j * deltapn[i], upper = Inf, p = j)$value * 
      prob_binom
    
    sum_upper = sum_upper + 
      terms * 
      integrate(f1, lower = 0, upper = Inf, p = j)$value * 
      prob_binom
  }

  lowerbound[i] = sum_lower
  upperbound[i] = sum_upper
}

ggplot() + 
  geom_line(aes(x = N, y = lowerbound), size = .1) +
  geom_line(aes(x = N, y = upperbound), size = .1) + 
  labs(x = "n", y = "size") + 
  ylim(0, .3)