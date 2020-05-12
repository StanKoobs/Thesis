##### Finding size distortion in simple case ##############################

source("Packages.R")

# Define the function over which we need to integrate
f1 = function(x, j, p, alpha) {
  pchisq(qchisq(1 - alpha, p) - x, p, lower.tail = F) * 
    dgamma(x, shape = j / 2, scale = 2 * sqrt(p))
}

# In this vector we save the values of the lower bound of the actual size
lowerbound = c()
upperbound = c()
dif_bound= c()

alpha = 0.05
p = 10
N = 1:100
deltapn =  log(log(N)) * sqrt(log(p))

for (i in seq_along(N)) {
  
  # removing cases where deltapn is negative
  if (deltapn[i] <= 0) {
    next
  }
  
  n = N[i]
  
  prob = 2 * pnorm(deltapn[i], lower.tail = F)
  
  
  # Computing the lower and upper bounds
  sum_lower = alpha * dbinom(0, p, prob)
  sum_upper = sum_lower
  
  for (j in 1:p) {
    # Splitting the integrals for slightly faster computation
    integral_part_1 = integrate(f1,
                                lower = j * deltapn[i]^2 * sqrt(p),
                                upper = Inf,
                                j = j, p = p, alpha = 0.05)$value
    
    #print(integral_part_1)
    # Binomial weight
    prob_binom = dbinom(j, p, prob)
    
    # Computing the lower and upper bound
    sum_upper = sum_upper +
      integral_part_1 *
      prob_binom /
      (1 - pgamma(j * sqrt(p) * deltapn[i]^2, shape = j / 2, scale = 2 * sqrt(p)))
  }
  
  lowerbound[i] = sum_lower + dbinom(1, p, prob) * 
    integrate(f1, 
              lower = deltapn[i]^2 * sqrt(p), 
              upper = Inf, 
              j = 1, p = p, alpha = 0.05)$value /
    (1 - pgamma(sqrt(p) * deltapn[i]^2, shape = 1 / 2, scale = 2 * sqrt(p)))
  
  upperbound[i] = sum_upper
  dif_bound[i] = upperbound[i] - lowerbound[i]
}

ggplot() + 
  geom_line(aes(x = N, y = lowerbound), size = .3) +
  geom_line(aes(x = N, y = upperbound), size = .3) + 
  geom_line(aes(x = N, y = dif_bound), size = .3, col = "red") +
  labs(x = "n", y = "size") + 
  ylim(0, .6)
