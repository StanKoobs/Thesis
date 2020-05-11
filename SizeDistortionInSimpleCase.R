##### Finding size distortion in simple case ##############################

source("Packages.R")

# Define the functions over which we need to integrate
f1 = function(x, p, alpha) {
  pchisq(qchisq(1 - alpha, p) - x, p, lower.tail = F) * dgamma(x, shape = p / 2, 
                                                  scale = 2 * sqrt(2))
}

# In this vector we save the values of the lower bound of the actual size
lowerbound = c()
upperbound = c()
dif_bound= c()

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
  
  prob = 2 * pnorm(deltapn[i], lower.tail = F)
  

  # The actual lower bound is now given by
  sum_lower = alpha * dbinom(0, p, prob)
  sum_upper = sum_lower
  
  for (j in 1:p) {
    terms = choose(p, j)
    prob_binom = dbinom(j, p, prob)
    
    # Splitting the integrals for slightly faster computation
    integral_part_1 = integrate(f1, 
                                lower = j * deltapn[i], 
                                upper = Inf, 
                                p = j, alpha = 0.05)$value 
    
    integral_part_2 = integrate(f1, 
                                lower = 0, 
                                upper = j * deltapn[i], 
                                p = j, alpha = 0.05)$value
    
    sum_lower = sum_lower + 
      terms * 
      integral_part_1 * 
      prob_binom
    
    sum_upper = sum_upper + 
      terms * 
      (integral_part_1 + integral_part_2) * 
      prob_binom
  }

  lowerbound[i] = sum_lower
  upperbound[i] = sum_upper
  dif_bound[i] = sum_upper - sum_lower
}

ggplot() + 
  geom_line(aes(x = N, y = lowerbound), size = .1) +
  geom_line(aes(x = N, y = upperbound), size = .1) + 
  geom_line(aes(x = N, y = dif_bound), size = .1, col = "red") +
  labs(x = "n", y = "size") + 
  ylim(0, .3)
