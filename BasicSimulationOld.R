##### In this script we the first basic simulations can be done ###########

# We use the rmvnorm function from the mvtnorm package as random number 
# generator for the multivariate normal distribution
source("Packages.R")

p = 10
h = 10
significance = 0.05

BasicSimulationProbReject = function(p, n, h, significance, nruns, 
                           Sigma = diag(rep(1, p))) {
  # Vector Theta is built up of h one's and p - h zeros 
  Theta = c(rep(1, h), rep(0, p - h))
  
  # We keep track of the rejections using the following vector
  # Per default, all values are zero but if test i rejects we set the ith
  # component equal to 1
  RejectVecInit = rep(0, nruns)
  RejectVecPE = rep(0, nruns)
  Shatelem = rep(0, nruns)
  #prob1 = c()
  #prob2 = c()
  
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
    #if (Shatelem[i] == 1) {
    #  prob1 = c(prob1, RejectVecPE[i])
    #}
    
    #if (Shatelem[i] == 2) {
    #  prob2 = c(prob2, RejectVecPE[i])
    #}
  }
  returnlist = list("Initial" = mean(RejectVecInit), 
                    "PE" = mean(RejectVecPE))
  return(returnlist)
}

simvecp10 = c()
N = 1:100

for (i in N) {
  simvecp10[i] = BasicSimulationProbReject(10, i, 0, 0.05, 5000)$PE
}

ggplot() +
  geom_line(aes(x = N, y = term0, color = "Term 0"), 
            size = .7) +
  geom_line(aes(x = N, y = term1, color = "Term 1"), 
            size = .7) +
  geom_line(aes(x = N, y = term2, color = "Term 2"), 
            size = .7) +
  geom_line(aes(x = N, y = exactValue), size = .7) +
    scale_color_manual(name = "Method", 
                       values = c(
                         "Term 0" = "#FF0000", 
                         "Term 1" = "#0000FF", 
                         "Term 2" =  "#00cd00")) +
    theme(legend.key.size = unit(1.2, "cm"), 
          legend.text = element_text(size = 15, face = "bold"),
          legend.title = element_text(size = 15, face = "bold"),
          plot.title = element_text(size = 15, face = "bold", 
                                    hjust = 0.001)) +
    labs(x = "n", y = "Size") + 
    ylim(0, .6) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    ThesisggTheme()


ggplot() +
  geom_line(aes(x = N, y = simvecp10, color = "Simulations"), size = .7) +
  geom_line(aes(x = N, y = mcApprox, color = "MC integration"), size = .7) +
  scale_color_manual(name = "Method", 
                     values = c( 
                       "MC integration" = "#0000FF", 
                       "Simulations" =  "#00cd00")) +
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", 
                                  hjust = 0.001)) +
  labs(x = "n", y = "Size") + 
  geom_hline(yintercept = 0.05) +
  ylim(0, .6) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  ThesisggTheme()



