### Script in which we plot the probability that there is no size 
### distortion for different values of n

source("Packages.R")
source("ThesisggTheme.R")

# First we define the integrand on which we can apply the integrate 
# function
errorfuncintegrand = function(x) {
  (2 / sqrt(pi)) * exp(-x^2)
}

# This function computes the desired probability, it expects a vector of 
# parameters and a single value of n
probShatempty = function(pvec, n) {
  outcome = c()
  for (p in pvec) {
    outcome = c(outcome, (integrate(errorfuncintegrand, 
                                    0,  
                                    sqrt(log(p) / 2) * 
                                      log(log(n)))$value)^p)    
  }
  return(outcome)
}


probShat1 = function(pvec, n) {
  outcome = c()
  for (p in pvec) {
    integral = integrate(errorfuncintegrand,0,  
                         sqrt(log(p) / 2) * log(log(n)))$value
    outcome = c(outcome, p * (1 - integral) * integral^(p - 1))
  }
  return(outcome)
}

# We will compute the probability for these values of p
# Note that p is in practice an integer but here we use to it plot a smooth
# line
pvec = seq(1, 100, by = 0.01)

df = data.frame(pvec = rep(pvec, 3),
                probline = c(probShatempty(pvec, 10), 
                             probShatempty(pvec, 30), 
                             probShatempty(pvec, 100)),
                levels = rep(c("10", "30", "100"), each = length(pvec)))

df$levels = factor(df$levels, levels = c("10", "30", "100"))

ggplot(df) +              
  geom_line(aes(x = pvec, y = probline, 
                color = levels)) +
  labs(x = "p", y = "Probability") + 
  scale_color_manual(name = "n", 
                     values = c("10" = "#FF0000", 
                                "30" = "#0000FF", 
                                "100" = "#00cd00")) + 
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", 
                                  hjust = 0.001)) +
  ThesisggTheme()



# Prob Shat 1

# We will compute the probability for these values of p
# Note that p is in practice an integer but here we use to it plot a smooth
# line
pvec = seq(1, 100, by = 0.01)

df = data.frame(pvec = rep(pvec, 3),
                probline = c(probShat1(pvec, 10), 
                             probShat1(pvec, 30), 
                             probShat1(pvec, 100)),
                levels = rep(c("10", "30", "100"), each = length(pvec)))

df$levels = factor(df$levels, levels = c("10", "30", "100"))

ggplot(df) +              
  geom_line(aes(x = pvec, y = probline, 
                color = levels)) +
  labs(x = "p", y = "Probability") + 
  scale_color_manual(name = "n", 
                     values = c("10" = "#FF0000", 
                                "30" = "#0000FF", 
                                "100" = "#00cd00")) + 
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", 
                                  hjust = 0.001)) +
  ThesisggTheme()



