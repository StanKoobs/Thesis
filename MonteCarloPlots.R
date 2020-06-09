### Script in which we plot the Monte Carlo approximations ################

# First compute the MC approximations using the SizeDistortionInSimpleCase
# script and call them accordingly

# First we only plot the exact and the Monte Carlo approximation for p = 2

source("ThesisggTheme.R")

ggplot() +
  geom_line(aes(x = N, y = exactValue2, color = "Exact"), size = .7) +
  geom_line(aes(x = N, y = mcApprox2, color = "Monte Carlo"),
            size = .7) +
  scale_color_manual(name = "Method",
    values = c("Exact" = "#FF0000", 
                                "Monte Carlo" = "#0000FF")) +
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", 
                                  hjust = 0.001)) + 
  geom_hline(yintercept = 0.05) +
  labs(x = "n", y = "Size") + 
  ylim(0, .6) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  ThesisggTheme()


# Now we plot the Monte Carlo approximations for different values of p

data = data.frame(N = rep(N, 3), MC = c(mcApprox2, mcApprox8, mcApprox15),
                   levels = rep(c("2", "8", "15"), each = 100))

data$levels = 
  factor(data$levels, levels = c("2", "8", "15"))

exactdata = data.frame(N = N, exactValue = exactValue2)

ggplot() + 
  geom_line(aes(x = N, y = exactValue), exactdata, size = .7) +
  geom_line(aes(x = N, y = MC, color = levels), data, size = .7) +
  scale_color_manual(name = "p", 
                     values = c("#FF0000", "#0000FF", "#00FF00")) +
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", 
                                  hjust = 0.001)) +
  geom_hline(yintercept = 0.05) +
  labs(x = "n", y = "Size") + 
  ylim(0, .6) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  ThesisggTheme()



