### Numerical verification of results plots ###############################

ggplot() +
  geom_line(aes(x = N, y = simvecp10, color = "Simulations"), size = .7) +
  geom_line(aes(x = N, y = mcApprox10, color = "MC integration"), size = .7) +
  geom_line(aes(x = N, y = upperBound10, color = "Upper bound"), size = .7) +
  geom_line(aes(x = N, y = lowerBound10, color = "Lower bound"), size = .7) +
  scale_color_manual(name = "Method", 
                     values = c( 
                       "MC integration" = "#FF0000", 
                       "Simulations" =  "#0000FF",
                       "Upper bound" = "#00cd00",
                       "Lower bound" = "violet")) +
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", 
                                  hjust = 0.001)) +
  labs(x = "n", y = "Size") + 
  geom_hline(yintercept = 0.05) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), limits = c(0,1)) +
  ThesisggTheme()


ggplot() +
  geom_line(aes(x = N, y = simvecp200, color = "Simulations"), size = .7) +
  geom_line(aes(x = N, y = upperBound1, color = "Upper bound"), size = .7) +
  geom_line(aes(x = N, y = lowerBound, color = "Lower bound"), size = .7) +
  scale_color_manual(name = "Method", 
                     values = c( 
                       "Simulations" =  "#0000FF",
                       "Upper bound" = "#00cd00",
                       "Lower bound" = "#FF0000")) +
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", 
                                  hjust = 0.001)) +
  labs(x = "n", y = "Size") + 
  geom_hline(yintercept = 0.05) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), limits = c(0, 1)) +
  ThesisggTheme()





