### Numerical verification of results plots ###############################

ggplot() +
  geom_line(aes(x = N, y = simvecp10, color = "Simulations"), size = .7) +
  geom_line(aes(x = N, y = mcApprox, color = "MC integration"), size = .7) +
  geom_line(aes(x = N, y = upperBound1, color = "Upper bound"), size = .7) +
  scale_color_manual(name = "Method", 
                     values = c( 
                       "MC integration" = "#FF0000", 
                       "Simulations" =  "#0000FF",
                       "Upper bound" = "#00cd00")) +
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


ggplot() +
  geom_line(aes(x = N, y = simvecp200, color = "Simulations"), size = .7) +
  geom_line(aes(x = N, y = upperBound1, color = "Upper bound"), size = .7) +
  scale_color_manual(name = "Method", 
                     values = c( 
                       "Simulations" =  "#0000FF",
                       "Upper bound" = "#00cd00")) +
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





