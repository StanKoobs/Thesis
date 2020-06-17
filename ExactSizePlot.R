### Plots of exact size ###################################################

# Exact value p = 2

nominal = rep(0.05, 100)

ggplot() +
  geom_line(aes(x = N, y = exactValue2, color = "Exact")) +
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", 
                                  hjust = 0.001)) +
  labs(x = "n", y = "Size") + 
  geom_line(aes(x = N, y = nominal, color = "Nominal")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_color_manual(name = "Size", 
                     values = c("Exact" = "#FF0000", 
                                "Nominal" = "#0000FF"))
  ThesisggTheme()


