### Plots of power for different constants ################################

source("Packages.R")
source("ThesisggTheme.R")

ggplot() +
  geom_line(aes(x = N, y = exactvaluepower05, color = "0.5"), 
            size = .7) +
  geom_line(aes(x = N, y = exactvaluepower1, color = "1"), 
            size = .7) +
  geom_line(aes(x = N, y = exactvaluepower2, color = "2"), 
            size = .7) +
  geom_line(aes(x = N, y = initialtest05, color = "0.5"), 
            size = .7) +
  geom_line(aes(x = N, y = initialtest1, color = "1"), 
            size = .7) +
  geom_line(aes(x = N, y = initialtest2, color = "2"), 
            size = .7) +
  scale_color_manual(name = "Scalar", 
                     values = c(
                       "0.5" = "#FF0000", 
                       "1" = "#0000FF", 
                       "2" =  "#00cd00")) +
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", 
                                  hjust = 0.001)) +
  labs(x = "n", y = "Size") + 
  ylim(0, .6) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  ThesisggTheme()





