### Plot of decompositon of effects #######################################

source("Packages.R")
source("ThesisggTheme.R")

# Plot of exact decomposition for p = 2

ggplot() +
  geom_line(aes(x = N, y = term0, color = "Term 0"), 
            size = .7) +
  geom_line(aes(x = N, y = term1, color = "Term 1"), 
            size = .7) +
  geom_line(aes(x = N, y = term2, color = "Term 2"), 
            size = .7) +
  geom_line(aes(x = N, y = exactValue2), size = .7) +
  geom_line(aes(x = N, y = lowerBound), size = .7) +
  scale_color_manual(name = "Effect", 
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

# Plot of MC decomposition for p = 10

ggplot() +
  geom_line(aes(x = N, y = term0, color = "Term 0"), 
            size = .7) +
  geom_line(aes(x = N, y = term1, color = "Term 1"), 
            size = .7) +
  geom_line(aes(x = N, y = term2, color = "Term 2"), 
            size = .7) +
  geom_line(aes(x = N, y = mcApprox), size = .7) +
  scale_color_manual(name = "Effect", 
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


