### In this script we plot the upper bounds of the size

source("Packages.R")
source("ThesisggTheme.R")

df = data.frame(N = rep(N, 2),
                mcApproxs = c(mcApprox5, mcApprox10),
                upperBounds = c(upperBound15, upperBound110), 
                levels = rep(c("5", "10"), each = 100))

df$levels = factor(df$levels, levels = c("5", "10"))

ggplot(df) +
  geom_line(aes(x = N, y = mcApproxs, color = levels), size = .7) +
  geom_line(aes(x = N, y = upperBounds, color = levels), size = .7) +
  scale_color_manual(name = "p", 
                     values = c(
                       "5" = "#FF0000", 
                       "10" = "#0000FF")) +
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
  geom_line(aes(x = N, y = mcApprox10), size = .7) +
  geom_line(aes(x = N, y = upperBound110), size = .7) +
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




