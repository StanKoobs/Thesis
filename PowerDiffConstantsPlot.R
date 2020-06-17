### Plots of power for different constants ################################

source("Packages.R")
source("ThesisggTheme.R")

df = data.frame(N = rep(N, 4),
                y = c(exactValue2, exactValue2alt, initialtest2, initialtestalt), 
                Type = rep(c("Size", "Power"), each = length(N), times = 2),
                Test = rep(c("PE", "Initial"), each = 2 * length(N)))

df$Type = factor(df$Type, levels = c("Size", "Power"))
df$Test = factor(df$Test, levels = c("PE", "Initial"))

ggplot(data = df) +
  geom_line(aes(x = N, y = y, color = Type, linetype = Test), size = .7) +
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", 
                                  hjust = 0.001)) +
  labs(x = "n", y = "") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), 
                     limits = c(0, .8)) +
  ThesisggTheme() +
  scale_linetype_manual(values = c(PE = "solid", Initial = "longdash")) +
  scale_color_manual(values = c(Size = "#FF0000", Power = "#0000FF"))

ggplot() +
  geom_line(aes(x = N, y = exactValue2, color = "Size PE"), 
            size = .7, linetype = "longdash") +
  geom_line(aes(x = N, y = exactValue2alt, color = "Power PE"), 
            size = .7, linetype = "longdash") +
  geom_line(aes(x = N, y = initialtest2, color = "Initial size"), 
            size = .7) +
  geom_line(aes(x = N, y = initialtestalt, color = "Initial power"), 
            size = .7) +
  scale_color_manual(name = "Type", 
                     values = c(
                       "Size PE" = "#FF0000", 
                       "Initial size" = "#FF0000", 
                       "Power PE" = "#0000FF",
                       "Initial power" = "#0000FF")) + 
  scale_linetype_manual(name = "Type",
                        values = c(
                          "Size PE" = "longdash", 
                          "Initial size" = "longdash", 
                          "Power PE" = "solid",
                          "Initial power" = "solid")) +
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", 
                                  hjust = 0.001)) +
  labs(x = "n", y = "Size") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), 
                     limits = c(0, 1)) +
  ThesisggTheme()





