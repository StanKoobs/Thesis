### Detailed plot case 2 of the lower bound

source("Packages.R")
source("Thesisggtheme.R")

options(tz = "Europe/Berlin")

tikz(file = "Case2LowerBoundPlot.tex", width = 4.6, height = 4)


newbaseplot = ggplot() +
  geom_rect(aes(xmin = 0, xmax = 2, ymin = 0, ymax = 2), 
            alpha = 0, color = "black") +
  scale_x_continuous(breaks = c(0, 1, 2), 
                     labels = c(0, "$C_{\\alpha} - \\delta_{p,n}^2$", "$\\delta_{p,n}^2$"), 
                     limits = c(-0.3, 3.3)) +
  scale_y_continuous(breaks = c(0, 1, 2), 
                     labels = c(0, "$C_{\\alpha} - \\delta_{p,n}^2$", "$\\delta_{p,n}^2$"),
                     limits = c(-0.3, 3.1)) +
  xlab("$n \\widehat{\\theta}_1$") +
  ylab("$n \\widehat{\\theta}_2$") +
  #geom_hline(yintercept = 0) +
  #geom_vline(xintercept = 0) +
  ThesisggTheme() +
  theme(panel.grid.minor = element_blank())

case2 = newbaseplot +
  geom_polygon(aes(x = c(1,2,2), y = c(2,2,1)), alpha = 0.3, fill = "red") +
  geom_abline(intercept = 3, slope = -1, linetype = "longdash", label = 3) +
  annotate("text", x = 2.85, y = 0.5, size = 3.5,
           label = "$n \\widehat{\\theta}^2_1$ + $n \\widehat{\\theta}^2_2 = C_{\\alpha}$", 
           angle = -45)

case2 +
  geom_polygon(aes(x = c(0, 0, 1), y = c(2, 3, 2)), 
               alpha = 0.3, fill = "blue") +
  geom_polygon(aes(x = c(2, 3, 2), y = c(1, 0, 0)), 
               alpha = 0.3, fill = "blue") +
  geom_line(aes(x = c(-Inf, 2), y = c(1, 1)), linetype = "dotted") +
  geom_line(aes(x = c(1, 1), y = c(-Inf, 2)), linetype = "dotted") +
  annotate("text", x = 1.5, y = 1.48, size = 14,
           label = "D")


dev.off()
