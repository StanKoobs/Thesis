### In this script we generate the plots which are shown to support the 
### derivation of the lower bound

source("Packages.R")
source("Thesisggtheme.R")

options(tz = "Europe/Berlin")

tikz(file = "LowerBoundCasesPlot.tex", width = 6, height = 6)

baseplot = ggplot() +
  geom_rect(aes(xmin = 0, xmax = 2, ymin = 0, ymax = 2), 
            alpha = 0, color = "black") +
  scale_x_continuous(breaks = c(0, 2), 
                     labels = c(0, "$ \\delta_{p,n}^2$"), 
                     limits = c(-0.7, 3.5)) +
  scale_y_continuous(breaks = c(0, 2), 
                     labels = c(0, "$\\delta_{p,n}^2$"),
                     limits = c(-1.05, 3.1)) +
  xlab("$n \\widehat{\\theta}_1^2$") +
  ylab("$n \\widehat{\\theta}_2^2$") +
  #geom_hline(yintercept = 0) +
  #geom_vline(xintercept = 0) +
  ThesisggTheme() +
  theme(panel.grid.minor = element_blank())



case1 = baseplot +
  geom_abline(intercept = 4.3, slope = -1, linetype = "longdash", label = 3)  +
  annotate("text", x = 2.52, y = 2.22, size = 3, 
           label = "$n \\widehat{\\theta}^2_1$ + $n \\widehat{\\theta}^2_2 = C_{\\alpha}$",
           angle = -45)


case2 = baseplot +
  geom_polygon(aes(x = c(1,2,2), y = c(2,2,1)), alpha = 0.3, fill = "red") +
  geom_abline(intercept = 3, slope = -1, linetype = "longdash", label = 3) +
  annotate("text", x = 2.85, y = 0.6, size = 3,
           label = "$n \\widehat{\\theta}^2_1$ + $n \\widehat{\\theta}^2_2 = C_{\\alpha}$", 
           angle = -45)


case3 = baseplot +
  geom_polygon(aes(x = c(0, 0, 2, 2, 1), y = c(1, 2, 2, 0, 0)), 
               alpha = 0.3, fill = "red") +
  geom_abline(intercept = 1, slope = -1, linetype = "longdash", label = 3) +
  annotate("text", x = 1.96, y = -0.61, size = 2.7, 
           label = "$n \\widehat{\\theta}^2_1$ + $n \\widehat{\\theta}^2_2 = C_{\\alpha}$",
           angle = -45)


case4 = baseplot +
  geom_polygon(aes(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)), 
               alpha = 0.3, fill = "red") +
  geom_abline(intercept = -0.5, slope = -1, linetype = "longdash", label = 3) +
  annotate("text", x = 0.2, y = -0.4, size = 3,
           label = "$n \\widehat{\\theta}^2_1$ + $n \\widehat{\\theta}^2_2 = C_{\\alpha}$",
           angle = -45)


print(plot_grid(case1, case2, case3, case4, 
                labels = c("Case 1", "Case 2", "Case 3", "Case 4"),
                label_size = 11, hjust = 0, vjust = 1.5,
                scale = c(1., 1., 1., 1.)))
dev.off()


  
  
  
  
  