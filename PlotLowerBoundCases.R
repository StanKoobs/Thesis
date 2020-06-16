

baseplot = ggplot() +
  geom_rect(aes(xmin = 0, xmax = 2, ymin = 0, ymax = 2), 
            alpha = 0, color = "black") +
  scale_x_continuous(breaks = c(0, 1, 2), 
                     labels = c(0, TeX('$C_{\\alpha} - \\delta_{p,n}^2$'), TeX('$\\delta_{p,n}^2$')), 
                     limits = c(-0.5, 3.1)) +
  scale_y_continuous(breaks = c(0, 1, 2), 
                     labels = c(0, TeX('$C_{\\alpha} - \\delta_{p,n}^2$'), TeX('$\\delta_{p,n}^2$')),
                     limits = c(-0.5, 3.1)) +
  xlab(TeX('$n \\widehat{\\theta}_1$')) +
  ylab(TeX('$n \\widehat{\\theta}_2$')) +
  #geom_hline(yintercept = 0) +
  #geom_vline(xintercept = 0) +
  ThesisggTheme() +
  theme(panel.grid.minor = element_blank())



case1 = baseplot +
  geom_abline(intercept = 4.3, slope = -1, linetype = "longdash", label = 3)  +
  annotate("text", x = 2.47, y = 2.4, size = 3.7, 
           label = TeX('$n \\widehat{\\theta}_1$ + $n \\widehat{\\theta}_2 = C_{\\alpha}$'))


case2 = baseplot +
  geom_polygon(aes(x = c(1,2,2), y = c(2,2,1)), alpha = 0.3, fill = "red") +
  geom_abline(intercept = 3, slope = -1, linetype = "longdash", label = 3) +
  annotate("text", x = 2.58, y = 0.8, size = 5,
           label = TeX('$n \\widehat{\\theta}_1$ + $n \\widehat{\\theta}_2 = C_{\\alpha}$'))


case3 = baseplot +
  geom_polygon(aes(x = c(0, 0, 2, 2, 1), y = c(1, 2, 2, 0, 0)), 
               alpha = 0.3, fill = "red") +
  geom_abline(intercept = 1, slope = -1, linetype = "longdash", label = 3) +
  annotate("text", x = 1.8, y = -0.25, size = 3.7, 
           label = TeX('$n \\widehat{\\theta}_1$ + $n \\widehat{\\theta}_2 = C_{\\alpha}$'))


case4 = baseplot +
  geom_polygon(aes(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)), 
               alpha = 0.3, fill = "red") +
  geom_abline(intercept = -0.3, slope = -1, linetype = "longdash", label = 3) +
  annotate("text", x = 0.7, y = -0.4, size = 3.7,
           label = TeX('$n \\widehat{\\theta}_1$ + $n \\widehat{\\theta}_2 = C_{\\alpha}$'))


plot_grid(case1, case2, case3, case4, 
          labels = c("Case 1", "Case 2", "Case 3", "Case 4"),
          label_size = 15, hjust = 0, vjust = 1,
          scale = c(1., 1., 1., 1.))


case2 +
  geom_polygon(aes(x = c(0, 0, 1), y = c(2, 3, 2)), 
               alpha = 0.3, fill = "blue") +
  geom_polygon(aes(x = c(2, 3, 2), y = c(1, 0, 0)), 
               alpha = 0.3, fill = "blue") +
  geom_line(aes(x = c(-Inf, 2), y = c(1, 1)), linetype = "dotted") +
  geom_line(aes(x = c(1, 1), y = c(-Inf, 2)), linetype = "dotted") +
  annotate("text", x = 1.5, y = 1.5, size = 18,
           label = "D")
  
  
  
  
  