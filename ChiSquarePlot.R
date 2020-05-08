##### In this script we plot the chi-square distribution for different 
##### degrees of freedom

source("Packages.R")
source("ThesisggTheme.R")

# Range on which we will plot
xvec = 0:650

curvesplot = ggplot() +              
  geom_line(aes(x = xvec, y = dchisq(xvec, df = 20), 
                color = "20")) +
  geom_line(aes(x = xvec, y = dchisq(xvec, df = 200), 
                color = "200")) +
  geom_line(aes(x = xvec, y = dchisq(xvec, df = 500), 
                color = "500")) +
  geom_hline(yintercept = 0) +
  labs(title = "Chi-square curves", x = "x", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = "Degrees of freedom", 
                     values = c("20" = "#FF0000", 
                                "200" = "#0000FF", 
                                "500" = "#00FF00")) +
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 15, face = "bold")) +
  ThesisggTheme()

# Lets also find the critical value for different degrees of freedom

critvals = c()
for (i in 1:650) {
  critvals = c(critvals, qchisq(0.975, i))
}

critvalplot = ggplot() +
  geom_line(aes(x = 1:650, y = critvals)) +
  coord_cartesian(xlim = c(0, 600)) +
  labs(title = "Critical value ", 
       x = "Degrees of freedom", y = "Critical value") +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  ThesisggTheme()

library(cowplot)
plot_grid(curvesplot, critvalplot)
