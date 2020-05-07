##### In this script we plot the chi-square distribution for different 
##### degrees of freedom

source("ThesisggTheme.R")

# Range on which we will plot
xvec = 0:650

ggplot() +              
  geom_line(aes(x = xvec, y = dchisq(xvec, df = 20), 
                color = "20")) +
  geom_line(aes(x = xvec, y = dchisq(xvec, df = 200), 
                color = "200")) +
  geom_line(aes(x = xvec, y = dchisq(xvec, df = 500), 
                color = "500")) +
  geom_hline(yintercept = 0) +
  labs(x = "x", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = "Degrees of freedom", 
                     values = c("20" = "#FF0000", 
                                "200" = "#0000FF", 
                                "500" = "#00FF00")) +
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold")) +
  ThesisggTheme()


