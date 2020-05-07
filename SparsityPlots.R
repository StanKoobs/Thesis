##### L1 norm and L2 norm #################################################

# First load in our ggplot theme
source("ThesisggTheme.R")

# We also need some packages like ggplot2
source("Packages.R")

# First we generate the l1 and l2 level plot of value 1
# To do this, we define the following two dataframes 
l2norm1 = data.frame(
  x0 = 0,
  y0 = 0,
  r = 1
)

l1norm1 <- data.frame(
  x = c(-1, 0, 1),
  y = c(0, 1, 0),
  w = c(-1, 0, 1),
  z = c(0, -1, 0)
)

# Here we generate the plot
l1andl2plot = ggplot() + 
  geom_line(aes(x = x, y = y, color = "blue"), data = l1norm1) +
  geom_line(aes(x = w, y = z, color = "blue"), data = l1norm1) +
  geom_circle(aes(x0 = x0, y0 = y0, r = r, color = "red"), 
              data = l2norm1) +
  scale_color_manual(name = "Legend", 
                     values = c("red" = "#FF0000", "blue" = "#0000FF"), 
                     labels = c("L1-norm", "L2-norm")) +
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15, face = "bold")) +
  ThesisggTheme()
l1andl2plot 

# Now we are going to plot the level curves of the Hoyer index
# First we run the FindLevelCurvesHoyer script to get the slope
source("FindLevelCurvesHoyer.R")

LevelCurvesHoyerPlot = ggplot() +
  geom_line(aes(x = x, y = y), data = l1norm1) +
  geom_line(aes(x = w, y = z), data = l1norm1) +
  geom_circle(aes(x0 = x0, y0 = y0, r = r), data = l2norm1) +
  geom_hline(aes(yintercept = 0, color = "blue")) +
  geom_vline(aes(xintercept = 0, color = "blue")) +
  geom_abline(aes(intercept = 0, slope = slope, color = "green")) +
  geom_abline(aes(intercept = 0, slope = 1 / slope, color = "green")) +
  geom_abline(aes(intercept = 0, slope = -1 / slope, color = "green")) +
  geom_abline(aes(intercept = 0, slope = -slope, color = "green")) +
  geom_abline(aes(intercept = 0, slope = 1, color = "red")) +
  geom_abline(aes(intercept = 0, slope = -1, color = "red")) +
  scale_color_manual(name = "Legend", 
                     values = c("red" = "#FF0000",
                                "blue" = "#0000FF",
                                "green" = "#00FF00"), 
                     labels = c("y = 1", "y = 1/2", "y = 0")) +
  theme(legend.key.size = unit(1.2, "cm"), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.title=element_text(size = 15, face = "bold")) +
  ThesisggTheme()
LevelCurvesHoyerPlot

