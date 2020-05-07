##### In this script we determine the 2-dimensional vectors which have 
##### sparsity equal to 1 / 2

# First, we load in the sparsity function
source("DefineHoyerIndex.R")

# We want solve for all x1 and x2 such that HoyerIndex(x1, x2) = 1 / 2
# We cannot solve this system analytically so we need a root-solver
# Therefore we use the rootSolve package
source("Packages.R")

# For these values of x2 we will find the x1 coordinate
x2values = seq(0.1, 1, by = 0.01)
x1values = c()

# The input of the uniroot function needs a function with 1-dimensional 
# input
# To satisfy this, we loop through all x2values and in every iteration we 
# define the root function again with another x2 value as default
for (i in x2values) {
  RootOfHoyer = function(x1, x2 = i, y = 1 / 2) {
    HoyerIndex(c(x1, x2)) - y
  }
  x1values = c(x1values, uniroot(RootOfHoyer, c(0, 0.25))$root)
}

# Now we find the slope
slope = 0.9 / (x1values[length(x1values)] - x1values[1])
slope
# Slope is given approximately by 4.134
# This will be used to generate the plot of the level curves

