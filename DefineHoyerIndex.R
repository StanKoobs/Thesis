##### Script in which we define the Hoyer index ###########################

# Define function
HoyerIndex = function(vec) {
  if (all(vec == 0)) {
    return(0)
  }
  p = length(vec)
  l1norm = sum(abs(vec))
  l2norm = sqrt(sum(vec^2))
  return((sqrt(p) - l1norm / l2norm) / (sqrt(p) - 1))
}

