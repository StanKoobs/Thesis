##### Script in which we define the Gini index ############################

GiniIndex <- function(vec) {
  sortvec <- sort(vec)
  GiniSum = 0
  p <- length(vec)
  for (i in 1:p) {
    GiniSum = GiniSum + (sortvec[i] / sum(abs(vec))) * ((p - i + 0.5) / p)
  }
  return(1 - 2 * GiniSum)
}

