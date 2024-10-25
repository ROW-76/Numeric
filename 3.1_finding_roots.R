library(parallel) 

f <- function(x) sin(3 * pi * cos(2 * pi * x) * sin(pi * x))
a <- -3
b <- 5
n <- 4^9
x0 <- seq(a, b, length.out = n)
q <- numeric(length(x0)) 


find_R <- function(f, x) {
  lowerlim <- x - 0.01
  Upperlim <- x + 0.01
  if (f(lowerlim) * f(Upperlim) < 0) { # Check if signs are opposite
    return(uniroot(f, lower = lowerlim, upper = Upperlim)$root)
  } else {
    return(NA) 
  }
}


Seriestime <- system.time({
  for (i in seq_along(x0)) {
    q[i] <- find_R(f, x0[i])
  }
})
cat("Series time:", Seriestime["elapsed"], "\n")


no_workers <- detectCores()
num_cores <- min(no_workers - 1, 4)
c <- makeCluster(num_cores)
clusterExport(c, list("f", "find_R"))


paralleltime <- system.time({
  q_parallel <- parLapply(c, x0, function(x) find_R(f, x))
})
cat("Parallel time:", paralleltime["elapsed"], "\n")

stopCluster(c)

Speedup <- Seriestime["elapsed"] / paralleltime["elapsed"]
cat("Speedup:", Speedup)

Efficiency <- Speedup / num_cores * 100
cat("Efficiency:",Efficiency)
