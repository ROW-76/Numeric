library(profvis)
profvis({
  # Start fresh
  rm(list = ls())
  # Initialize variables
  n <- 100
  A <- matrix(runif(n * n), nrow = n, ncol = n)  # Random matrix
  x <- runif(n)  # Random vector
  b <- numeric(n)
  bb <- numeric(n)
  # Loop approach
  time_loop <- system.time({
    for (i in 1:n) {
      for (j in 1:n) {
        b[i] <- b[i] + A[i, j] * x[j]
      }
    }
  })
  cat("loop time:", time_loop["elapsed"], "\n")
  # Partial vectorized approach
  time_parvec <- system.time({
    for (i in 1:n) {
      bb[i] <- A[i, ] %*% x
    }
  })
  cat("Partial Vector time:", time_parvec["elapsed"], "\n")
  # Fully vectorized approach
  time_comp_vec <- system.time({
    bbb <- A %*% x
  })
  cat("Vector time:", time_comp_vec["elapsed"], "\n")
  # Compare results
  cat("Difference1:" ,(norm(b - bb, type = "2")))
  cat("Difference2:" ,(norm(b - bbb, type = "2")))
  # Speedups
  Speedup1 <- time_loop["elapsed"] / time_parvec["elapsed"]
  Speedup2 <- time_loop["elapsed"] / time_comp_vec["elapsed"]
  Speedup3 <- time_parvec["elapsed"] / time_comp_vec["elapsed"]
  cat("Speedup1:", Speedup1, "\n")
  cat("Speedup2:", Speedup2, "\n")
  cat("Speedup3:", Speedup3, "\n")
})