library(profvis)
profvis({
  # Start fresh
  rm(list = ls())
  
  # Initialize variables
  n <- 1000  # Set matrix size
  A <- matrix(runif(n * n), nrow = n, ncol = n)  # Random matrix A
  B <- matrix(runif(n * n), nrow = n, ncol = n)  # Random matrix B
  C <- matrix(0, nrow = n, ncol = n)  # Resultant matrix for loop method
  CC <- matrix(0, nrow = n, ncol = n) 
  # Loop approach
  time_loop <- system.time({
    for (i in 1:n) {
      for (j in 1:n) {
        for (k in 1:n) {
          C[i, j] <- C[i, j] + A[i, k] * B[k, j]
        }
      }
    }
  })
  cat("loop time:", time_loop["elapsed"], "\n")
  
  # Partial vectorized approach
  time_parvec <- system.time({
    for (j in 1:n) {
      CC[, j] <- A %*% B[, j]
    }
  })
  cat("Partial Vector time:", time_parvec["elapsed"], "\n")
  # Fully vectorized approach
  time_comp_vec <- system.time({
    CCC <- A %*% B
  })
  cat("Vector time:", time_comp_vec["elapsed"], "\n")
  # Compare results
  cat("Difference1:" ,(norm(C - CC, type = "2")))
  cat("Difference2:" ,(norm(C - CCC, type = "2")))
  # Speedups
  Speedup1 <- time_loop["elapsed"] / time_parvec["elapsed"]
  Speedup2 <- time_loop["elapsed"] / time_comp_vec["elapsed"]
  Speedup3 <- time_parvec["elapsed"] / time_comp_vec["elapsed"]
  cat("Speedup1:", Speedup1, "\n")
  cat("Speedup2:", Speedup2, "\n")
  cat("Speedup3:", Speedup3, "\n")
})