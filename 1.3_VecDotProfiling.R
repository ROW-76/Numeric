library(profvis)
profvis({
  # Start fresh
  n <- 10000 # Increase the length of the vectors for better timing
  a <- runif(n) # Create a random vector a
  b <- runif(n) # Create a random vector b
  c <- 0 # Pre-allocate memory to store the result in c
  
  # For-loop
  time_for <- system.time({
    for (i in 1:n) {
      c <- c + a[i] * b[i]
    }
  })["elapsed"] 
  
  cat("Dot product with for-loop:", c, "\n")
  cat("Time taken (for-loop):", time_for, "\n")
  
  # Vectorization
  time_vec <- system.time({
    cc <- sum(a * b)
  })["elapsed"] # Get the elapsed time
  
  cat("Dot product with vectorization:", cc, "\n")
  cat("Time taken (vectorization):", time_vec, "\n")
  
  # Compare the results
  norm_difference <- (sum((c - cc))) 
  cat("Difference (norm):", norm_difference, "\n") 
  
  Speedup <- time_for / time_vec
  
  cat("Speed-up:", Speedup, "\n")
})