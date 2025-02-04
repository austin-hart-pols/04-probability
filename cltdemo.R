
# Define the population ---------
# Data and density
  population <- rnorm(10000, mean = 0, sd = 1)  #^2 - 2 for skew
  pop_density <- density(population)

# SET graphs --------------------  
# Panels
  par(mfrow = c(3, 1))

  ## TOP PANEL: Population Density ----
  plot(
    pop_density,
    main = "Population Distribution",
    xlab = "Test score (standardized)",
    yaxt = 'n',
    ylab = NA,
    ylim = c(0, max(pop_density$y) * 1.5),
    xlim = c(-3, 3),
    col = "black",
    lwd = 2,
    lty = 2
  )

  ## MIDDLE PANEL: Sample Density ----
  plot(
    pop_density, 
    type = "n",   # create an empty plot
    main = "Sample Distributions",
    xlab = "Test score (standardized)",
    yaxt = 'n',
    ylab = NA,
    xlim = c(-3, 3),
    ylim = c(0, max(pop_density$y) * 1.5)
  )

  ## Legend
  legend(
    "topright",
    legend = c("Dist", "Mean"),
    col = c(rgb(0, 0, 1, alpha = 0.5), rgb(1, 0, 0, alpha = 0.7)),
    lty = c(1, NA),
    pch = c(NA, 1),
    bty = "n"
  )

# Draw random samples ------------
# Define samples
  n_iterations <- 50
  sample_size <- 250
  
# Create a vector to store the sample means
  all_sample_means <- numeric(n_iterations)
  
# Take sample and plot
  for (i in 1:n_iterations) {
    
    # Take a random sample from the population
    sample_data <- sample(population, sample_size)
    
    # Compute density for the sample
    sample_density <- density(sample_data)
    
    # Overlay the sample density using a semi-transparent blue line
    lines(sample_density, col = rgb(0, 0, 1, alpha = 0.5), lwd = 1)
    
    # Compute the sample mean and store it
    sample_mean <- mean(sample_data)
    all_sample_means[i] <- sample_mean
    
    # Mark the sample mean on the x-axis using a hollow dot
    points(sample_mean, 0, pch = 1, col = rgb(1, 0, 0, alpha = 0.7), cex = 1.5)
    
    # Pause briefly so that you can see the updates in real time
    Sys.sleep(0.75)
  }

  
# Sampling Dist ------------------
# Density of the sample means
  sample_means_density <- density(all_sample_means)

# Plot sampling distribution
  plot(
    sample_means_density,
    main = "SAMPLING DIST (sample means)",
    xlab = "AVG score across samples",
    ylab = NA,
    yaxt = 'n',
    xlim = c(-3,3),
    ylim = c(0, max(sample_means_density$y) * 1.25),
    col = "darkgreen",
    lwd = 2
  )

# Summary stats on sample means
  summary(all_sample_means)  
  