# Load required libraries
library(rstan)        # For Stan modeling
library(rstantools)   # Tools for Stan modeling
library(posterior)    # Tools for working with posterior distributions
library(data.table)   # For data manipulation
library(doParallel)   # For parallel processing
library(parallelly)   # For parallel processing
library(future)       # For parallel and distributed computing
library(dplyr)        # For data manipulation
library(tidyr)        # For data tidying

# Load the data and prepare it
sizeabundance3 <- fread("sizeabundance3_hold//sizeabundance3_[descriptive_name_here].csv") %>%
  data.frame() %>%                           # Convert to data frame
  mutate(PLT_CN = as.character(PLT_CN))      # Convert PLT_CN to character type

# Load select function from dplyr to avoid conflicts
select <- dplyr::select

# Compile the Stan model
model <- stan_model(file = "density2_simplified.stan")

# Detect number of cores available
cores <- detectCores()

# Create a parallel cluster using all available cores
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# Run the following code in parallel for each unique PLT_CN
foreach(i = unique(sizeabundance3$PLT_CN)) %dopar% {
  # Load required libraries inside the parallel loop
  library(rstan)
  library(rstantools)
  library(posterior)
  library(data.table)
  library(doParallel)
  library(parallelly)
  library(future)
  library(dplyr)
  library(tidyr)
  
  # Filter the data for the current PLT_CN and remove rows with missing DIA values
  test <- sizeabundance3 %>%
    filter(PLT_CN == i) %>%
    drop_na(DIA)
  
  # Prepare data for Stan model
  N <- length(test$DIA)  # Number of observations
  x <- test$DIA          # Diameter values
  x_min <- 5             # Minimum diameter value possible, due to how data is collected in the subplot
  x_max <- max(test$DIA) # Maximum diameter value
  
  # Create a list of data for Stan model
  stan_dat <- list(N = N, x = x, x_min = x_min, x_max = x_max)
  
  # Fit the Stan model
  fit <- sampling(model, data = stan_dat, iter = 9000, warmup = 6000, chains = 4, cores = 4)
  
  # Define the sequence of iteration numbers for summarizing draws
  iterations <- round(seq(from = 100, to = 3000, length.out = 10))
  
  # Create an empty list to store the results
  results <- list()
  
  # Loop through each iteration number to summarize draws
  for (j in iterations) {
    # Summarize draws for alpha_low variable, the alpha for the lower part of the piecewise
    # fit
    summary <- summarise_draws(subset_draws(as_draws_df(as_draws(fit)), iteration = j, variable = c("alpha_low"))) %>%
      mutate(multbayesnumber = row_number()) %>%
      rbind(
        # Summarize draws for alpha_high variable, the alpha for the higher part of the piecewise
        # fit
        summarise_draws(subset_draws(as_draws_df(as_draws(fit)), iteration = j, variable = c("alpha_high"))) %>%
          mutate(multbayesnumber = row_number())
      ) %>%
      rbind(
        # Summarize draws for tau variable (the x coordiate where the break for the piecewise fit
        # comes from)
        summarise_draws(subset_draws(as_draws_df(as_draws(fit)),
                                     iteration = j, variable = c("tau"))) %>%
          mutate(multbayesnumber = row_number())
      )
    
    # Add the iteration number to the summary
    summary <- mutate(summary, iteration = i)
    
    # Append the summary to the results list
    results[[j]] <- summary
  }
  
  # Combine all summaries into a single data frame
  data <- bind_rows(results)
  data$N_Trees <- nrow(test)  # Add the number of trees to the data frame
  
  # Select relevant columns and add plot identifier
  data <- data %>%
    select(-rhat, -ess_bulk, -ess_tail) %>%  # Remove specific columns
    mutate(PLT_CN = i) %>%                  # Add plot identifier
    mutate(corrected_slope = ifelse(variable == "alpha_low" |
                                      variable == "alpha_high", 
                                    -(mean + 1), NA))  # Calculate slope from alpha,
  # which is just the opposite of (alpha+1)
  
  # Save the results to a CSV file
  fwrite(data,
         file = paste0("distribution_output[descriptivename]/", i, ".csv"))
}

# Run garbage collection to free up memory
gc()
