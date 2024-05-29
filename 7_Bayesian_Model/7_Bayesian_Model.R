# Load necessary libraries
library(brms)       # For Bayesian regression modeling
library(data.table) # For data manipulation
library(dplyr)      # For data manipulation
library(future)     # For parallel processing

# Read in the data and filter it
bayesiandata <- fread("bayesian_piecepareto.csv") %>%
  data.frame() %>% 
  filter(STDAGE > 0) %>%                           # Filter out rows where STDAGE is not greater than 0
  filter(variable == "alpha_low") %>%              # Filter for the piecewise segment for intermediate trees
  rename(region = ECODE_NAME) %>%                  # Rename 'ECODE_NAME' column to 'region'
  filter(region != "")                             # Filter out rows where region is an empty string

# Set the seed for reproducibility
set.seed(1)

# Read in the adjacency matrix for regions, from Read et al. 2020
tnc_bin <- read.csv('tnc_adjacencymatrix.csv', row.names = 1)
tnc_bin <- as.matrix(tnc_bin)
dimnames(tnc_bin)[[2]] <- rownames(tnc_bin)        # Set the column names of the matrix

# Get unique regions from bayesiandata
groups_in_data <- unique(bayesiandata$region)

# Get regions from tnc_bin matrix
groups_in_tnc_bin <- rownames(tnc_bin)

# Find common regions between bayesiandata and tnc_bin
common_groups <- intersect(groups_in_tnc_bin, groups_in_data)

# Extract rows and columns corresponding to common regions in tnc_bin
tnc_bin_common <- tnc_bin[common_groups, common_groups]
dimnames(tnc_bin_common)[[2]] <- NULL              # Remove column names

# Define the region adjacency matrix for the CAR model
region <- tnc_bin_common

# Create a list to store data for multiple Bayesian models
multipledata <- list()

# Loop through the data and filter so each model has its own data frame in list form
for(i in 1:10){
  multipledata[[i]] <- bayesiandata %>%
    filter(multbayesnumber == i) %>%
    mutate(Disturbance = as.factor(Disturbance))   # Convert 'Disturbance' to a factor
}

# Set up parallel processing with 10 workers
plan(multisession, workers = 10)

# Define priors for the Bayesian model
priors <- c(
  prior(normal(0, 5), class = "b"),       # Prior for slope parameter
  prior(normal(0, 2), class = "sigma")    # Prior for intercept parameter
)

# Fit Bayesian models using the brm_multiple function
bayesianmodel <- brm_multiple(
  data = multipledata,                   # List of datasets for multiple imputation
  formula = corrected_slope ~ log10(STDAGE) + 
    Prop_Hardwood_to_Softwood + Disturbance +
    poly(MORT_PERC, 2) + log10(Max_Height),  # Model formula
  autocor = cor_car(region, formula = ~ 1 | region),  # Define CAR model for spatial autocorrelation
  iter = 9000, warmup = 6000,            # Number of iterations and warm-up period
  data2 = list(
    list(region = region), list(region = region), list(region = region),
    list(region = region), list(region = region), list(region = region),
    list(region = region), list(region = region), list(region = region),
    list(region = region)
  ),                                    # List of region adjacency matrices for each imputed dataset
  prior = priors,                        # Set priors for the model
  sample_prior = "yes",                  # Include prior samples in the output
  chains = 4                             # Number of Markov chains
)

# Save the fitted Bayesian model
save(bayesianmodel, file = paste0("bayesian_piecepareto.RData"))