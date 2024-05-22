# Load dplyr's summarise function explicitly to avoid any conflicts
summarise <- dplyr::summarise

# Filter sizeabundance data to only include trees with diameter (DIA) > 5 
# and surveys conducted from 2001 onwards, to ensure that we are not 
# examining the microplots and using only data collected with the 
# consistent plot designs
sizeabundance1 <- sizeabundance %>%
  filter(DIA > 5) %>%
  filter(INVYR >= 2001) %>%
  group_by(pltID, INVYR) %>%
  drop_na(DIA) %>%  # Remove rows with missing DIA values
  ungroup()

# Further filter sizeabundance1 to only include plots with at least 25 trees. Plots with
# only a few trees won't provide enough to work with for the distribution fit
sizeabundance1 <- sizeabundance1 %>%
  group_by(pltID, INVYR) %>%
  summarize(N_trees = n()) %>%  # Count the number of trees in each plot and survey year
  ungroup() %>%
  filter(N_trees >= 25) %>%  # Keep only the groups with 25 or more trees
  inner_join(sizeabundance1)  # Join back to the original sizeabundance1 to retain all columns

# Arrange the data by plot ID and inventory year,
# then add columns to indicate disturbance and filter out plots with a certain disturbance code
sizeabundance2 <- sizeabundance1 %>%
  arrange(pltID, INVYR) %>%
  group_by(pltID) %>%
  mutate(KINDCD1 = cumany(KINDCD == 3)) %>%  # Check if a plot was ever replaced, meaning 
  # a new plot was generated with the same ID code to replace an old one that was no longer
  # operational for some reason
  filter(KINDCD1 != 3) %>%  # Remove the replacement plots only, keeping the originals
  mutate(Description = cumany(Disturbance != "No")) %>%  # Check if there's any disturbance 
  # in the plot's history, and when
  mutate(Disturbance = ifelse(Description == TRUE, "Yes", "No")) %>%  # Mark plots as disturbed
  # if any disturbance occurred, from the year they were disturbed onward
  ungroup()

# Set a seed for reproducibility when sampling random years
set.seed(01282024)

# Randomly select one inventory year per plot
random_years <- sizeabundance2 %>%
  group_by(pltID) %>%
  filter(!is.na(INVYR)) %>%
  summarise(INVYR = sample(INVYR, 1))  # Randomly sample one year per plot

# Join the original data with the randomly selected years to filter the data
sizeabundance1 <- sizeabundance2 %>%
  inner_join(random_years)

# Run garbage collection to free up memory
gc()

#The column PLT_CN has unique IDs for every plot/year combination, but the IDs are
# very long and in integer64 format, which can be tricky to use sometimes. You can
# either change them to character with as.character(), use the pltID column that we created
# earlier from a unique combination of country, state, county, and plot numbers, or add in a
# new column Plot_Num that just has the row number to identify the plot
sizeabundance3<-sizeabundance1%>%
  dplyr::select(pltID)%>%
  distinct(pltID)%>%
  dplyr::mutate(Plot_Num = row_number())%>%
  full_join(sizeabundance1)

write_csv(sizeabundance3,
          "sizeabundance3_grt25_nodisturbancemaxmortyearMay17.csv")
