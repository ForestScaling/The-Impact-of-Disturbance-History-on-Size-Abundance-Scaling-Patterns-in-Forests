library(rFIA)
library(tidyverse)
library(data.table)
filter<-dplyr::filter
select<-dplyr::select
mutate<-dplyr::mutate

dir.create("mortdir")

#Create a vector of the 48 states so we can loop over them
successfulstates <- c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                      "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                      "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", 
                      "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
                      "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA",
                      "WV", "WI", "WY")

# Calculating mortality in the FIA dataset can be difficult if you don't know what you're
# doing. The rFIA package has a function that allows for the calculation of mortality for
# each plot in each year already, so we take advantage of that

#Loop over all states
for (i in successfulstates){
  #Read in one state database at a time. Even so, it's a lot of data, and we don't need
  #to bring it into memory since we aren't adjusting any of the class groups. So we leave
  #inMemory = FALSE
  db<-readFIA(dir="FIA", inMemory = FALSE, states = i)
  # Calculate the mortality by plot
  mort<-growMort(db, byPlot = TRUE)
  # Write the subsequent results to the folder we made above
  fwrite(mort, paste0("mortdir",i,"_mortbyplot.csv"))
  
}
# Read the files back into R and combine them into a single data frame
mort<-list.files("mortdir//",
                 pattern="_mortbyplot.csv",
                 full.names = TRUE)%>%
  lapply(fread)%>%
  rbindlist()%>%
  data.frame()%>%
  # Some of the time, percentage mortality is NA if there was a division by 0 in the calculations.
  # Occurs if there is no mortality, so replace NAs with 0
  mutate(MORT_PERC = ifelse(is.na(MORT_PERC) == TRUE, 0, MORT_PERC))


# Calculate the  maximum mortality that has occurred up until a given survey.
# Only mortality measurements that occur in the surveys prior to and including a given survey
# are counted as part of the maximum mortality.
morttest <- mort %>%
  arrange(pltID,YEAR)%>%
  group_by(pltID) %>%
  select(YEAR, pltID, PLT_CN, MORT_PERC)%>%
  mutate(Max_Mort = cummax(MORT_PERC))%>%
  ungroup()


# For a given maximum mortality, calculate the year in which it occurred and add it to a new column
# for each plot
morttest<-morttest%>%
  arrange(pltID, YEAR)%>%
  group_by(pltID, Max_Mort)%>%
  mutate(Max_Mort_Year = cummin(YEAR))%>%
  ungroup

# For each survey, calculate the time in years since the year of the survey with maximum 
# mortality (allows for a model to account for temporal effects)
morttest <- morttest %>%
  mutate(Years_Since_Max_Mort = YEAR - Max_Mort_Year)

# Recombine data frame with original full data frame of all plots and trees, adding the 
# maximum mortality calculations for each site
sizeabundanceandmort<-sizeabundance%>%
  rename(PLT_CN = PLT_CN.x)%>%
  inner_join(morttest%>%
               data.frame()%>%
               rename(INVYR=YEAR), by=c("PLT_CN","INVYR"))%>%
  distinct(PLT_CN, .keep_all= TRUE)


# Create a new column indicating whether a disturbance occurred at a plot in
# a given survey year (0 for No, 1 for Yes)
sizeabundanceandmort <- sizeabundanceandmort %>%
  mutate(Disturbance_Flag = if_else(Disturbance == "Yes", 1, 0))

# Filter df to include only pltID groups where the sum of Disturbance_Flag equals 1
# This allows us to only look at plots that had a single disturbance year and a single maximum
# mortality year, controlling for any confounding effect of multiple disturbances
filtered_df <- sizeabundanceandmort %>%
  group_by(pltID.y) %>%
  filter(sum(Disturbance_Flag) == 1)

# Perform an inner join with the original dataframe to retain all original columns,
# filtering plots that don't fit with our criteria
filtered_df <- filtered_df %>%
  inner_join(sizeabundanceandmort, by = c("pltID.y", "PLT_CN","INVYR", "Disturbance"))%>%
  select(PLT_CN, INVYR, Max_Mort.x,Max_Mort_Year.x, Disturbance_Flag.x)

sizeabundanceandmort<-inner_join(filtered_df,filtered_df%>%
                                   mutate(Year_of_Disturbance = ifelse(Disturbance_Flag.x == 1, INVYR, 0))%>%
                                   summarize(Year_of_Disturbance=max(Year_of_Disturbance)))%>%
  # Plots have multiple measurements, one per tree, which we don't need quite yet.
  distinct(pltID.y,.keep_all=TRUE)


# Examine the relationship between year of maximum mortality and the year of disturbance. If mortality is tied
# to disturbance, there should be a high correlation and the slope should be approximately 1
summary(lm(data=sizeabundanceandmort,
           Max_Mort_Year.x ~ Year_of_Disturbance))

cor(sizeabundanceandmort$Max_Mort_Year.x, sizeabundanceandmort$Year_of_Disturbance)
ggplot(sizeabundanceandmort, aes(Year_of_Disturbance, Max_Mort_Year.x))+
  geom_hex()+geom_smooth(method= "lm")+
  scale_fill_viridis_c()

ggplot(sizeabundanceandmort, aes(Year_of_Disturbance, Max_Mort_Year.x))+
  stat_density_2d(geom = "point", aes(size = after_stat(density)),n=75, contour = FALSE)+
  geom_smooth(method= "lm")



# Filter to just the plots where the year the maximum mortality occurred is the same
# as the year when the disturbance occurred
sizeabundanceandmort<-sizeabundanceandmort%>%
  filter(Max_Mort_Year.x == Year_of_Disturbance)%>%
  mutate(Disturbance= "Yes")%>%
  rename(pltID=pltID.y)%>%
  select(pltID,Max_Mort_Year.x,Year_of_Disturbance)%>%
  mutate(INVYR = Year_of_Disturbance)%>%
  inner_join(filtered_df%>%
               rename(pltID= pltID.y))

# Make sure to join with the original table of trees so we have all the
# associated variables
sizeabundanceandmort1<-sizeabundance%>%
  rename(PLT_CN =PLT_CN.x)%>%
  inner_join(sizeabundanceandmort, by = c("INVYR","PLT_CN"))

summarise<-dplyr::summarise

sizeabundanceandmort1<-sizeabundanceandmort1 %>%
  rename(pltID = pltID.x)%>%
# We need to remove the microplot data
  filter(DIA>5)%>%
  # Convert inches to centimeters
  mutate(DIA = DIA * 2.54)%>%
  # Make sure we are only looking at plots with the modern survey method
  filter(INVYR>=2001)%>%
  group_by(pltID, INVYR) %>%
  drop_na(DIA)%>%
  ungroup()


# Make sure we are only looking at plots with more than 25 trees
sizeabundanceandmort1<-sizeabundanceandmort1%>%
  group_by(pltID, INVYR)%>%
  summarize(N_trees = n())%>%
  ungroup()%>%
  filter(N_trees >= 25)%>%
  inner_join(sizeabundanceandmort1)

# There is a plot id column, but we number them instead just to make it a little easier
sizeabundance3mort<-sizeabundanceandmort1%>%
  dplyr::select(pltID)%>%
  distinct(pltID)%>%
  dplyr::mutate(Plot_Num = row_number())%>%
  full_join(sizeabundanceandmort1)

write_csv(sizeabundance3mort,
          "sizeabundance3_grt25_maxmortyear_yearofdisturbance.csv")

