# The Impact of Disturbance History on Size-Abundance Scaling Patterns in Forests
This repository houses the scripts used to develop the models and analyses in the paper *The Impact of Disturbance History on Size-Abundance Scaling Patterns in Forests*, which is currently in review.

## What's in the Repository

The repository contains the following directories and scripts:

### **Data**
- **FIA_Model_Data/** – CSV files containing the data used to fit the models.

### **Stan Scripts**
- **STAN_scripts/** – Stan model files used to fit the Pareto distribution to the data.

### **R Scripts**
1. **1_Obtain_FIA_Data.R** – Downloads FIA data.
2. **2_Prelim_Data_Prep.R** – Prepares FIA data for analysis.
3. **3_Data_Prep_Random_Years.R** – Further data preparation for analysis.
4. **4_Fit_Pareto_Distribution.R** – Fits the size-abundance distribution to each plot to obtain the size-abundance slope. Calls a Stan script from **STAN_scripts/**.
5. **5_Linking_Unique_Plots_Across_Time.R** – Links FIA plot and tree IDs across re-measurements to ensure consistent plot IDs over time. Prepares data for the time-since-disturbance analysis.
6. **6_Bayesian_Model.R** – Sets up and runs the Bayesian model testing the effect of disturbance type on size-abundance slope.
7. **7_Time_Since_Disturbance.R** – Sets up and runs the Bayesian model testing the effect of time since disturbance on size-abundance slope, focusing on the disturbance types identified as important in **6_Bayesian_Model.R**.

### **Deprecated**
- Files and scripts that are no longer actively used but are retained for reference.

---

## Acknowledgements

We thank Brian Walters (NRS FIA) and Glenn Christensen (PNW FIA) for providing critical information regarding the inclusion of FIA trees within macroplots, which were excluded from our subplot measurements. This research was supported by NASA’s Biodiversity and Ecological Conservation program (grant 80NSSC23K0421), by Hatch project award no. [MEO-022425], from the U.S. Department of Agriculture’s National Institute of Food and Agriculture, and the Northeastern States Research Cooperative through funding made available by the USDA Forest Service. This research was also supported by the U.S. Department of Agriculture Forest Service, Northern Research Station, Forest Inventory and Analysis Program. The conclusions and opinions in this paper are those of the authors and not of the NSRC, the Forest Service, or the USDA.
