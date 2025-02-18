This folder holds the data required to make our Bayesian models

# FIA_Disturbance_DataClean_TRTCD
This csv file selects just those plots that were "treated" according to the FIA classification system. Possible treatments are

| Code | Description |
|------|-------------|
| 00   | **No observable treatment**. |
| 10   | **Cutting** – The removal of one or more trees from a stand. |
| 20   | **Site preparation** – Clearing, slash burning, chopping, disking, bedding, or other practices clearly intended to prepare a site for either natural or artificial regeneration. |
| 30   | **Artificial regeneration** – Following a disturbance or treatment (usually cutting), a new stand where at least 50 percent of the live trees present resulted from planting or direct seeding. |
| 40   | **Natural regeneration** – Following a disturbance or treatment (usually cutting), a new stand where at least 50 percent of the live trees present (of any size) were established through the growth of existing trees and/or natural seeding or sprouting. |
| 50   | **Other silvicultural treatment** – The use of fertilizers, herbicides, girdling, pruning, or other activities (not covered by codes 10-40) designed to improve the commercial value of the residual stand; or chaining, which is a practice used on woodlands to encourage wildlife forage. |

We use this file to know which plots to remove from our data. We want to fit a Bayesian model to only those plots that had no observable treatment.


# slope_TPA_UNADJ_timesincedist_allvars_ecoregion
This csv file selects just those plots that we found had significant impacts of disturbance (animal grazing and ground fire). We look at the effect of time since disturbance on the size-abundance slope. The calculated slope for each plot is based on data that has been corrected using the TPA_UNADJ factor.

# slope_TPAUNADJcorrected
This csv file filters plots to those that fit our criteria as specified in our paper. The calculated slope for each plot is based on data that has been corrected using the TPA_UNADJ factor.

# slope_xmin12.7cm_xmax50cm_recentclip.csv
This csv file holds information about each plot that is useful for the actual modeling process (e.g., Disturbance Type)

# tnc_adjacencymatrix
This csv file is an adjacency matrix that is used to tell brms which ecoregions are adjacent to which ecoregions. It provides the data necessary to set up the spatial autocorrelation model