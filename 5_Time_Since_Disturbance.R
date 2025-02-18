library(brms)
library(data.table)
library(dplyr)
library(future)

bayesiandata<-fread("sizeabundance3_hold//slope_TPA_UNADJ_timesincedist_allvars_ecoregion.csv")%>%
  data.frame()%>%
  mutate(PLT_CN = as.character(PLT_CN))%>%
  inner_join(fread("sizeabundance3_hold//xmin12.7cm_xmax50cm_recentclip.csv")%>%
               data.frame()%>%
               mutate(PLT_CN = as.character(PLT_CN)))%>%
  filter(STDAGE <=900 & STDAGE > 0)%>%
  # mutate(DSTRBCD1 = ifelse(TRTCD1 == 10, 110, DSTRBCD1))%>%
  # mutate(DSTRBCD1 = ifelse(TRTCD1 == 20, 120, DSTRBCD1))%>%
  filter(DSTRBCD1 %in% c(0,10,22, 46,40, 30,31,80, 52#, 110,120
  ))%>%
  dplyr::mutate(Disturbance_Type = case_when(
    DSTRBCD1 == 0 ~ "No Disturbance",
    DSTRBCD1 == 10 ~ "Insect Damage",
    DSTRBCD1 == 80 ~ "Human-Caused Damage",
    DSTRBCD1 == 22 ~ "Disease Damage",
    DSTRBCD1 == 30 ~ "Fire (Crown and Ground)",
    DSTRBCD1 == 31 ~ "Ground Fire",
    DSTRBCD1 == 32 ~ "Crown Fire",
    DSTRBCD1 %in% c(46, 40) ~ "Animal Damage/Grazing",
    DSTRBCD1 == 52 ~ "Wind",
    DSTRBCD1 %in% c(95, 91) ~ "Landslides/Earthquakes",
    DSTRBCD1 == 110 ~ "Tree Cutting"#,
    #DSTRBCD1 == 120 ~ "Slash and Burn"
  ))%>%
  mutate(Disturbance_Type = relevel(factor(Disturbance_Type),
                                    "No Disturbance"))%>%
  filter(STDAGE >0&MAX_HEIGHT>0 &
           corrected_slope <0)%>%
  rename(region = ECODE_NAME)%>%
  filter(region != "")%>%
  mutate(error = se_mean/sd)

tnc_bin <- read.csv('tnc_adjacencymatrix.csv', row.names = 1)
tnc_bin <- as.matrix(tnc_bin)
dimnames(tnc_bin)[[2]] <-rownames(tnc_bin)

# Get unique groups from bayesiandata
groups_in_data <- unique(bayesiandata$region)  # Replace group_column_name with the actual column name

# Find groups in tnc_bin that are present in bayesiandata
groups_in_tnc_bin <- rownames(tnc_bin)

common_groups <- intersect(groups_in_tnc_bin, groups_in_data)
# Extract rows and columns corresponding to common groups in tnc_bin
tnc_bin_common <- tnc_bin[common_groups, common_groups]

dimnames(tnc_bin_common)[[2]] <- NULL

region<-tnc_bin_common
priors <- c(
  prior(normal(0, 5), class = "b"),        # Prior for slope parameter
  prior(normal(0, 2), class = "sigma") # Prior for intercept parameter
)

# 
# bayesianmodel1<-brm(data = bayesiandata,
#                    formula = bf(
#                      corrected_slope | mi(error) ~ s(Time_Since_Dist) + 
#                        log10(STDAGE) + log10(MAX_HEIGHT) #+ (1 | FINAL_PLT_ID) 
#                    ),
#                    autocor = cor_car(region,
#                                      formula = ~ 1|region),
#                    iter=9000, warmup=6000,
#                    data2= list(region =region),
#                   prior = priors,
#                    sample_prior = "yes", cores = 4, 
#                    chains = 4,  #control = list(adapt_delta = 0.9, max_treedepth = 15),
#                   threads = threading(15)
#                   )

# priors <- c(
#   prior(normal(0, 2), class = "b"),
#   prior(student_t(3, 0, 1), class = "sd"),
#   prior(student_t(3, 0, 1), class = "sds"),
#   prior(student_t(3, 0, 2), class = "Intercept"),
#   prior(student_t(3, 0, 1), class = "sigma")
# )
bayesiandata<-bayesiandata%>%
  inner_join(fread("sizeabundance3_hold//slope_TPA_UNADJ_timesincedist_allvars_ecoregion.csv")%>%
               data.frame()%>%
               mutate(PLT_CN = as.character(PLT_CN))%>%
               inner_join(fread("sizeabundance3_hold//xmin12.7cm_xmax50cm_recentclip.csv")%>%
                            data.frame()%>%
                            mutate(PLT_CN = as.character(PLT_CN)))%>%
               filter(STDAGE <=900 & STDAGE > 0)%>%
               # mutate(DSTRBCD1 = ifelse(TRTCD1 == 10, 110, DSTRBCD1))%>%
               # mutate(DSTRBCD1 = ifelse(TRTCD1 == 20, 120, DSTRBCD1))%>%
               filter(DSTRBCD1 %in% c(0,10,22, 46,40, 30,31,80, 52#, 110,120
               ))%>%
               dplyr::mutate(Disturbance_Type = case_when(
                 DSTRBCD1 == 0 ~ "No Disturbance",
                 DSTRBCD1 == 10 ~ "Insect Damage",
                 DSTRBCD1 == 80 ~ "Human-Caused Damage",
                 DSTRBCD1 == 22 ~ "Disease Damage",
                 DSTRBCD1 == 30 ~ "Fire (Crown and Ground)",
                 DSTRBCD1 == 31 ~ "Ground Fire",
                 DSTRBCD1 == 32 ~ "Crown Fire",
                 DSTRBCD1 %in% c(46, 40) ~ "Animal Damage/Grazing",
                 DSTRBCD1 == 52 ~ "Wind",
                 DSTRBCD1 %in% c(95, 91) ~ "Landslides/Earthquakes",
                 DSTRBCD1 == 110 ~ "Tree Cutting"#,
                 #DSTRBCD1 == 120 ~ "Slash and Burn"
               ))%>%group_by(FINAL_PLT_ID)%>%
               filter(Time_Since_Dist == 0)%>%mutate(Actual_Dist = Disturbance_Type)%>%
               ungroup()%>%filter(Actual_Dist == "Ground Fire"|Actual_Dist == "Animal Damage/Grazing")%>%
               select(FINAL_PLT_ID, Actual_Dist)%>%
               distinct(FINAL_PLT_ID,.keep_all = TRUE))

bayesianmodel <- brm(
  corrected_slope | mi(error) ~ 
    Time_Since_Dist + log10(STDAGE) + Actual_Dist+ 
    log10(MAX_HEIGHT) + (1 | FINAL_PLT_ID),
  autocor = cor_car(region,
                    formula = ~ 1|region),
  data2= list(region =region),
  data = bayesiandata,
  prior = priors,
  iter = 9000, warmup = 6000,
  cores = 4, chains = 4,threads = threading(15)
)

save(bayesianmodel, file = paste0("FIA_bayesmodel_smoothterm_reviewerresponse_linear_includeCAR.RData"))
