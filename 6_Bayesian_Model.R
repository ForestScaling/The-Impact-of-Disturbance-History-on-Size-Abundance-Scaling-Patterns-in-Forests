library(brms)
library(data.table)
library(dplyr)
library(future)

bayesiandata<-fread("sizeabundance3_hold//slope_TPA_UNADJ_subplotonly.csv")%>%
  data.frame()%>%
  mutate(PLT_CN = as.character(PLT_CN))%>%
  inner_join(fread("sizeabundance3_hold//xmin12.7cm_xmax50cm_recentclip.csv")%>%
               data.frame()%>%
               mutate(PLT_CN = as.character(PLT_CN)))%>%
  filter(STDAGE <=900 & STDAGE > 0)%>%
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

bayesiandata<-bayesiandata%>%
  inner_join(fread("sizeabundance3_hold//FIA_Disturbance_DataClean_TRTCD.csv")%>%
               mutate(PLT_CN = as.character(PLT_CN)))%>%
  mutate(remove = ifelse(Disturbance_Type == "No Disturbance" & Treated == "Yes", "Yes","No"))%>%
  filter(remove == "No")


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


bayesianmodel<-brm(data = bayesiandata,
                   formula = bf(
                     corrected_slope | mi(error) ~ 
                       log10(STDAGE)  +  log10(MAX_HEIGHT) + Disturbance_Type
                     
                   ),
                   autocor = cor_car(region,
                                     formula = ~ 1|region),
                   iter=9000, warmup=6000,
                   data2= list(region =region),
                   prior = priors,
                   sample_prior = "yes", cores = 4, 
                   chains = 4, threads = threading(15))
save(bayesianmodel, file = paste0("slope_TPA_UNADJ_subplotonly.RData"))
