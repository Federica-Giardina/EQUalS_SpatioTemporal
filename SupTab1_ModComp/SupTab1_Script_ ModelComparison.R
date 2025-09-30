# LIBRARIES
library(tidyverse); library(INLA)

# SET WORKIND DIRECTORY
setwd("H:\\Carsten - First glance at data/RESULTS APRIL 2025/")

# Set working directory
setwd("H:\\Carsten - First glance at data")


# DATA IMPORT AND PREP
source("CrucialScripts_DONOTMOVE/DataPrep.R")
source("CrucialScripts_DONOTMOVE/Mob_Thresholds.R")

# READ MODELS
HospBasic1   <- readRDS(file = "RESULTS JULY 2025/00_ModRes/res_HospBasic1.rds")
HospBasic2   <- readRDS(file = "RESULTS JULY 2025/00_ModRes/res_HospBasic2.rds")
HospBasic3   <- readRDS(file = "RESULTS JULY 2025/00_ModRes/res_HospBasic3.rds")

InfsAgg3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_InfsAgg3.rds")
InfsDag3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_InfsDag3.rds")

HospAgg3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg3.rds")
HospDag3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospDag3.rds")

HospAgg1     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg1.rds")
HospAgg5     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg5_post.rds")


# Specify values
data               <- Cases.nb.wk %>% filter(week_begin > as.Date("2020-05-25")) # Remove filter for hospitalisations
data$y             <- data$case_obs # case_obs OR hosp_obs
type               <- "InfsAgg3"
result             <- InfsAgg3
week_begin         <- as.Date("2020-06-01") # Hosps: as.Date("2020-01-06") OR Infs: as.Date("2020-06-01")
t                  <- length(unique(as.integer(factor(data$week_id))))
i                  <- 3228
r                  <- 1:t

# Convert mesh to neighbourhoods
source("CrucialScripts_DONOTMOVE/st_mesh-spde-stk_objects_aggmod.R")

# ----- 1. Define relevant functions to be used -----
# Compute LS, MAE and MSPE on the original INLA results
inla_stats <- function(model){
  
  mean.deviance = model$dic$mean.deviance
  p.eff.dic     = model$dic$p.eff
  p.eff.waic    = model$waic$p.eff
  DIC           = model$dic$dic
  WAIC          = model$waic$waic
  
  
  index       <- inla.stack.index(stk, tag = "est")$data 
  eta         <- model$summary.fitted.values[index, "mean"]
  pred_counts <- eta * data$tot_pop
  
  MAE  <- mean(abs(data$y - pred_counts))
  MSPE <- mean((data$y - pred_counts)^2)
  LS = -mean(log(model$cpo$cpo))
  
  data.frame(mean.deviance = mean.deviance, 
             p.eff.waic = p.eff.waic,
             p.eff.dic  = p.eff.dic,
             
             WAIC = WAIC, 
             DIC = DIC, 
             
             LS = LS, 
             MAE = MAE, 
             MSPE = MSPE)
}

inla_stats(result)


# Compute LS, MAE and MSPE on the LOO/LGO Cross-Validation results
cv_stats <- function(model){
  
  cv_pred_probs <- 1 / (1 + exp(-model$mean))
  cv_pred_counts <- cv_pred_probs * data$tot_pop
  
  MAE  <- mean(abs(data$y - cv_pred_counts))
  MSPE <- mean(   (data$y - cv_pred_counts)^2)
  LS = -mean(log(model$cv))
  
  data.frame(LS = LS, MSPE = MSPE, MAE = MAE)
  }

# ----- 2. Perform Leave-One-Out Cross-Validation (LOOCV) on individual models
loocv.HospAgg3 <- inla.group.cv(HospAgg3, num.level.sets = -1)
saveRDS(loocv.HospAgg3, file = "loocvHospAgg3.rds")
loocv.HospAgg1 <- inla.group.cv(HospAgg1, num.level.sets = -1)
saveRDS(loocv.HospAgg1, file = "loocvHospAgg1.rds")
loocv.HospAgg5 <- inla.group.cv(HospAgg5, num.level.sets = -1)
saveRDS(loocv.HospAgg5, file = "loocvHospAgg5.rds")
loocv.HospBasic1 <- inla.group.cv(HospBasic1, num.level.sets = -1)
saveRDS(loocv.HospBasic1, file = "loocvHospBacis1.rds")
loocv.HospBasic2 <- inla.group.cv(HospBasic2, num.level.sets = -1)
saveRDS(loocv.HospBasic2, file = "loocvHospBasic2.rds")
loocv.HospBasic3 <- inla.group.cv(HospBasic3, num.level.sets = -1)
saveRDS(loocv.HospBasic3, file = "loocvHospBasic3.rds")
loocv.HospDag3       <- inla.group.cv(HospDag3, num.level.sets = -1)
saveRDS(loocv.HospDag3,       file = "loocvHospDag3.rds")

# ----- 3. Extract groups using a prior precision matrix strategy -----
lgocv.HospAgg3.prior <- inla.group.cv(result = HospAgg3, num.level.sets = 3, strategy = "prior")
grps.HospAgg3.prior <- lapply(lgocv.HospAgg3.prior$groups, function(x) x$idx)
saveRDS(grps.HospAgg3.prior, file = "groups_hosp_prior.rds")


# ----- 4. Extract groups using a posterior strategy -----
lgocv.HospAgg3.post <- inla.group.cv(result = HospAgg3, num.level.sets = 3, strategy = "posterior")
grps.HospAgg3.post <- lapply(lgocv.HospAgg3.post$groups, function(x) x$idx)
saveRDS(grps.HospAgg3.post, file = "groups_hosp_post.rds")


# ----- 5. Apply prior grouping strategy to other models -----
lgocv.HospAgg1.prior <- inla.group.cv(result = HospAgg1, groups = grps.HospAgg3.prior)
lgocv.HospAgg5.prior <- inla.group.cv(result = HospAgg5, groups = grps.HospAgg3.prior)

lgocv.HospBasic1.prior <- inla.group.cv(result = HospBasic1, groups = grps.HospAgg3.prior)
lgocv.HospBasic2.prior <- inla.group.cv(result = HospBasic2, groups = grps.HospAgg3.prior)
lgocv.HospBasic3.prior <- inla.group.cv(result = HospBasic3, groups = grps.HospAgg3.prior)

lgocv.HospDag3.prior <- inla.group.cv(result = HospDag3, groups = grps.HospAgg3.prior)
#saveRDS(lgocv.HospDag3.prior, file = "lgocv_HospDag3_prior.rds")

# ----- 6. Apply posterior grouping strategy to other models -----
lgocv.HospAgg1.post <- inla.group.cv(result = HospAgg1, groups = grps.HospAgg3.post)
lgocv.HospAgg5.post <- inla.group.cv(result = HospAgg5, groups = grps.HospAgg3.post)

lgocv.HospBasic1.post <- inla.group.cv(result = HospBasic1, groups = grps.HospAgg3.post)
lgocv.HospBasic2.post <- inla.group.cv(result = HospBasic2, groups = grps.HospAgg3.post)
lgocv.HospBasic3.post <- inla.group.cv(result = HospBasic3, groups = grps.HospAgg3.post)

lgocv.HospDag3.post  <- inla.group.cv(result = HospDag3, groups = grps.HospAgg3.post)
#saveRDS(lgocv.HospDag3.post,  file = "lgocv_HospDag3_post.rds")


# ----- 7. Combine results -----
# Call function on original INLA models
table_inla_stats <- round(do.call(rbind, lapply(list(
  HospBasic1 = HospBasic1,
  HospBasic2 = HospBasic2,
  HospBasic3 = HospBasic3,
  
  HospAgg3 = HospAgg3,
  HospAgg1 = HospAgg1,
  HospAgg5 = HospAgg5
), inla_stats)), 6)

print(table_inla_stats)
saveRDS(table_inla_stats, file = "20250902_INLA_stats_hosp.rds")

# Call function on LOO/LGO CV models
table_cv_stats <- round(do.call(rbind, lapply(list(
  
  
  loocv.HospBasic1       = loocv.HospBasic1,
  loocv.HospBasic2       = loocv.HospBasic2,
  loocv.HospBasic3       = loocv.HospBasic3,
  loocv.HospAgg3         = loocv.HospAgg3,
  loocv.HospAgg1         = loocv.HospAgg1,
  loocv.HospAgg5         = loocv.HospAgg5,
  loocv.HospDag3         = loocv.HospDag3,
  
  lgocv.HospBasic1.prior = lgocv.HospBasic1.prior,
  lgocv.HospBasic2.prior = lgocv.HospBasic2.prior,
  lgocv.HospBasic3.prior = lgocv.HospBasic3.prior,
  lgocv.HospAgg3.prior   = lgocv.HospAgg3.prior,
  lgocv.HospAgg1.prior   = lgocv.HospAgg1.prior,
  lgocv.HospAgg5.prior   = lgocv.HospAgg5.prior,  
  lgocv.HospDag3.prior   = lgocv.HospDag3.prior,
  
  lgocv.HospBasic1.post  = lgocv.HospBasic1.post,
  lgocv.HospBasic2.post  = lgocv.HospBasic2.post,
  lgocv.HospBasic3.post  = lgocv.HospBasic3.post,
  lgocv.HospAgg3.post    = lgocv.HospAgg3.post,
  lgocv.HospAgg1.post    = lgocv.HospAgg1.post,
  lgocv.HospAgg5.post    = lgocv.HospAgg5.post,
  lgocv.HospDag3.post    = lgocv.HospDag3.post
), cv_stats)), 6)

table_cv_stats
#saveRDS(table_cv_stats, file = paste0(sys.Date(), "_LOOLGOCV_stats_hosp.rds"))










# ----- 1. Repeat the process for infection models -----
# Specify values
data               <- Cases.nb.wk %>% filter(week_begin > as.Date("2020-05-25"))
data$y             <- data$case_obs
result             <- InfsAgg3
week_begin         <- as.Date("2020-06-01")
t                  <- length(unique(as.integer(factor(data$week_id))))
i                  <- 3228
r                  <- 1:t

# Convert mesh to neighbourhoods
source("CrucialScripts_DONOTMOVE/st_mesh-spde-stk_objects_aggmod.R")


# ----- 2. Perform Leave-One-Out Cross-Validation (LOOCV) on individual models
# Aggregated model
loocv.InfsAgg3 <- inla.group.cv(InfsAgg3, num.level.sets = -1)
saveRDS(loocv.InfsAgg3, file = "loocv_InfsAgg3.rds")
# Disaggregated model
loocv.InfsDag3 <- inla.group.cv(InfsDag3, num.level.sets = -1)
saveRDS(loocv.InfsDag3, file = "loocv_InfsDag3.rds")


# ----- 3. Extract groups using a prior precision matrix strategy -----
lgocv.InfsAgg3.prior <- inla.group.cv(result = InfsAgg3, num.level.sets = 3, strategy = "prior")
grps.InfsAgg3.prior  <- lapply(lgocv.InfsAgg3.prior$groups, function(x) x$idx)
saveRDS(grps.InfsAgg3.prior, file = "groups_infs_prior.rds")


# ----- 4. Extract groups using a posterior strategy -----
lgocv.InfsAgg3.post <- inla.group.cv(result = InfsAgg3, num.level.sets = 3, strategy = "posterior")
grps.InfsAgg3.post  <- lapply(lgocv.InfsAgg3.post$groups, function(x) x$idx)
saveRDS(grps.InfsAgg3.post, file = "groups_infs_post.rds")


# ----- 5. Apply prior grouping strategy to other models -----
lgocv.InfsDag3.prior <- inla.group.cv(result = InfsDag3, groups = grps.InfsAgg3.prior)


# ----- 6. Apply posterior grouping strategy to other models -----
lgocv.InfsDag3.post <- inla.group.cv(result = InfsDag3, groups = grps.InfsAgg3.prior)


# ----- 7. Combine results -----
# Call function on original INLA models
table_inla_stats <- round(do.call(rbind, lapply(list(
  InfsAgg3 = InfsAgg3
  #InfsDag3 = InfsDag3
), inla_stats)), 6)

print(table_inla_stats)
saveRDS(table_inla_stats, file = "20250908_INLA_stats_infs.rds")

# Call function on LOO/LGO CV models
table_cv_stats <- round(do.call(rbind, lapply(list(
  loocv.InfsAgg3       = loocv.InfsAgg3,
  loocv.InfsDag3       = loocv.InfsDag3,
  lgocv.InfsAgg3.prior = lgocv.InfsAgg3.prior,
  lgocv.InfsDag3.prior = lgocv.InfsDag3.prior,
  lgocv.InfsAgg3.post  = lgocv.InfsAgg3.post,
  lgocv.InfsDag3.post  = lgocv.InfsDag3.post
  ), cv_stats)), 6)

table_cv_stats
saveRDS(table_cv_stats, file = "20250908_LOOLGOCV_stats_infs.rds")



