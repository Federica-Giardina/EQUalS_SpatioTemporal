# Import libraries
#libs <- c("")
#lapply(libs, library, character.only = TRUE)

# Set working directory
setwd("H:\\Carsten - First glance at data")

# DATA IMPORT AND PREP
source("CrucialScripts_DONOTMOVE/DataPrep.R")
source("CrucialScripts_DONOTMOVE/Mob_Thresholds.R")

InfsAgg3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_InfsAgg3.rds")
InfsDag3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_InfsDag3.rds")

HospAgg3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg3.rds")
HospDag3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospDag3.rds")

HospAgg1     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg1.rds")
HospAgg5     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg5_post.rds")


# ----- 0. Explanation of process... -----
# 1. Posterior sampling was not available in the DAG models.
#    Therfore, we decided to stick with the posterior mean:
#    B_t = B + y_t, whereas OR_t = exp(B_t)
# 2. Since we cannot base the 95% CrI on the sampled posteriors,
#    we compute the 95% CrI using INLA marginal distributions


# ----- 1. Define covariates, corresponding random effect variables -----
covs <- c("SES_HH_Q1_prop_stand",
          "MBG_group_Antilles_prop_stand",
          "MBG_group_Marocco_prop_stand",
          "MBG_group_Surinam_prop_stand",
          "MBG_group_Turkiye_prop_stand",
          "MBG_group_Other_W_prop_stand",
          "MBG_group_Other_nW_prop_stand"
          )

res  <- list("SES_HH_Q1_prop_stand"          = "week_SES", 
             "MBG_group_Antilles_prop_stand" = "week_MBG1",
             "MBG_group_Marocco_prop_stand"  = "week_MBG2",
             "MBG_group_Surinam_prop_stand"  = "week_MBG3",
             "MBG_group_Turkiye_prop_stand"  = "week_MBG4",
             "MBG_group_Other_W_prop_stand"  = "week_MBG5",
             "MBG_group_Other_nW_prop_stand" = "week_MBG6"
          )


# ----- 2. Select correct model -----
#result <- HospDag3
result <- InfsDag3


# ----- 3. Extract combined fixed and random effects
results.list <- list()
  
for (cov in covs) {
  
  # 1) Fixed effects
  fe.mean <- result$summary.fixed[cov, "mean"]
  
  # 2) Random effects
  re.name <- res[[cov]]
  
  re.mean <- result$summary.random[[re.name]]$mean %>%
    as.data.frame() %>%
    mutate(week_id = rep(1:nrow(result$summary.random[[re.name]])),
           week_id = as.integer(week_id)) %>%
    rename(re.mean = ".")
  
  df <- re.mean %>%
    mutate(fi_re.mean = fe.mean + re.mean,
           OR.mean    = exp(fi_re.mean),
           covariate  = cov)
  
  results.list[[cov]] <- df
  
  }

# ----- 45. Combine datasets
TVA.cov.InfsDag3 <- do.call(rbind, results.list)
TVA.cov.HospDag3 <- do.call(rbind, results.list)

# ----- 5. Write data to .csv files -----
#write.csv(TVA.cov.InfsDag3, paste0(format(Sys.Date(), "%Y%m%d"), "_TVA.cov.InfsDag3_", type, ".csv"))
#write.csv(TVA.cov.HospDag3, paste0(format(Sys.Date(), "%Y%m%d"), "_TVA.cov.HospDag3_", type, ".csv"))

TVA.cov.InfsDag3 <- read_csv("H:/Carsten - First glance at data/Manuscript/SupFig7_TVA_cov_DAG/SupFig7_Data_TVA_cov_Dag_infs.csv")
TVA.cov.HospDag3 <- read_csv("H:/Carsten - First glance at data/Manuscript/SupFig7_TVA_cov_DAG/SupFig7_Data_TVA_cov_Dag_hosp.csv")


# ----- 6. Plot ----- 
# Hosps: as.Date("2020-01-06")
# Infs:  as.Date("2020-06-01")

# Match dates to week_id and join corresponding dataset
TVA.cov.HospDag3 <- 
  data.frame(date = as.Date("2020-01-06")) %>%
  complete(  date    = seq.Date(as.Date("2020-01-06"), as.Date("2021-12-27"), by = "week")) %>%
  mutate(    week_id = seq(1:length(unique(as.integer(factor(Cases.nb.wk$week_id)))))) %>% 
  left_join(TVA.cov.HospDag3, by = c("week_id" = "week_id")) %>% 
  mutate(model = "HospDag3")
  
data <- Cases.nb.wk %>% filter(week_begin > as.Date("2020-05-25")) 
TVA.cov.InfsDag3 <- 
  data.frame(date    = as.Date("2020-06-01")) %>%
  complete(  date    = seq.Date(as.Date("2020-06-01"), as.Date("2021-12-27"), by = "week")) %>%
  mutate(    week_id = seq(1:length(unique(as.integer(factor(data$week_id)))))) %>% 
  left_join(TVA.cov.InfsDag3, by = c("week_id" = "week_id"))%>% 
  mutate(model = "InfsDag3")

TVA.cov.com      <- rbind(TVA.cov.HospDag3, TVA.cov.InfsDag3) 

# Redefine factor levels
TVA.cov.com <- TVA.cov.com %>%
  mutate(covariate = case_when(covariate == "SES_HH_Q1_prop_stand"          ~ "low-SES",
                               covariate == "MBG_group_Antilles_prop_stand" ~ "Antilles",
                               covariate == "MBG_group_Marocco_prop_stand"  ~ "Marocco",
                               covariate == "MBG_group_Surinam_prop_stand"  ~ "Surinam",
                               covariate == "MBG_group_Turkiye_prop_stand"  ~ "Turkey",
                               covariate == "MBG_group_Other_W_prop_stand"  ~ "Other - Western",
                               covariate == "MBG_group_Other_nW_prop_stand" ~ "Other - non-Western"),
         covariate = factor(covariate, levels = c("low-SES", "Antilles", "Marocco", "Surinam", "Turkey", "Other - Western", "Other - non-Western")),
         model     = case_when(model == "InfsDag3" ~ "Infections model",
                               model == "HospDag3" ~ "Hospitalisations model"),
         model     = factor(model, levels = c("Infections model", "Hospitalisations model")))

# Specify colour palettes
## Full palette
custom_palette <- c("#8E44AD", "#17bebb", "#8BC061",  "#FFC107", "#F28220", "#e54339") #"#3498DB",
# Subset palette
custom_palette <- c("#FFC107", "#CC4C02")

# If applicable: filter for specific variables
plot <- TVA.cov.com %>% filter(
  #covariate == "low-SES"
  #covariate == "Antilles",
  #covariate == "Marocco",
  #covariate == "Surinam",
  #covariate == "Turkey",
  #covariate == "Other - Western",
  #covariate == "Other - non-Western"
  #model == "Infections model" & covariate != "low-SES",
  model == "Hospitalisations model" & covariate != "low-SES"
  )

# Plot
ggplot(plot, aes(x = date, y = OR.mean, colour = covariate)) + # OR colour = model or covariate
  geom_point(aes(fill = covariate), shape = 18, size = .1) +   # OR fill = model or covariate
  geom_line(aes(), linewidth = .61, alpha = .71) +
  scale_colour_manual(values = custom_palette) +
  scale_fill_manual(values = custom_palette) +   
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y", limits = c(as.Date("2020-01-06"), as.Date("2022-01-08")), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 10, .05), minor_breaks = seq(0, 10, .0125)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = rel(1.00)),
        legend.title = element_text(size = rel(1.00), face = "bold"),
        axis.text.x = element_text(size = rel(1.25)),
        axis.text.y = element_text(size = rel(1.25)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = rel(1.00)),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left = element_line(colour = "black"),
        plot.tag = element_text(face = "bold")) +
  
  labs(title = "", x = "", y = "Odds Ratio", colour   = "Model", fill     = "Model", shape    = "Model", linetype = "Model", tag = "b") +
  
  geom_hline(yintercept = 1.00, colour = "grey", linetype = 1, linewidth = 1) +
  geom_vline(xintercept = as.Date("2020-06-29"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2020-11-30"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-02-01"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-07-05"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2021-10-04"), colour = "black", linetype = 2, linewidth = 0.5) +
  
  annotate("text", x = as.Date("2020-04-20"), y = max(plot$OR.mean) + 0.125*max(plot$OR.mean), label = "Period 1",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2020-09-15"), y = max(plot$OR.mean) + 0.125*max(plot$OR.mean), label = "Period 2a", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-01-01"), y = max(plot$OR.mean) + 0.125*max(plot$OR.mean), label = "Period 2b", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-04-18"), y = max(plot$OR.mean) + 0.125*max(plot$OR.mean), label = "Period 3",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-08-18"), y = max(plot$OR.mean) + 0.125*max(plot$OR.mean), label = "Period 4",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-11-20"), y = max(plot$OR.mean) + 0.125*max(plot$OR.mean), label = "Period 5",  colour = "black", fontface = "bold") 


# Save plot
#ggname <- "SupFig7a_TVA_cov_Infs_Dag3.jpeg"
#ggname <- "SupFig7b_TVA_cov_Hosp_Dag3.jpeg"
#ggname <- "SupFig7c_TVA_cov_lowSES_Dag3.jpeg"
#ggname <- "SupFig7d_TVA_cov_Antilles_Dag3.jpeg"
#ggname <- "SupFig7e_TVA_cov_Marocco_Dag3.jpeg"
#ggname <- "SupFig7f_TVA_cov_Surinam_Dag3.jpeg"
#ggname <- "SupFig7g_TVA_cov_Turkey_Dag3.jpeg"
#ggname <- "SupFig7h_TVA_cov_OtherW_Dag3.jpeg"
#ggname <- "SupFig7i_TVA_cov_OthernW_Dag3.jpeg"

ggsave(ggname, 
       width = 15,
       height = 6.5,
       units = "in",
       dpi = 600)

























# ---- 2. Extract covariate-specific random effects -----
re.names <- names(HospDag3$summary.fixed)
re.









































# Specify values
data               <- Cases.nb.wk #%>% filter(week_begin > as.Date("2020-05-25")) # Remove filter for hospitalisations
data$y             <- data$hosp_obs # case_obs OR hosp_obs
type1              <- "HospAgg1"
type2              <- "HospAgg5"
result1            <- HospAgg1
result2            <- HospAgg5

week_begin         <- as.Date("2020-01-06") # Hosps: as.Date("2020-01-06") OR Infs: as.Date("2020-06-01")
t                  <- length(unique(as.integer(factor(data$week_id))))
i                  <- 3228
r                  <- 1:t
custom_palette     <- c("#17bebb", "#F28220", "#FFC107", "#e54339")
custom_palette.rev <- rev(custom_palette)



# ----- 1. Draw or read posterior samples -----
#post.sample     <- inla.posterior.sample(100, result)
PS.HospAgg1 <- readRDS("RESULTS JULY 2025/00_ModRes/Post_Samples/PS_HospAgg1.rds")
PS.HospAgg5 <- readRDS("RESULTS JULY 2025/00_ModRes/Post_Samples/PS_HospAgg5.rds")
PS.HospAgg3 <- readRDS("RESULTS JULY 2025/00_ModRes/Post_Samples/PS_HospAgg3.rds")
PS.InfsAgg3 <- readRDS("RESULTS JULY 2025/00_ModRes/Post_Samples/PS_infsAgg3.rds")

PS_SES.HospAgg1 <- inla.posterior.sample.eval(function(xx) week_SES + SES_HH_Q1_prop_stand, PS.HospAgg1)
PS_SES.HospAgg5 <- inla.posterior.sample.eval(function(xx) week_SES + SES_HH_Q1_prop_stand, PS.HospAgg5)
PS_SES.HospAgg3 <- inla.posterior.sample.eval(function(xx) week_SES + SES_HH_Q1_prop_stand, PS.HospAgg3)
PS_SES.InfsAgg3 <- inla.posterior.sample.eval(function(xx) week_SES + SES_HH_Q1_prop_stand, PS.InfsAgg3)

PS_MBG.HospAgg1 <- inla.posterior.sample.eval(function(xx) week_MBG  + MBG_M_prop_stand, PS.HospAgg1)
PS_MBG.HospAgg5 <- inla.posterior.sample.eval(function(xx) week_MBG  + MBG_M_prop_stand, PS.HospAgg5)
PS_MBG.HospAgg3 <- inla.posterior.sample.eval(function(xx) week_MBG  + MBG_M_prop_stand, PS.HospAgg3)
PS_MBG.InfsAgg3 <- inla.posterior.sample.eval(function(xx) week_MBG  + MBG_M_prop_stand, PS.InfsAgg3)


# ----- 2. Summarise results over rows (iterate over posterior samples drawn) -----
# Socioeconomic status
PS_SES.res1 <- data.frame(
  week_id = 1:nrow(PS_SES.HospAgg1),
  median  = apply(PS_SES.HospAgg1, 1, median),
  CrI.low = apply(PS_SES.HospAgg1, 1, quantile, probs = 0.025),
  CrI.upp = apply(PS_SES.HospAgg1, 1, quantile, probs = 0.975))

PS_SES.res2 <- data.frame(
  week_id = 1:nrow(PS_SES.HospAgg5),
  median  = apply(PS_SES.HospAgg5, 1, median),
  CrI.low = apply(PS_SES.HospAgg5, 1, quantile, probs = 0.025),
  CrI.upp = apply(PS_SES.HospAgg5, 1, quantile, probs = 0.975))

# Migration background
PS_MBG.res1 <- data.frame(
  week_id = 1:nrow(PS_MBG.HospAgg1),
  median  = apply(PS_MBG.HospAgg1, 1, median),
  CrI.low = apply(PS_MBG.HospAgg1, 1, quantile, probs = 0.025),
  CrI.upp = apply(PS_MBG.HospAgg1, 1, quantile, probs = 0.975))

PS_MBG.res2 <- data.frame(
  week_id = 1:nrow(PS_MBG.HospAgg5),
  median  = apply(PS_MBG.HospAgg5, 1, median),
  CrI.low = apply(PS_MBG.HospAgg5, 1, quantile, probs = 0.025),
  CrI.upp = apply(PS_MBG.HospAgg5, 1, quantile, probs = 0.975))

# ----- 3. Convert to Odds Ratios (exponentiate)  -----
# Socioeconomic status
PS_SES.res1 <- PS_SES.res1 %>%
  mutate(OR.med     = exp(median),
         OR.CrI.low = exp(CrI.low),
         OR.CrI.upp = exp(CrI.upp),
         Group      = factor(paste0(type1, " - SES")))

PS_SES.res2 <- PS_SES.res2 %>%
  mutate(OR.med     = exp(median),
         OR.CrI.low = exp(CrI.low),
         OR.CrI.upp = exp(CrI.upp),
         Group      = factor(paste0(type2, " - SES")))

# Migration background
PS_MBG.res1 <- PS_MBG.res1 %>%
  mutate(OR.med     = exp(median),
         OR.CrI.low = exp(CrI.low),
         OR.CrI.upp = exp(CrI.upp),
         Group      = factor(paste0(type1, " - Migration")))

PS_MBG.res2 <- PS_MBG.res2 %>%
  mutate(OR.med     = exp(median),
         OR.CrI.low = exp(CrI.low),
         OR.CrI.upp = exp(CrI.upp),
         Group      = factor(paste0(type2, " - Migration")))


# ----- 4. Combine datasets and group -----
# Create template
df.date <- data.frame(date = week_begin) %>%
  complete(date   = seq.Date(week_begin, as.Date("2021-12-27"), by = "week")) %>%
  mutate(week_id  = seq(1:t))
# SES result 1
plot_SES.res1 <- df.date %>% 
  left_join(PS_SES.res1, by = c("week_id" = "week_id"))
# SES result 2
plot_SES.res2 <- df.date %>% 
  left_join(PS_SES.res2, by = c("week_id" = "week_id"))
# MBG result 1
plot_MBG.res1 <- df.date %>% 
  left_join(PS_MBG.res1, by = c("week_id" = "week_id"))
# MBG result 2
plot_MBG.res2 <- df.date %>% 
  left_join(PS_MBG.res2, by = c("week_id" = "week_id"))
# Bind resulting datasets
plot <- rbind(plot_SES.res1, plot_SES.res2, plot_MBG.res1, plot_MBG.res2)

# Write resulting dataset to a .csv file
#write.csv(plot, paste0(format(Sys.Date(), "%Y%m%d"), "_TVA_cov_", type1, "_and_", type2, ".cvs")) 


















# ----- 5. Plot over time ----- 


# Specify palette for Figure
custom_colour     <- c("#FFC107", "#FFC107", "#F28220", "#F28220")


# Plot results
ggplot(data = plot, aes(x = date, y = OR.med, colour = Group)) + 
  geom_point(aes(colour = Group, fill = Group, shape = Group)) + 
  geom_line(aes(colour = Group, linetype = Group), linewidth = .61, alpha = .71) + 
  geom_ribbon(aes(ymin = OR.CrI.low, ymax = OR.CrI.upp, fill = Group), colour  = NA, alpha   = .3) +
  
  scale_colour_manual(values      = custom_colour) +
  scale_fill_manual(  values      = custom_colour) +
  
  
  scale_x_date(       date_breaks = "1 month",
                      date_labels = "%b %y", 
                      limits      = c(week_begin, as.Date("2022-01-08")), 
                      expand      = c(0,0)) +
  
  scale_y_continuous(breaks      = seq(0, 10, .05),
                     minor_breaks = seq(0, 10, .025)) +  
  
  theme_minimal() + 
  
  theme(legend.position    = "bottom",
        legend.text        = element_text(size = rel(1.00)),
        legend.title       = element_text(size = rel(1.00), 
                                          face = "bold"),
        axis.text.x        = element_text(size = rel(1.25)),
        axis.text.y        = element_text(size = rel(1.25)),
        axis.title.x       = element_blank(),
        axis.title.y       = element_text(size = rel(1.00)),
        
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left   = element_line(colour = "black")) +
  
  labs(title    = "", x      = "", 
       y        = "Median and 95% CrI (OR)", 
       colour   = "Model - Covariate",
       fill     = "Model - Covariate",
       shape    = "Model - Covariate",
       linetype = "Model - Covariate") +
  
  geom_vline(xintercept = as.Date("2020-06-29"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2020-11-30"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-02-01"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-07-05"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2021-10-04"), colour = "black", linetype = 2, linewidth = 0.5)









ggname <- paste0("tva_cov_", type1, "_and_", type2, ".jpeg")


ggsave(ggname, 
       width = 15,
       height = 6.5,
       units = "in",
       dpi = 600,
       #compression = "lzw"
)

