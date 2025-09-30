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



#plot <- read_csv("H:/Carsten - First glance at data/Manuscript/SupFig8_TVA_cov_SensAnalyisis/SupFig8_data_TVA_cov_HospAgg1_and_HospAgg5.csv")
#plot <- read.csv("C:/Users/Z710214/OneDrive - Radboudumc/z710214/My Documents/PhD/S5 - EQUalS/Writing (Carsten)/20250922_Final_export/SupFig8_TVA_cov_SensAnalyisis/SupFig8_Data_TVA_cov_HospAgg1_and_HospAgg5.csv")

plot <- plot %>%
  mutate(Group = 
           factor(case_when(Group == "HospAgg1 - SES" ~ "Sensitivity analysis 1 - Low-SES",
                            Group == "HospAgg5 - SES" ~ "Sensitivity analysis 2 - Low-SES",
                            Group == "HospAgg1 - Migration" ~ "Sensitivity analysis 1 - Migration",
                            Group == "HospAgg5 - Migration" ~ "Sensitivity analysis 2 - Migration"),
                  levels = c("Sensitivity analysis 1 - Low-SES",
                             "Sensitivity analysis 2 - Low-SES",
                             "Sensitivity analysis 1 - Migration",
                             "Sensitivity analysis 2 - Migration")))


# ----- 5. Plot over time ----- 
# Specify palette for Figure
custom_colour     <- c("#FFC107", "#FFC107", "#CC4C02", "#CC4C02")


# Plot results
ggplot(data = plot, aes(x = as.Date(date), y = OR.med, colour = Group)) + 
  geom_point(aes(colour = Group, fill = Group, shape = Group), size = 0.1) + 
  geom_line(aes(colour = Group, linetype = Group), linewidth = .61, alpha = .71) + 
  geom_ribbon(aes(ymin = OR.CrI.low, ymax = OR.CrI.upp, fill = Group), colour  = NA, alpha   = .3) +
  
  scale_colour_manual(values      = custom_colour) +
  scale_fill_manual(  values      = custom_colour) +
  

  scale_x_date(       date_breaks = "2 month",
                      date_labels = "%b %y", 
                      limits      = c(as.Date("2020-01-01"), as.Date("2022-01-08")), 
                      expand      = c(0,0)) +
 
  scale_y_continuous(breaks      = seq(0, 10, .1)) +  
  
  theme_minimal() + 
  
  theme(legend.position    = "bottom",
        legend.text        = element_text(size = 14),
        legend.title       = element_text(size = 14, 
                                          face = "bold"),
        axis.text.x        = element_text(size = 12),
        axis.text.y        = element_text(size = 14),
        axis.title.x       = element_blank(),
        axis.title.y       = element_text(size = 12),
        
        axis.line.x.bottom = element_line(colour = "black"),
        axis.line.y.left   = element_line(colour = "black"),
        ) +
  
  labs(title    = "", x      = "", 
       y        = "OR, Median and 95% CrI", 
       colour   = "Model - Covariate",
       fill     = "Model - Covariate",
       shape    = "Model - Covariate",
       linetype = "Model - Covariate") +
  
  geom_hline(yintercept = 1.00, colour = "grey", linetype = 1, linewidth = 1) +
  geom_vline(xintercept = as.Date("2020-06-29"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2020-11-30"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-02-01"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-07-05"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2021-10-04"), colour = "black", linetype = 2, linewidth = 0.5) +
  
  annotate("text", x = as.Date("2020-04-20"), y = max(plot$OR.med) + 0.125*max(plot$OR.med), label = "Period 1",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2020-09-15"), y = max(plot$OR.med) + 0.125*max(plot$OR.med), label = "Period 2a", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-01-01"), y = max(plot$OR.med) + 0.125*max(plot$OR.med), label = "Period 2b", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-04-18"), y = max(plot$OR.med) + 0.125*max(plot$OR.med), label = "Period 3",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-08-18"), y = max(plot$OR.med) + 0.125*max(plot$OR.med), label = "Period 4",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-11-20"), y = max(plot$OR.med) + 0.125*max(plot$OR.med), label = "Period 5",  colour = "black", fontface = "bold") 


#ggname <- "SupFig11_TVA_cov_SensAnalysis.jpeg"
ggsave(ggname, 
       width = 15,
       height = 6.5,
       units = "in",
       dpi = 600)

