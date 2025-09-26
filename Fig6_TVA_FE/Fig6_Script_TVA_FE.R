# Import libraries
#libs <- c("")
#lapply(libs, library, character.only = TRUE)

# Set working directory
setwd("H:\\Carsten - First glance at data")

# DATA IMPORT AND PREP
source("CrucialScripts_DONOTMOVE/DataPrep.R")
source("CrucialScripts_DONOTMOVE/Mob_Thresholds.R")

InfsAgg3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_InfsAgg3.rds")
HospAgg3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg3.rds")

# Specify values
data               <- Cases.nb.wk #%>% filter(week_begin > as.Date("2020-05-25")) # Remove filter for hospitalisations
data$y             <- data$hosp_obs # case_obs OR hosp_obs
type1              <- "InfsAgg3"
type2              <- "HospAgg3"
result1            <- InfsAgg3
result2            <- HospAgg3

week_begin         <- as.Date("2020-01-06") # Hosps: as.Date("2020-01-06") OR Infs: as.Date("2020-06-01")
t                  <- length(unique(as.integer(factor(data$week_id))))
i                  <- 3228
r                  <- 1:t
custom_palette     <- c("#17bebb", "#F28220", "#FFC107", "#e54339")
custom_palette.rev <- rev(custom_palette)


# ----- 1. Draw or read posterior samples -----
#post.sample     <- inla.posterior.sample(100, result)
PS.InfsAgg3 <- readRDS("RESULTS JULY 2025/00_ModRes/Post_Samples/PS_InfsAgg3.rds")
PS.HospAgg3 <- readRDS("RESULTS JULY 2025/00_ModRes/Post_Samples/PS_HospAgg3.rds")

# SES: B_ses + y_ses
PS_SES.InfsAgg3 <- inla.posterior.sample.eval(function(xx) week_SES + SES_HH_Q1_prop_stand, PS.InfsAgg3)
PS_SES.HospAgg3 <- inla.posterior.sample.eval(function(xx) week_SES + SES_HH_Q1_prop_stand, PS.HospAgg3)

# MBG: B_mbg + y_mbg
PS_MBG.InfsAgg3 <- inla.posterior.sample.eval(function(xx) week_MBG  + MBG_M_prop_stand, PS.InfsAgg3)
PS_MBG.HospAgg3 <- inla.posterior.sample.eval(function(xx) week_MBG  + MBG_M_prop_stand, PS.HospAgg3)


# ----- 2. Summarise results over rows (iterate over posterior samples drawn) -----
# Socioeconomic status
PS_SES.res1 <- data.frame(
  week_id = 1:nrow(PS_SES.InfsAgg3),
  median  = apply(PS_SES.InfsAgg3, 1, median),
  CrI.low = apply(PS_SES.InfsAgg3, 1, quantile, probs = 0.025),
  CrI.upp = apply(PS_SES.InfsAgg3, 1, quantile, probs = 0.975))

PS_SES.res2 <- data.frame(
  week_id = 1:nrow(PS_SES.HospAgg3),
  median  = apply(PS_SES.HospAgg3, 1, median),
  CrI.low = apply(PS_SES.HospAgg3, 1, quantile, probs = 0.025),
  CrI.upp = apply(PS_SES.HospAgg3, 1, quantile, probs = 0.975))

# Migration background
PS_MBG.res1 <- data.frame(
  week_id = 1:nrow(PS_MBG.InfsAgg3),
  median  = apply(PS_MBG.InfsAgg3, 1, median),
  CrI.low = apply(PS_MBG.InfsAgg3, 1, quantile, probs = 0.025),
  CrI.upp = apply(PS_MBG.InfsAgg3, 1, quantile, probs = 0.975))

PS_MBG.res2 <- data.frame(
  week_id = 1:nrow(PS_MBG.HospAgg3),
  median  = apply(PS_MBG.HospAgg3, 1, median),
  CrI.low = apply(PS_MBG.HospAgg3, 1, quantile, probs = 0.025),
  CrI.upp = apply(PS_MBG.HospAgg3, 1, quantile, probs = 0.975))
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
df.date.res1 <- data.frame(date = as.Date("2020-06-01")) %>%
  complete(date  = seq.Date(as.Date("2020-06-01"), as.Date("2021-12-27"), by = "week")) %>%
  mutate(week_id = seq(1:83))

df.date.res2 <- data.frame(date = as.Date("2020-01-06")) %>%
  complete(date  = seq.Date(as.Date("2020-01-06"), as.Date("2021-12-27"), by = "week")) %>%
  mutate(week_id = seq(1:104))

# SES and MBG - result 1 (infections)
plot_SES.res1 <- df.date.res1 %>% left_join(PS_SES.res1, by = c("week_id" = "week_id"))
plot_MBG.res1 <- df.date.res1 %>% left_join(PS_MBG.res1, by = c("week_id" = "week_id"))
# SES and MBG - result 2 (hospitalisations) 
plot_SES.res2 <- df.date.res2 %>% left_join(PS_SES.res2, by = c("week_id" = "week_id"))
plot_MBG.res2 <- df.date.res2 %>% left_join(PS_MBG.res2, by = c("week_id" = "week_id"))
# Bind resulting datasets
plot <- rbind(plot_SES.res1, plot_SES.res2, plot_MBG.res1, plot_MBG.res2)

# Write resulting dataset to a .csv file
#write.csv(plot, paste0(format(Sys.Date(), "%Y%m%d"), "_TVA_cov_", type1, "_and_", type2, ".csv")) 



plot <- read.csv("Manuscript/Fig6_TVA_FE/Fig6_Data_TVA_cov_InfsAgg3_and_HospAgg3.csv")

# ----- 5. Plot over time ----- 
# Specify covariate to plot
plot.f <- plot %>% 
  filter(Group == "InfsAgg3 - SES" | Group == "HospAgg3 - SES") %>% 
  mutate(Group = case_when(Group == "InfsAgg3 - SES" ~ "Infections", 
                           Group == "HospAgg3 - SES" ~ "Hospitalisations"))

plot.f <- plot %>% 
  filter(Group == "InfsAgg3 - Migration" | Group == "HospAgg3 - Migration") %>%
  mutate(Group = case_when(Group == "InfsAgg3 - Migration" ~ "Infections", 
                           Group == "HospAgg3 - Migration" ~ "Hospitalisations"))

# Specify palette for Figure
custom_colour <- c("#8E44AD", "#17bebb")   
custom_colour <- c("#CC4C02", "#FFC107")   


# Plot results
ggplot(data = plot.f, aes(x = as.Date(date), y = OR.med, colour = Group)) + 
  geom_point(aes(colour = Group, fill = Group), size = .1) + #, shape = Group
  geom_line(aes(colour = Group), linewidth = .61, alpha = .71) + #, linetype = Group
  geom_ribbon(aes(ymin = OR.CrI.low, ymax = OR.CrI.upp, fill = Group), colour  = NA, alpha   = .3) +
  
  scale_colour_manual(values      = custom_colour) +
  scale_fill_manual(  values      = custom_colour) +
  
  
  scale_x_date(       date_breaks = "1 month",
                      date_labels = "%b %y", 
                      limits      = c(as.Date("2020-01-06"), as.Date("2022-01-08")), 
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
        axis.line.y.left   = element_line(colour = "black"),
        
        plot.tag = element_text(face = "bold")) +
  
  labs(title    = "", x      = "", 
       y        = "Median and 95% CrI (OR)", 
       colour   = "Model",
       fill     = "Model",
       shape    = "Model",
       linetype = "Model",
       tag = "b") +
  
  geom_hline(yintercept = 1.00, colour = "grey", linetype = 1, linewidth = 1) +
  geom_vline(xintercept = as.Date("2020-06-29"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2020-11-30"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-02-01"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-07-05"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2021-10-04"), colour = "black", linetype = 2, linewidth = 0.5) +
  
  annotate("text", x = as.Date("2020-04-20"), y = max(plot.f$OR.med) + 0.125*max(plot.f$OR.med), label = "Period 1",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2020-09-15"), y = max(plot.f$OR.med) + 0.125*max(plot.f$OR.med), label = "Period 2a", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-01-01"), y = max(plot.f$OR.med) + 0.125*max(plot.f$OR.med), label = "Period 2b", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-04-18"), y = max(plot.f$OR.med) + 0.125*max(plot.f$OR.med), label = "Period 3",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-08-18"), y = max(plot.f$OR.med) + 0.125*max(plot.f$OR.med), label = "Period 4",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-11-20"), y = max(plot.f$OR.med) + 0.125*max(plot.f$OR.med), label = "Period 5",  colour = "black", fontface = "bold") 

# ----- 6. Save plots -----
ggname <- paste0("Fig6a_TVA_cov_HospAgg3_and_InfsAgg3_SES.jpeg")
ggname <- paste0("Fig6b_TVA_cov_HospAgg3_and_InfsAgg3_MBG.jpeg")

ggsave(ggname, width = 15, height = 6.5, units = "in", dpi = 600)
























