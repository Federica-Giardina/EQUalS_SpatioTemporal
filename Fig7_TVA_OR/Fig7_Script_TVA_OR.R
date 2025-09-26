# Import libraries
#libs <- c("")
#lapply(libs, library, character.only = TRUE)

# Set working directory
setwd("H:\\Carsten - First glance at data")

# DATA IMPORT AND PREP
source("CrucialScripts_DONOTMOVE/DataPrep.R")
source("CrucialScripts_DONOTMOVE/Mob_Thresholds.R")

# Models
InfsAgg3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_InfsAgg3.rds")
HospAgg3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg3.rds")
HospAgg1     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg1.rds")
HospAgg5     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg5_post.rds")

# Specify values
data               <- Cases.nb.wk #%>% filter(week_begin > as.Date("2020-05-25")) # Remove filter for hospitalisations
data$y             <- data$hosp_obs # case_obs OR hosp_obs
type               <- "HospAgg3"
result             <- HospAgg3
week_begin         <- as.Date("2020-01-06") # Hosps: as.Date("2020-01-06") OR Infs: as.Date("2020-06-01")
t                  <- length(unique(as.integer(factor(data$week_id))))
i                  <- 3228
r                  <- 1:t
custom_palette     <- c("#17bebb", "#F28220", "#FFC107", "#e54339")
custom_palette.rev <- rev(custom_palette)

# Convert mesh to neighbourhoods
source("CrucialScripts_DONOTMOVE/Extr_SpatTemp_RandEff.R")





# ----- 1. Define quantiles (i = n, t = 1) -----
quintiles.df <- data  %>% 
  # Select random
  filter(week_id == 83) %>%
  # Summarise SES, MBG and Age into quartiles
  summarise(SES_HH_Q1_prop_stand = quantile(SES_HH_Q1_prop_stand, probs = c(.25, .5, .75)),
            MBG_M_prop_stand     = quantile(MBG_M_prop_stand,     probs = c(.25, .5, .75)),
            Age_65.00_prop_stand = quantile(Age_65.00_prop_stand, probs = c(.25, .5, .75)))


# ----- 2. Extract temporal random effects -----
df.rand_eff.t <- 
  result$summary.random$week_idx %>% 
  select(ID, mean)


# ----- 3. Extract spatiotemporal random effects -----
# Projected from spde/mesh model component; use R-script sourced previously
df.rand_eff.st.wk <- 
  df.rand_eff.st %>% 
  group_by(week_id) %>% 
  summarise(mean = mean(st_rand_eff, na.rm = TRUE))


# ----- 4. Draw or read posterior samples -----
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


# ----- 5. Define function for calculating linear predictor to specify time-varying associations -----
# Change to model used!
#post.sample.SES <- PS_SES.HospAgg3
#post.sample.MBG <- PS_MBG.HospAgg3

# Define function
fun_lin.pred <- function(j, q_SES, q_MBG) { # if posterior.sample drawn, add "j": function(j, q_SES, q_MBG)
  
  df.coeff <- data.frame(
    week_id   = rep(1:t),
    post_mean = NA)
  
  for(week in 1:t){
    
    df.coeff[week, "post_mean"] <-
      
      e1071::sigmoid(                                                                # Transform INLA output to probabilities
        
        result$summary.fixed["(Intercept)", "0.5quant"] +                            # Post. mean of the intercept
          
          post.sample.SES[week, j] * quintiles.df[q_SES, "SES_HH_Q1_prop_stand"] + 
          post.sample.MBG[week, j] * quintiles.df[q_MBG, "MBG_M_prop_stand"]     + 
         
          df.rand_eff.st.wk[week, "mean"]                                          + # st.  rand_eff (projected on nbh from spde/mesh model component)
          df.rand_eff.t    [week, "mean"])                                           # temp.rand_eff effect
    
    df.coeff$week_id <- seq(1:t)
    df.coeff$lvl_SES <- q_SES
    df.coeff$lvl_MBG <- q_MBG
    df.coeff$sim     <- j
  }
  
  return(df.coeff)
}


# ----- 6. Apply linear predictor function -----
df.coeff1.1 <- lapply(1:100, fun_lin.pred, q_SES = 1, q_MBG = 1)
df.coeff1.3 <- lapply(1:100, fun_lin.pred, q_SES = 1, q_MBG = 3)
df.coeff3.1 <- lapply(1:100, fun_lin.pred, q_SES = 3, q_MBG = 1)
df.coeff3.3 <- lapply(1:100, fun_lin.pred, q_SES = 3, q_MBG = 3)

# Combine the results
df.coeff1.1.b <- do.call("rbind", df.coeff1.1)
df.coeff1.3.b <- do.call("rbind", df.coeff1.3)
df.coeff3.1.b <- do.call("rbind", df.coeff3.1)
df.coeff3.3.b <- do.call("rbind", df.coeff3.3)

# ----- 7. Calculate Odds Ratios/Relative Risks -----
df.coeff1.1.b$RR <- df.coeff1.1.b$post_mean / df.coeff1.1.b$post_mean
df.coeff1.3.b$RR <- df.coeff1.3.b$post_mean / df.coeff1.1.b$post_mean
df.coeff3.1.b$RR <- df.coeff3.1.b$post_mean / df.coeff1.1.b$post_mean
df.coeff3.3.b$RR <- df.coeff3.3.b$post_mean / df.coeff1.1.b$post_mean


# ----- 8. Combine all output -----
df.tva <- rbind(df.coeff1.1.b, df.coeff1.3.b, df.coeff3.1.b, df.coeff3.3.b)


df.tva <- df.tva %>% 
  group_by(week_id, lvl_SES, lvl_MBG) %>% 
  summarise(q2   = median(RR), 
            q1   = quantile(RR, probs = 0.025), 
            q3   = quantile(RR, probs = 0.975)) %>%
  mutate(level   = interaction(as.factor(lvl_SES), as.factor(lvl_MBG)),
         level.r = factor(recode(level,
                                 "1.1" = "Type 4: Low % - Low %",
                                 "1.3" = "Type 2: Low % - High %",
                                 "3.1" = "Type 3: High % - Low %",
                                 "3.3" = "Type 1: High % - High %"),
                          levels = c("Type 1: High % - High %", 
                                     "Type 2: Low % - High %", 
                                     "Type 3: High % - Low %", 
                                     "Type 4: Low % - Low %")))


levels(df.tva$level.r) 

# ----- 9. Plot results -----
# Match dates to the week_id variable used for plotting
df.date <- 
  data.frame(date = week_begin) %>%
  complete(date = seq.Date(week_begin, as.Date("2021-12-27"), by = "week")) %>%
  mutate(week_id = seq(1:t))

# Join dates to df.tva using week_id as key
plot.tva <- df.tva %>%
  left_join(df.date, by = c("week_id" = "week_id"))

# Write resulting dataset to a .csv format
#write.csv(plot.tva, paste0(format(Sys.Date(), "%Y%m%d"), "_TVA_", type, ".csv"))

#plotAgg3 <- plot.tva
#plotAgg5 <- plot.tva
#plotAgg1 <- plot.tva

plot <- read.csv("Manuscript/Fig7_TVA_OR/Fig7_Data_TVA_OR_InfsAgg3.csv")
plot <- read.csv("Manuscript/Fig7_TVA_OR/Fig7_Data_TVA_OR_HospAgg3.csv")


plot <- read.csv("H:/Carsten - First glance at data/Manuscript/SupFig9_TVA_OR_SensAnalysis/SupFig9_Data_TVA_OR_HospAgg1.csv")
plot <- read.csv("H:/Carsten - First glance at data/Manuscript/SupFig9_TVA_OR_SensAnalysis/SupFig9_Data_TVA_OR_HospAgg5.csv")


week_begin <-as.Date("2020-01-06")

plot <- plot %>% 
  mutate(level.r = factor(recode(level.r,
                                 "Type 4: Low % - Low %"   = "High-SES, Low-migration",
                                 "Type 3: High % - Low %"  = "Low-SES, Low-migration",
                                 "Type 2: Low % - High %"  = "High-SES, High-migration",
                                 "Type 1: High % - High %" = "Low-SES, High-migration"),
                          levels = c("Low-SES, High-migration",
                                     "High-SES, High-migration",
                                     "Low-SES, Low-migration",
                                     "High-SES, Low-migration")))
                            
                            
                            
                            
                            
       



# Plot results
ggplot(data = plot, aes(x = as.Date(date), y = q2, colour = level.r, fill   = level.r)) + 
  
  geom_point(size = .1) + 
  geom_line(linewidth = .75, alpha     = .71) + 
  geom_ribbon(data = plot, aes(ymin = q1, ymax = q3, fill = level.r), colour  = NA, alpha   = .3) +
  
  scale_colour_manual(values      = custom_palette) +
  scale_fill_manual(  values      = custom_palette) +
  scale_y_continuous( breaks      = seq(0, 10, .05),
                      limits      = c(0.650, NA)) +
  scale_x_date(       date_breaks = "1 month",
                      date_labels = "%b %y", 
                      limits      = c(week_begin, as.Date("2022-01-08")), 
                      expand      = c(0,0)) +
  
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
  
  labs(title  = "", x      = "", 
       y      = "Median and 95% CrI (OR)", 
       colour = "Area Type:",
       fill   = "Area Type:",
       tag = "b") +
  
  geom_vline(xintercept = as.Date("2020-06-29"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2020-11-30"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-02-01"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-07-05"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2021-10-04"), colour = "black", linetype = 2, linewidth = 0.5) +
  
  annotate("text", x = as.Date("2020-04-20"), y = max(plot$q2) + 0.1*max(plot$q2), label = "Period 1",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2020-09-15"), y = max(plot$q2) + 0.1*max(plot$q2), label = "Period 2a", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-01-01"), y = max(plot$q2) + 0.1*max(plot$q2), label = "Period 2b", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-04-18"), y = max(plot$q2) + 0.1*max(plot$q2), label = "Period 3",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-08-18"), y = max(plot$q2) + 0.1*max(plot$q2), label = "Period 4",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-11-20"), y = max(plot$q2) + 0.1*max(plot$q2), label = "Period 5",  colour = "black", fontface = "bold") 



# ----- 10. Save resulting plot -----
#ggname <- "Fig7a_TVA_OR_InfsAgg3.jpeg"
#ggname <- "Fig7b_TVA_OR_HospAgg3.jpeg"
#ggname <- "Fig14a_TVA_OR_HospAgg1.jpeg"
#ggname <- "Fig14b_TVA_OR_HospAgg5.jpeg"

ggsave(ggname, 
       width = 15,
       height = 6.5,
       units = "in",
       dpi = 600#,
       #compression = "lzw"
)











