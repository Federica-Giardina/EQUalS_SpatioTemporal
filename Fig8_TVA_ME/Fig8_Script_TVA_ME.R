# Import libraries
#libs <- c("")
#lapply(libs, library, character.only = TRUE)

# Set working directory
setwd("H:\\Carsten - First glance at data")


# DATA IMPORT AND PREP
source("CrucialScripts_DONOTMOVE/DataPrep.R")
source("CrucialScripts_DONOTMOVE/Mob_Thresholds.R")

# Models
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
data               <- Cases.nb.wk #%>% filter(week_begin > as.Date("2020-05-25")) # REMOVE FILTER IF HOSP!
data$y             <- data$hosp_obs # case_obs OR hosp_obs
type               <- "hosp" # "infs" OR "hosp"!
result             <- HospAgg3
week_begin         <- as.Date("2020-01-06") # Hosps: as.Date("2020-01-06") OR Infs: as.Date("2020-06-01")
t                  <- length(unique(as.integer(factor(data$week_id))))
i                  <- 3228
r                  <- 1:t
custom_palette     <- c("#17bebb", "#F28220", "#FFC107", "#e54339")
custom_palette.rev <- rev(custom_palette)

# Convert mesh to neighbourhoods
source("CrucialScripts_DONOTMOVE/Extr_SpatTemp_RandEff.R")


# Extract TEMPORAL random effect (group = week_idx)
df.rand_eff.t <- result$summary.random$week_idx %>% select(ID, mean)


# Extract SPATIOTEMPORAL random effects (projected from spde/mesh model component)
df.rand_eff.st.wk <- df.rand_eff.st %>% 
  group_by(week_id) %>%
  summarise(mean = mean(st_rand_eff, na.rm = TRUE))


# Draw posterior samples
#post.sample     <- inla.posterior.sample(100, result)
#saveRDS(post.sample, "post_sample_infs_agg.rds")
#saveRDS(post.sample, "post_sample_hosp_agg.rds")
post.sample <- readRDS("RESULTS JULY 2025/00_ModRes/Post_Samples/PS_HospAgg3.rds")

post.sample.SES <- inla.posterior.sample.eval(function(xx) week_SES + SES_HH_Q1_prop_stand, post.sample)
post.sample.MBG <- inla.posterior.sample.eval(function(xx) week_MBG + MBG_M_prop_stand,     post.sample)
post.sample.AGE <- inla.posterior.sample.eval(function(xx) week_age + Age_65.00_prop_stand, post.sample) 


# Calculate the lower and upper quartiles
data.q <- data %>% filter(week_id == 104)

lower_quartile_SES <- quantile(data.q$SES_HH_Q1_prop_stand, probs = c(.25))
upper_quartile_SES <- quantile(data.q$SES_HH_Q1_prop_stand, probs = c(.75)) 
lower_quartile_MBG <- quantile(data.q$MBG_M_prop_stand,     probs = c(.25))
upper_quartile_MBG <- quantile(data.q$MBG_M_prop_stand,     probs = c(.75))


# Create subsets for the lower and upper quartiles of SES & MBG
data_lower_lower <- data %>% filter(SES_HH_Q1_prop_stand <= lower_quartile_SES & MBG_M_prop_stand <= lower_quartile_MBG)
data_upper_lower <- data %>% filter(SES_HH_Q1_prop_stand >= upper_quartile_SES & MBG_M_prop_stand <= lower_quartile_MBG)
data_lower_upper <- data %>% filter(SES_HH_Q1_prop_stand <= lower_quartile_SES & MBG_M_prop_stand >= upper_quartile_MBG)
data_upper_upper <- data %>% filter(SES_HH_Q1_prop_stand >= upper_quartile_SES & MBG_M_prop_stand >= upper_quartile_MBG )


lin.pred.me <- function(j, df, grp) {
  
  df.me <- data.frame(
    week_id   = rep(1:t),
    post_mean = NA)
  
  for(week in 1:(t)) {
  
    df.me[week, "post_mean"] <- 
      result$summary.fixed["(Intercept)", "0.5quant"] +
      
      post.sample.SES[week, j] * mean(df$SES_HH_Q1_prop_stand) + 
      post.sample.MBG[week, j] * mean(df$MBG_M_prop_stand) +
      post.sample.AGE[week, j] * mean(df$Age_65.00_prop_stand) +
      
      df.rand_eff.t    [week, "mean"] +    # Temporal component
      df.rand_eff.st.wk[week, "mean"]      # Temporal and spatial interaction
    
    df.me$sim     <- j
    df.me$week_id <- seq(1, t)
    df.me$group   <- grp
  }
  return(df.me)
}


# Apply lin.pred.me with the posterior samples drawn to the four different area types.
result_lower_lower <- lapply(1:100, lin.pred.me, df = data_lower_lower, grp = "Low - Low")
result_upper_lower <- lapply(1:100, lin.pred.me, df = data_upper_lower, grp = "High - Low")
result_lower_upper <- lapply(1:100, lin.pred.me, df = data_lower_upper, grp = "Low - High")
result_upper_upper <- lapply(1:100, lin.pred.me, df = data_upper_upper, grp = "High - High")


# Combine results of lin.pred.me
df.ll <- do.call("rbind", result_lower_lower)
df.ul <- do.call("rbind", result_upper_lower)
df.lu <- do.call("rbind", result_lower_upper)
df.uu <- do.call("rbind", result_upper_upper)


# Compute predicted probabilities (inverse logit)
df.ll$pred.prob <- 1 / (1 + exp(-df.ll$post_mean))
df.ul$pred.prob <- 1 / (1 + exp(-df.ul$post_mean))
df.lu$pred.prob <- 1 / (1 + exp(-df.lu$post_mean))
df.uu$pred.prob <- 1 / (1 + exp(-df.uu$post_mean))


# Calculate the Marginal Effects
### The marg.eff for a binomial model is the the product of the pred. prob. and its complement (i.e. p * (1 - p))
### i.e. derivative of the logit. This results (when interested in B1 and B2) in: 
### Total marg.eff = (B1 * p * (1 - p)) + (B2 * p * (1 - p)) = (B1 SES + Y1 SES + B2 MBG + Y2 MBG) * pred.prob * (1 - pred.prob)

df.ll$marg.eff <- 
  (result$summary.fixed$mean[2] + result$summary.random$week_SES$mean +
     result$summary.fixed$mean[3] + result$summary.random$week_MBG$mean
   ) * df.ll$pred.prob * (1 - df.ll$pred.prob)

df.ul$marg.eff <- 
  (result$summary.fixed$mean[2] + result$summary.random$week_SES$mean +  
     result$summary.fixed$mean[3] + result$summary.random$week_MBG$mean
   ) * df.ul$pred.prob * (1 - df.ul$pred.prob)

df.lu$marg.eff <- 
  (result$summary.fixed$mean[2] + result$summary.random$week_SES$mean +
     result$summary.fixed$mean[3] + result$summary.random$week_MBG$mean
   ) * df.lu$pred.prob * (1 - df.lu$pred.prob)

df.uu$marg.eff <- 
  (result$summary.fixed$mean[2] + result$summary.random$week_SES$mean +  
     result$summary.fixed$mean[3] + result$summary.random$week_MBG$mean
   ) * df.uu$pred.prob * (1 - df.uu$pred.prob)



# Combine dataset and calculate quantiles from all posterior samples
df.me.fin <- rbind(df.ll, df.ul, df.lu, df.uu)

df.me.fin <- df.me.fin %>% 
  group_by(week_id, group) %>% 
  summarise(q2 = median(marg.eff),
            q1 = quantile(marg.eff, probs = 0.025),
            q3 = quantile(marg.eff, probs = 0.975)) %>%
  mutate(level.r =factor(recode(group,
                                "High - High" = "Type 1: High % - High %",
                                "Low - High"  = "Type 2: Low % - High %",
                                "High - Low"  = "Type 3: High % - Low %",
                                "Low - Low"   = "Type 4: Low % - Low %"),
                         levels = c("Type 1: High % - High %", 
                                    "Type 2: Low % - High %", 
                                    "Type 3: High % - Low %", 
                                    "Type 4: Low % - Low %")))

levels(df.me.fin$level.r) 


# Create dataset to match dates for plotting
df.date <- data.frame(date = week_begin) %>%
  complete(date   = seq.Date(week_begin, as.Date("2021-12-27"), by = "week")) %>%
  mutate(week_id  = seq(1:t))

plot.me <- df.me.fin %>%
  left_join(df.date, by = c("week_id" = "week_id"))

# Write resulting dataset to a .csv format
#write.csv(plot.me, paste0(format(Sys.Date(), "%Y%m%d"), "_MargEff_", type, ".csv"))








plot <- read.csv("Manuscript/Fig8_TVA_ME/Fig8_Data_TVA_ME_InfsAgg3.csv")
plot <- read.csv("Manuscript/Fig8_TVA_ME/Fig8_Data_TVA_ME_HospAgg3.csv")


plot <- read.csv("H:/Carsten - First glance at data/Manuscript/SupFig10_TVA_ME_SensAnalysis/SupFig10_Data_TVA_ME_HospAgg1.csv")
plot <- read.csv("H:/Carsten - First glance at data/Manuscript/SupFig10_TVA_ME_SensAnalysis/SupFig10_Data_TVA_ME_HospAgg5.csv")


week_begin <- as.Date("2020-01-06")
week_begin <- as.Date("2020-06-01")

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
ggplot(data = plot, aes(x = as.Date(date), y = q2, colour = level.r, fill = level.r, group = level.r)) + 
  
  geom_point(size = 0.1) + 
  geom_line(linewidth = .61, alpha     = .71) + 
  
  geom_ribbon(data = plot, aes(ymin = q1, ymax = q3, fill = level.r), colour  = NA, alpha = .3) +
  
  scale_colour_manual(values      = custom_palette) +
  scale_fill_manual(  values      = custom_palette) +
  
  scale_y_continuous( breaks       = seq(-0.5, 0.5, 0.0050), 
                      minor_breaks = seq(-0.5, 0.5, 0.0005),
                      limits       = c(-0.0075, NA)        # -0.025- to 0.010 for INFS and -0.0075 for HOSPS!
  ) +
  
  scale_x_date(      date_breaks = "1 month",
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
       y      = "Probability change, % points (95% CrI)", 
       colour = "Area Type:",
       fill   = "Area Type:",
       tag = "a") +
  
  geom_hline(yintercept = 0.00, colour = "grey", linetype = 1, linewidth = 1) +
  geom_vline(xintercept = as.Date("2020-06-29"), colour = "black", linetype = 2, linewidth = .75) + 
  geom_vline(xintercept = as.Date("2020-11-30"), colour = "black", linetype = 2, linewidth = .75) +
  geom_vline(xintercept = as.Date("2021-02-01"), colour = "black", linetype = 2, linewidth = .75) +
  geom_vline(xintercept = as.Date("2021-07-05"), colour = "black", linetype = 2, linewidth = .75) + 
  geom_vline(xintercept = as.Date("2021-10-04"), colour = "black", linetype = 2, linewidth = .75) +
  
  annotate("text", x = as.Date("2020-04-20"), y = max(plot$q2) + 0.2*max(plot$q2), label = "Period 1",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2020-09-15"), y = max(plot$q2) + 0.2*max(plot$q2), label = "Period 2a", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-01-01"), y = max(plot$q2) + 0.2*max(plot$q2), label = "Period 2b", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-04-18"), y = max(plot$q2) + 0.2*max(plot$q2), label = "Period 3",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-08-18"), y = max(plot$q2) + 0.2*max(plot$q2), label = "Period 4",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-11-20"), y = max(plot$q2) + 0.2*max(plot$q2), label = "Period 5",  colour = "black", fontface = "bold") 








ggname <- "Fig8a_TVA_ME_InfsAgg3.jpeg"
ggname <- "Fig8b_TVA_ME_HospAgg3.jpeg"
ggname <- "SupFig9a_TVA_ME_HospAgg1.jpeg"
ggname <- "SupFig9b_TVA_ME_HospAgg5.jpeg"


ggsave(ggname, 
       width = 15,
       height = 6.5,
       units = "in",
       dpi = 600)



