# Import libraries
#libs <- c("")
#lapply(libs, library, character.only = TRUE)

# Set working directory
setwd("H:\\Carsten - First glance at data")

# DATA IMPORT AND PREP
source("CrucialScripts_DONOTMOVE/DataPrep.R")
source("CrucialScripts_DONOTMOVE/Mob_Thresholds.R")

InfsDag3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_InfsDag3.rds")
HospDag3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospDag3.rds")




# ----- 1. Extract time-invariable fixed effects -----
extract_fixed <- function(model, model_name, covariates) { 
  fe <- as.data.frame(model$summary.fixed)
  fe$covariate <- rownames(fe) 
  fe$model <- model_name
  
  # Keep only specific covariates
  fe <- fe %>%
    filter(covariate %in% covariates) %>%
    mutate(mean = exp(mean),
           lcl  = exp(`0.025quant`),
           ucl  = exp(`0.975quant`))
  
  fe
    }





# ----- 2. Select covariates to plot -----
my_covars <- c("SES_HH_Q1_prop_stand",
          "MBG_group_Antilles_prop_stand",
          "MBG_group_Marocco_prop_stand",
          "MBG_group_Surinam_prop_stand",
          "MBG_group_Turkiye_prop_stand",
          "MBG_group_Other_W_prop_stand",
          "MBG_group_Other_nW_prop_stand")





# ----- 3. Apply function above ----- 
df1 <- extract_fixed(InfsDag3, "Infection model", my_covars)
df2 <- extract_fixed(HospDag3, "Hospitalisation model", my_covars)





# ----- 4. Combine df's -----
plot_data <- rbind(df1, df2)


covar_labels <- c(
  "SES_HH_Q1_prop_stand"          = "low SES",
  "MBG_group_Antilles_prop_stand" = "Migration: Antilles",
  "MBG_group_Marocco_prop_stand"  = "Migration: Marocco",
  "MBG_group_Surinam_prop_stand"  = "Migration: Surinam",
  "MBG_group_Turkiye_prop_stand"  = "Migration: Turkey",
  "MBG_group_Other_W_prop_stand"  = "Migration: Other (Western)",
  "MBG_group_Other_nW_prop_stand" = "Migration: Other (non-Western)"
)



plot_data <- plot_data %>%
  mutate(covariate = recode(covariate, !!!covar_labels),
         covariate = factor(covariate, levels = 
                              rev(c("low SES",
                                "Migration: Antilles",
                                "Migration: Marocco",
                                "Migration: Surinam",
                                "Migration: Turkey",
                                "Migration: Other (Western)",
                                "Migration: Other (non-Western)"))))





# ----- 5. Import data if available! -----
plot <- read.csv("C:/Users/Z710214/OneDrive - Radboudumc/z710214/My Documents/PhD/S5 - EQUalS/Writing (Carsten)/20250922_Final_export/SupFig6_FixedEffects/SupFig6_Data_FixedEffects.csv", sep = ";")





# ----- 6. Plot data -----
ggplot(plot, aes(x = exp(X0.5quant), y = covariate, colour = model)) +
  geom_point(position = position_dodge(width = 0.6), size = 3.5) +
  geom_errorbarh(aes(xmin = lcl, xmax = ucl),
                 position = position_dodge(width = 0.6),
                 height = 0.4,
                 size = 2) +
  geom_vline(xintercept = 1.00, colour = "grey", linetype = 1, linewidth = 1) +
  
  scale_colour_manual(values = c("Infection model" = "#CC4C02",
                                 "Hospitalisation model" = "#FFC107")) +
  labs(x = "OR, Median and 95% CrI",
       y = "Covariates", 
       colour = "Model") + 
  theme_minimal() +
  #scale_x_continuous(breaks = seq(0, 5, by = 0.025)) +
  theme(
    
    legend.position = "bottom",
    
    legend.text        = element_text(size = 14),
    legend.title       = element_text(size = 14, face = "bold"),
    axis.text.x        = element_text(size = 14),
    axis.text.y        = element_text(size = 12, face = "bold"),
    axis.title.y       = element_blank(),
    axis.title.x       = element_text(size = 14)
    
    
    
    
  )


# ----- 7. Save plot/data -----
#ggname <- "SupFig7_FixedEffects.jpeg"
#ggsave(ggname, width = 10, height = 10,  units = "in", dpi = 600)
#ggname <- "SupFig7_FixedEffects.eps"
#ggsave(ggname, width = 10, height = 10, units = "in", dpi = 600, device = "eps")
#write.csv(plot_data, paste0("SupFig7_Data_FixedEffects.csv"))


















