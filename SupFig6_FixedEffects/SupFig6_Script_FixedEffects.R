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


my_covars <- c("SES_HH_Q1_prop_stand",
          "MBG_group_Antilles_prop_stand",
          "MBG_group_Marocco_prop_stand",
          "MBG_group_Surinam_prop_stand",
          "MBG_group_Turkiye_prop_stand",
          "MBG_group_Other_W_prop_stand",
          "MBG_group_Other_nW_prop_stand")

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


# ----- 5. Plot data -----
ggplot(plot_data, aes(x = mean, y = covariate, colour = model)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbarh(aes(xmin = lcl, xmax = ucl),
                 position = position_dodge(width = 0.6),
                 height = 0.2) +
  geom_vline(xintercept = 1, linetype = 1, colour = "grey40") +
  scale_colour_manual(values = c("Infection model" = "#CC4C02",
                                 "Hospitalisation model" = "#FFC107")) +
  labs(x = "Odds Ratio (95% CrI)",
       y = "Covariates", 
       colour = "Model") + 
  theme_minimal() +
  #scale_x_continuous(breaks = seq(0, 5, by = 0.025)) +
  theme(
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom"
  )

# ----- 6. Save plot -----
ggname <- "SupFig7_FixedEffects.jpeg"

ggsave(ggname, 
       width = 15,
       height = 6.5,
       units = "in",
       dpi = 600)


write.csv(plot_data, paste0("SupFig7_Data_FixedEffects.csv"))


















