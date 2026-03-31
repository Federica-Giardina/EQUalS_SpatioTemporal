setwd("H:\\Carsten - First glance at data")
source("CrucialScripts_DONOTMOVE/DataPrep.R")
library(tmap)
library(RColorBrewer)

# ----- 1. Calculate SES/MBG/AGE Deciles -----
plot <- tot_pop_wc %>%
  mutate(
    SES_Q10 = case_when(
      SES_avg_HH <  quantile(tot_pop_wc$SES_avg_HH, probs = c(0.1), na.rm = TRUE) ~ "Q1", 
      SES_avg_HH >= quantile(tot_pop_wc$SES_avg_HH, probs = c(0.1), na.rm = TRUE) & SES_avg_HH < quantile(tot_pop_wc$SES_avg_HH, probs = c(0.2), na.rm = TRUE) ~ "Q2",
      SES_avg_HH >= quantile(tot_pop_wc$SES_avg_HH, probs = c(0.2), na.rm = TRUE) & SES_avg_HH < quantile(tot_pop_wc$SES_avg_HH, probs = c(0.3), na.rm = TRUE) ~ "Q3",
      SES_avg_HH >= quantile(tot_pop_wc$SES_avg_HH, probs = c(0.3), na.rm = TRUE) & SES_avg_HH < quantile(tot_pop_wc$SES_avg_HH, probs = c(0.4), na.rm = TRUE) ~ "Q4",
      SES_avg_HH >= quantile(tot_pop_wc$SES_avg_HH, probs = c(0.4), na.rm = TRUE) & SES_avg_HH < quantile(tot_pop_wc$SES_avg_HH, probs = c(0.5), na.rm = TRUE) ~ "Q5",
      SES_avg_HH >= quantile(tot_pop_wc$SES_avg_HH, probs = c(0.5), na.rm = TRUE) & SES_avg_HH < quantile(tot_pop_wc$SES_avg_HH, probs = c(0.6), na.rm = TRUE) ~ "Q6",
      SES_avg_HH >= quantile(tot_pop_wc$SES_avg_HH, probs = c(0.6), na.rm = TRUE) & SES_avg_HH < quantile(tot_pop_wc$SES_avg_HH, probs = c(0.7), na.rm = TRUE) ~ "Q7",
      SES_avg_HH >= quantile(tot_pop_wc$SES_avg_HH, probs = c(0.7), na.rm = TRUE) & SES_avg_HH < quantile(tot_pop_wc$SES_avg_HH, probs = c(0.8), na.rm = TRUE) ~ "Q8",
      SES_avg_HH >= quantile(tot_pop_wc$SES_avg_HH, probs = c(0.8), na.rm = TRUE) & SES_avg_HH < quantile(tot_pop_wc$SES_avg_HH, probs = c(0.9), na.rm = TRUE) ~ "Q9",
      SES_avg_HH >= quantile(tot_pop_wc$SES_avg_HH, probs = c(0.9), na.rm = TRUE) ~ "Q10"),
    SESQ1_Q10 = case_when(
      SES_HH_Q1_prop <  quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.1), na.rm = TRUE) ~ "Q1", 
      SES_HH_Q1_prop >= quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.1), na.rm = TRUE) & SES_HH_Q1_prop < quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.2), na.rm = TRUE) ~ "Q2",
      SES_HH_Q1_prop >= quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.2), na.rm = TRUE) & SES_HH_Q1_prop < quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.3), na.rm = TRUE) ~ "Q3",
      SES_HH_Q1_prop >= quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.3), na.rm = TRUE) & SES_HH_Q1_prop < quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.4), na.rm = TRUE) ~ "Q4",
      SES_HH_Q1_prop >= quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.4), na.rm = TRUE) & SES_HH_Q1_prop < quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.5), na.rm = TRUE) ~ "Q5",
      SES_HH_Q1_prop >= quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.5), na.rm = TRUE) & SES_HH_Q1_prop < quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.6), na.rm = TRUE) ~ "Q6",
      SES_HH_Q1_prop >= quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.6), na.rm = TRUE) & SES_HH_Q1_prop < quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.7), na.rm = TRUE) ~ "Q7",
      SES_HH_Q1_prop >= quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.7), na.rm = TRUE) & SES_HH_Q1_prop < quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.8), na.rm = TRUE) ~ "Q8",
      SES_HH_Q1_prop >= quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.8), na.rm = TRUE) & SES_HH_Q1_prop < quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.9), na.rm = TRUE) ~ "Q9",
      SES_HH_Q1_prop >= quantile(tot_pop_wc$SES_HH_Q1_prop, probs = c(0.9), na.rm = TRUE) ~ "Q10"),
    AGE_Q10 = case_when(
      Age_65.00_prop <  quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.1), na.rm = TRUE) ~ "Q1", 
      Age_65.00_prop >= quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.1), na.rm = TRUE) & Age_65.00_prop < quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.2), na.rm = TRUE) ~ "Q2",
      Age_65.00_prop >= quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.2), na.rm = TRUE) & Age_65.00_prop < quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.3), na.rm = TRUE) ~ "Q3",
      Age_65.00_prop >= quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.3), na.rm = TRUE) & Age_65.00_prop < quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.4), na.rm = TRUE) ~ "Q4",
      Age_65.00_prop >= quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.4), na.rm = TRUE) & Age_65.00_prop < quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.5), na.rm = TRUE) ~ "Q5",
      Age_65.00_prop >= quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.5), na.rm = TRUE) & Age_65.00_prop < quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.6), na.rm = TRUE) ~ "Q6",
      Age_65.00_prop >= quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.6), na.rm = TRUE) & Age_65.00_prop < quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.7), na.rm = TRUE) ~ "Q7",
      Age_65.00_prop >= quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.7), na.rm = TRUE) & Age_65.00_prop < quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.8), na.rm = TRUE) ~ "Q8",
      Age_65.00_prop >= quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.8), na.rm = TRUE) & Age_65.00_prop < quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.9), na.rm = TRUE) ~ "Q9",
      Age_65.00_prop >= quantile(tot_pop_wc$Age_65.00_prop, probs = c(0.9), na.rm = TRUE) ~ "Q10"),
    MBG_Q10 = case_when(
      MBG_M_prop <  quantile(tot_pop_wc$MBG_M_prop, probs = c(0.1), na.rm = TRUE) ~ "Q1", 
      MBG_M_prop >= quantile(tot_pop_wc$MBG_M_prop, probs = c(0.1), na.rm = TRUE) & MBG_M_prop < quantile(tot_pop_wc$MBG_M_prop, probs = c(0.2), na.rm = TRUE) ~ "Q2",
      MBG_M_prop >= quantile(tot_pop_wc$MBG_M_prop, probs = c(0.2), na.rm = TRUE) & MBG_M_prop < quantile(tot_pop_wc$MBG_M_prop, probs = c(0.3), na.rm = TRUE) ~ "Q3",
      MBG_M_prop >= quantile(tot_pop_wc$MBG_M_prop, probs = c(0.3), na.rm = TRUE) & MBG_M_prop < quantile(tot_pop_wc$MBG_M_prop, probs = c(0.4), na.rm = TRUE) ~ "Q4",
      MBG_M_prop >= quantile(tot_pop_wc$MBG_M_prop, probs = c(0.4), na.rm = TRUE) & MBG_M_prop < quantile(tot_pop_wc$MBG_M_prop, probs = c(0.5), na.rm = TRUE) ~ "Q5",
      MBG_M_prop >= quantile(tot_pop_wc$MBG_M_prop, probs = c(0.5), na.rm = TRUE) & MBG_M_prop < quantile(tot_pop_wc$MBG_M_prop, probs = c(0.6), na.rm = TRUE) ~ "Q6",
      MBG_M_prop >= quantile(tot_pop_wc$MBG_M_prop, probs = c(0.6), na.rm = TRUE) & MBG_M_prop < quantile(tot_pop_wc$MBG_M_prop, probs = c(0.7), na.rm = TRUE) ~ "Q7",
      MBG_M_prop >= quantile(tot_pop_wc$MBG_M_prop, probs = c(0.7), na.rm = TRUE) & MBG_M_prop < quantile(tot_pop_wc$MBG_M_prop, probs = c(0.8), na.rm = TRUE) ~ "Q8",
      MBG_M_prop >= quantile(tot_pop_wc$MBG_M_prop, probs = c(0.8), na.rm = TRUE) & MBG_M_prop < quantile(tot_pop_wc$MBG_M_prop, probs = c(0.9), na.rm = TRUE) ~ "Q9",
      MBG_M_prop >= quantile(tot_pop_wc$MBG_M_prop, probs = c(0.9), na.rm = TRUE) ~ "Q10")) %>%
  select(wc2021, tot_pop, Age_65.00, n_MBG_M, SES_I_Q1, SES_avg_HH, SES_HH_Q1_prop, SES_Q10, SES_Q5, SESQ1_Q10, SESQ1_Q5, MBG_M_prop, MBG_Q10, MBG_Q5, AGE_Q10, AGE_Q5)



# ----- 2. Filter out groups with <=10 individuals -----
plot.SES <- plot %>% mutate(SESQ1_Q10   = ifelse(SES_I_Q1   <= 10, "Censored", SESQ1_Q10),
                            SES_Q10     = ifelse(SES_I_Q1   <= 10, "Censored", SES_Q10),
                            SESQ1_Q10   = factor(SESQ1_Q10, levels = c("Censored", paste0("Q", seq_len(length(1:10))))),
                            SES_Q10     = factor(SES_Q10,   levels = c("Censored", paste0("Q", seq_len(length(1:10))))),
                            SES_Q10.rev = factor(SES_Q10,   levels = c(paste0("Q", seq_len(length(1:10))), "Censored")))
plot.MBG <- plot %>% mutate(MBG_Q10   = ifelse(n_MBG_M    <= 10, "Censored", MBG_Q10),
                            MBG_Q10   = factor(MBG_Q10,   levels = c("Censored", paste0("Q", seq_len(length(1:10))))))
plot.AGE <- plot %>% mutate(AGE_Q10   = ifelse(Age_65.00  <= 10, "Censored", AGE_Q10),
                            AGE_Q10   = factor(AGE_Q10,   levels = c("Censored", paste0("Q", seq_len(length(1:10))))))
                            

# ----- 3. Import mapping data ----- 
map <- read_sf("K:/Utilities/Tools/GISHulpbestanden/Gemeentewijkbuurt/2021/wk_2021.shp") %>% 
  mutate(wc2021 = substring(STATCODE, 3)) %>%
  select(wc2021, geometry)


# ----- 4. Join area data to mapping data -----
plot.SES <- map %>% left_join(plot.SES, by = c("wc2021"))
plot.MBG <- map %>% left_join(plot.MBG, by = c("wc2021"))
plot.AGE <- map %>% left_join(plot.AGE, by = c("wc2021"))

# ----- 5. Specify colour palette consistent with other Figures -----
pal     <- c("white", brewer.pal(10, "YlOrBr"))
pal.rev <- c(rev(brewer.pal(10, "YlOrBr")), "white")


# ----- 6. Plot median SES-Score (Deciles) -----
SES_Q10 <- tm_shape(plot.SES) +
  tm_borders(alpha = 0.25) +  
  tm_fill(col = "SES_Q10",
          style = "fixed",
          palette = pal,
          title = "",
          labels = c("Censored", "Lowest decile", rep("", 8), "Highest decile"),
          showNA = FALSE) +
  
  tm_layout(title                 = "SES-score (Decile)",
            title.fontface        = "bold",
            title.size            = .7,
            
            main.title            = "d",
            main.title.position   = "left", 
            main.title.fontface   = "bold",
            main.title.size       = 1.25,
            
            legend.title.fontface = "bold",
            legend.title.size     = .7,
            legend.frame          = FALSE,
            legend.position       = c("left", "top"),
            legend.text.size      = .55,
            legend.text.fontface  = "bold",
            
            frame = FALSE)

# Reversed
SES_Q10.rev <- tm_shape(plot.SES) +
  tm_borders(alpha = 0.25) +  
  tm_fill(col = "SES_Q10.rev",
          style = "fixed",
          palette = pal.rev,
          title = "",
          labels = c("Lowest decile", rep("", 8), "Highest decile", "Censored"),
          showNA = FALSE) +
  
  tm_layout(title                 = "SES-score (Decile)",
            title.fontface        = "bold",
            title.size            = .7,
            
            main.title            = "d",
            main.title.position   = "left", 
            main.title.fontface   = "bold",
            main.title.size       = 1.25,
            
            legend.title.fontface = "bold",
            legend.title.size     = .7,
            legend.frame          = FALSE,
            legend.position       = c("left", "top"),
            legend.text.size      = .55,
            legend.text.fontface  = "bold",
            
            frame = FALSE)


# ----- 7. Plot proportion of SES Q1 (Deciles) -----
SESQ1_Q10 <- tm_shape(plot.SES) +
  tm_borders(alpha = 0.25) +  
  tm_fill(col = "SESQ1_Q10",
          style = "fixed",
          palette = pal,
          title = "",
          labels = c("Censored", "Lowest decile", rep("", 8), "Highest decile"),
          showNA = FALSE) +
  
  tm_layout(title                 = "Proportion low-SES (Decile)",
            title.fontface        = "bold",
            title.size            = .7,
            
            main.title            = "a",
            main.title.position   = "left", 
            main.title.fontface   = "bold",
            main.title.size       = 1.25,
            
            legend.title.fontface = "bold",
            legend.title.size     = .7,
            legend.frame          = FALSE,
            legend.position       = c("left", "top"),
            legend.text.size      = .55,
            legend.text.fontface  = "bold",
            
            frame = FALSE)

# ----- 8. Plot proportion of migrants (deciles) -----
MBG_Q10 <- tm_shape(plot.MBG) +
  tm_borders(alpha = 0.25) +  
  tm_fill(col = "MBG_Q10",
          style = "fixed",
          palette = pal,
          title = "",
          labels = c("Censored", "Lowest decile", rep("", 8), "Highest decile"),
          
          showNA = FALSE) +
  
  tm_layout(title                 = "Proportion with a migration background (Decile)",
            title.fontface        = "bold",
            title.size            = .7,
            
            main.title            = "b",
            main.title.position   = "left", 
            main.title.fontface   = "bold",
            main.title.size       = 1.25,
            
            legend.title.fontface = "bold",
            legend.title.size     = .7,
            legend.frame          = FALSE,
            legend.position       = c("left", "top"),
            legend.text.size      = .55,
            legend.text.fontface  = "bold",
            
            frame = FALSE)

# ----- 9. plot proportion aged 65+ (deciles) -----
AGE_Q10 <- tm_shape(plot.AGE) +
  tm_borders(alpha = 0.25) +  
  tm_fill(col = "AGE_Q10",
          style = "fixed",
          palette = pal,
          title = "",
          labels = c("Censored", "Lowest decile", rep("", 8), "Highest decile"),
          
          showNA = FALSE) +
  
  tm_layout(title                 = "Proportion aged 65+ (Decile)",
            title.fontface        = "bold",
            title.size            = .7,
            
            main.title            = "c",
            main.title.position   = "left", 
            main.title.fontface   = "bold",
            main.title.size       = 1.25,
            
            legend.title.fontface = "bold",
            legend.title.size     = .7,
            legend.frame          = FALSE,
            legend.position       = c("left", "top"),
            legend.text.size      = .55,
            legend.text.fontface  = "bold",
            
            frame = FALSE)

# ----- 10. Plot maps -----
SESQ1_Q10
MBG_Q10
AGE_Q10
SES_Q10
SES_Q10.rev


# ----- 11. Save map objects -----

# SES Q1 deciles
tmap_save(SESQ1_Q10, 
          filename = "Fig4a_map_SESQ_deciles.jpeg", 
          width = 15, height = 6.5, units = "in", dpi = 600)
# MBG deciles
tmap_save(MBG_Q10,   
          filename = paste0("Fig4b_map_MBGQ_deciles.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 600)
# AGE deciles
tmap_save(AGE_Q10,     
          filename = paste0("Fig4c_map_AGEQ_deciles.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 600)
# SES-score deciles
tmap_save(SES_Q10,     
          filename = paste0("Fig4d_map_SESs_deciles.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 600)
# SES-score deciles reversed
tmap_save(SES_Q10.rev, 
          filename = paste0("Fig4d_map_SESs_deciles_reversed.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 600)

