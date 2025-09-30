# Import libraries
libs <- c("tmap", "RColorBrewer")
lapply(libs, library, character.only = TRUE)

# Set working directory
setwd("H:\\Carsten - First glance at data")

# DATA IMPORT AND PREP
source("CrucialScripts_DONOTMOVE/DataPrep.R")
source("CrucialScripts_DONOTMOVE/Mob_Thresholds.R")


# ----- 1. Assign RIVM periods to infections and hospitalisations -----
# Define periods
periods <- c("Period 1 (1 Jan 2020 - 29 Jun 2020); Wildtype", 
             "Period 2a (29 Jun 2020 - 30 Nov 2020); Wildtype", 
             "Period 2b (30 Nov 2020 - 1 Feb 2021); Wildtype", 
             "Period 3 (1 Feb 2021 - 5 Jul 2021); Alpha", 
             "Period 4 (5 Jul 2021 - 4 Oct 2021); Delta", 
             "Period 5 (4 Oct 2021 - 31 Dec 2021); Delta")

#Infections
Infs <- RepCases %>%
  select(RINPERSOON, COVID19_dateconfirmed, wc2021) %>%
  mutate(period = case_when(
    COVID19_dateconfirmed > as.Date("2019-12-31") & COVID19_dateconfirmed <= as.Date("2020-06-29") ~ "Period 1 (1 Jan 2020 - 29 Jun 2020); Wildtype",
    COVID19_dateconfirmed > as.Date("2020-06-29") & COVID19_dateconfirmed <= as.Date("2020-11-30") ~ "Period 2a (29 Jun 2020 - 30 Nov 2020); Wildtype",
    COVID19_dateconfirmed > as.Date("2020-11-30") & COVID19_dateconfirmed <= as.Date("2021-02-01") ~ "Period 2b (30 Nov 2020 - 1 Feb 2021); Wildtype", 
    COVID19_dateconfirmed > as.Date("2021-02-01") & COVID19_dateconfirmed <= as.Date("2021-07-05") ~ "Period 3 (1 Feb 2021 - 5 Jul 2021); Alpha",
    COVID19_dateconfirmed > as.Date("2021-07-05") & COVID19_dateconfirmed <= as.Date("2021-10-04") ~ "Period 4 (5 Jul 2021 - 4 Oct 2021); Delta", 
    COVID19_dateconfirmed > as.Date("2021-10-04") & COVID19_dateconfirmed <= as.Date("2021-12-31") ~ "Period 5 (4 Oct 2021 - 31 Dec 2021); Delta"))

# Hospitalisations
Hosp <- HospCases %>%
  select(RINPERSOON, LBZOpnamedatum, wc2021) %>%
  mutate(period = case_when(
    LBZOpnamedatum > as.Date("2019-12-31") & LBZOpnamedatum <= as.Date("2020-06-29") ~ "Period 1 (1 Jan 2020 - 29 Jun 2020); Wildtype",
    LBZOpnamedatum > as.Date("2020-06-29") & LBZOpnamedatum <= as.Date("2020-11-30") ~ "Period 2a (29 Jun 2020 - 30 Nov 2020); Wildtype",
    LBZOpnamedatum > as.Date("2020-11-30") & LBZOpnamedatum <= as.Date("2021-02-01") ~ "Period 2b (30 Nov 2020 - 1 Feb 2021); Wildtype", 
    LBZOpnamedatum > as.Date("2021-02-01") & LBZOpnamedatum <= as.Date("2021-07-05") ~ "Period 3 (1 Feb 2021 - 5 Jul 2021); Alpha",
    LBZOpnamedatum > as.Date("2021-07-05") & LBZOpnamedatum <= as.Date("2021-10-04") ~ "Period 4 (5 Jul 2021 - 4 Oct 2021); Delta", 
    LBZOpnamedatum > as.Date("2021-10-04") & LBZOpnamedatum <= as.Date("2021-12-31") ~ "Period 5 (4 Oct 2021 - 31 Dec 2021); Delta"))


# ----- 2. Count infections per neighbourhood per period -----
Infs.nb.period <- Infs %>% group_by(wc2021, period) %>% summarise(n_case = n())
Hosp.nb.period <- Hosp %>% group_by(wc2021, period) %>% summarise(n_hosp = n())


# ---- 3. Create template for all neighbourhood - period combinations and join counts -----   
temp <- Cases.nb.wk %>%
  select(wc2021, nbh_id, tot_pop) %>%
  distinct(wc2021, .keep_all = TRUE) %>%
  crossing(period = periods)

data <- temp %>%
  left_join(Infs.nb.period, by = c("wc2021", "period")) %>%
  left_join(Hosp.nb.period, by = c("wc2021", "period"))


# 4. ----- Calculate rates for infections and hospitalisations -----
data <- data %>%
  mutate(n_case.f = ifelse(n_case < 10, 0, n_case),
         n_hosp.f = ifelse(n_hosp < 10, 0, n_hosp),
         case_rate = n_case.f / tot_pop * 100000,
         hosp_rate = n_hosp.f / tot_pop * 100000,
         case_rate = ifelse(is.na(case_rate), 0, case_rate),
         hosp_rate = ifelse(is.na(hosp_rate), 0, hosp_rate),
         
         case_rate = ifelse(period == "Period 1 (1 Jan 2020 - 29 Jun 2020); Wildtype", NA, case_rate))


# ----- 5. Join rates to sf geometry -----
plot <- map %>% left_join(data, by = c("wc2021"))


# ----- 6. Prepare for plotting -----
# Remove missing neighbourhoods
plot.f <- plot %>% filter(!is.na(period))

# ----- 7. Subset periods for plotting -----
plot.1  <- plot.f %>% filter(period == "Period 1 (1 Jan 2020 - 29 Jun 2020); Wildtype")
plot.2a <- plot.f %>% filter(period == "Period 2a (29 Jun 2020 - 30 Nov 2020); Wildtype")
plot.2b <- plot.f %>% filter(period == "Period 2b (30 Nov 2020 - 1 Feb 2021); Wildtype" )
plot.3  <- plot.f %>% filter(period == "Period 3 (1 Feb 2021 - 5 Jul 2021); Alpha")
plot.4  <- plot.f %>% filter(period == "Period 4 (5 Jul 2021 - 4 Oct 2021); Delta")
plot.5  <- plot.f %>% filter(period == "Period 5 (4 Oct 2021 - 31 Dec 2021); Delta")


# ----- 8. Define colourset for plotting -----
pal <- brewer.pal(9, "YlOrBr")

#pal <- c("white", brewer.pal(10, "YlOrBr"))
# ----- 9. Plot for infections for each period separately -----
# Infections
#plot.1  <- plot.1  %>% mutate(case_rate = ifelse(case_rate == 0, NA, case_rate),
#                              case_rate = ifelse(case_rate < 10, NA, case_rate))
#plot.2a <- plot.2a %>% mutate(case_rate = ifelse(case_rate == 0, NA, case_rate),
#                              case_rate = ifelse(case_rate < 10, NA, case_rate))
#plot.2b <- plot.2b %>% mutate(case_rate = ifelse(case_rate == 0, NA, case_rate),
#                              case_rate = ifelse(case_rate < 10, NA, case_rate))
#plot.3  <- plot.3  %>% mutate(case_rate = ifelse(case_rate == 0, NA, case_rate),
#                              case_rate = ifelse(case_rate < 10, NA, case_rate))
#plot.4  <- plot.4  %>% mutate(case_rate = ifelse(case_rate == 0, NA, case_rate),
#                              case_rate = ifelse(case_rate < 10, NA, case_rate))
#plot.5  <- plot.5  %>% mutate(case_rate = ifelse(case_rate == 0, NA, case_rate),
#                              case_rate = ifelse(case_rate < 10, NA, case_rate))

## Period 1 

infs.1 <- tm_shape(plot.1) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "case_rate",
          style = "quantile",
          n = 10,
          palette = pal,
          title = "Infections per 100,000", 
          labels = c("Lowest decile", rep("", 8), "Highest decile"),
          showNA = TRUE,
          textNA = "No testing available") +
  
  tm_layout(title                 = "Period 1 (1 Jan 2020 - 29 Jun 2020); Wildtype",
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
            
            frame = FALSE)

## Period 2a
breaks <- quantile(plot.2a$case_rate, probs = seq(0, 1, 0.1), na.rm = TRUE)
breaks <- unique(breaks)
plot.2a$decile <- cut(
  plot.2a$case_rate,
  breaks = breaks,
  include.lowest = TRUE,
  labels = paste0("D", seq_len(length(breaks)-1)))
plot.2a$decile <- addNA(plot.2a$decile)
levels(plot.2a$decile)[is.na(levels(plot.2a$decile))] <- "< 10 observations"
plot.2a$decile <- factor(plot.2a$decile, levels = c("< 10 observations", paste0("D", seq_len(length(breaks)-1))))

infs.2a <- tm_shape(plot.2a) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "case_rate",
          style = "quantile",
          n = 10,
          palette = pal,
          title = "Infections per 100,000", 
          labels = c("Lowest decile", rep("", 8), "Highest decile"),
          showNA = FALSE) +
  
  tm_layout(title                 = "Period 2a (29 Jun 2020 - 30 Nov 2020); Wildtype",
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

## Period 2b
breaks <- quantile(plot.2b$case_rate, probs = seq(0, 1, 0.1), na.rm = TRUE)
breaks <- unique(breaks)
plot.2b$decile <- cut(
  plot.2b$case_rate,
  breaks = breaks,
  include.lowest = TRUE,
  labels = paste0("D", seq_len(length(breaks)-1)))
plot.2b$decile <- addNA(plot.2b$decile)
levels(plot.2b$decile)[is.na(levels(plot.2b$decile))] <- "< 10 observations"
plot.2b$decile <- factor(plot.2b$decile, levels = c("< 10 observations", paste0("D", seq_len(length(breaks)-1))))

infs.2b <- tm_shape(plot.2b) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "case_rate",
          style = "quantile",
          n = 10,
          palette = pal,
          title = "Infections per 100,000", 
          labels = c("Lowest decile", rep("", 8), "Highest decile"),
          showNA = FALSE) +
  
  tm_layout(title                 = "Period 2b (30 Nov 2020 - 1 Feb 2021); Wildtype",
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

## Period 3
breaks <- quantile(plot.3$case_rate, probs = seq(0, 1, 0.1), na.rm = TRUE)
breaks <- unique(breaks)
plot.3$decile <- cut(
  plot.3$case_rate,
  breaks = breaks,
  include.lowest = TRUE,
  labels = paste0("D", seq_len(length(breaks)-1)))
plot.3$decile <- addNA(plot.3$decile)
levels(plot.3$decile)[is.na(levels(plot.3$decile))] <- "< 10 observations"
plot.3$decile <- factor(plot.3$decile, levels = c("< 10 observations", paste0("D", seq_len(length(breaks)-1))))

infs.3 <- tm_shape(plot.3) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "case_rate",
          style = "quantile",
          n = 10,
          palette = pal,
          title = "Infections per 100,000", 
          labels = c("Lowest decile", rep("", 8), "Highest decile"),
          showNA = FALSE) +
  
  tm_layout(title                 = "Period 3 (1 Feb 2021 - 5 Jul 2021); Alpha",
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

## Period 4
breaks <- quantile(plot.4$case_rate, probs = seq(0, 1, 0.1), na.rm = TRUE)
breaks <- unique(breaks)
plot.4$decile <- cut(
  plot.4$case_rate,
  breaks = breaks,
  include.lowest = TRUE,
  labels = paste0("D", seq_len(length(breaks)-1)))
plot.4$decile <- addNA(plot.4$decile)
levels(plot.4$decile)[is.na(levels(plot.4$decile))] <- "< 10 observations"
plot.4$decile <- factor(plot.4$decile, levels = c("< 10 observations", paste0("D", seq_len(length(breaks)-1))))

infs.4 <- tm_shape(plot.4) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "case_rate",
          style = "quantile",
          n = 10,
          palette = pal,
          title = "Infections per 100,000", 
          labels = c("Lowest decile", rep("", 8), "Highest decile"),
          showNA = FALSE) +
  
  tm_layout(title                 = "Period 4 (5 Jul 2021 - 4 Oct 2021); Delta",
            title.fontface        = "bold",
            title.size            = .7,
            
            main.title            = "e",
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

## Period 5
breaks <- quantile(plot.5$case_rate, probs = seq(0, 1, 0.1), na.rm = TRUE)
breaks <- unique(breaks)
plot.5$decile <- cut(
  plot.5$case_rate,
  breaks = breaks,
  include.lowest = TRUE,
  labels = paste0("D", seq_len(length(breaks)-1)))
plot.5$decile <- addNA(plot.5$decile)
levels(plot.5$decile)[is.na(levels(plot.5$decile))] <- "< 10 observations"
plot.5$decile <- factor(plot.5$decile, levels = c("< 10 observations", paste0("D", seq_len(length(breaks)-1))))

infs.5 <- tm_shape(plot.5) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "case_rate",
          style = "quantile",
          n = 10,
          palette = pal,
          title = "Infections per 100,000", 
          labels = c("Lowest decile", rep("", 8), "Highest decile"),
          showNA = FALSE) +
  
  tm_layout(title                 = "Period 5 (4 Oct 2021 - 31 Dec 2021); Delta",
            title.fontface        = "bold",
            title.size            = .7,
            
            main.title            = "f",
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

# ----- 10. Save plots -----
tmap_save(infs.1, filename = paste0("20250922_Export/Fig1_Heatmap_CaseRate/Fig1a_heatmap_infs_n.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 600)

tmap_save(infs.2a, filename = paste0("20250922_Export/Fig1_Heatmap_CaseRate/Fig1b_heatmap_infs_n.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 600)

tmap_save(infs.2b, filename = paste0("20250922_Export/Fig1_Heatmap_CaseRate/Fig1c_heatmap_infs_n.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 600)

tmap_save(infs.3, filename = paste0("20250922_Export/Fig1_Heatmap_CaseRate/Fig1d_heatmap_infs_n.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 600)

tmap_save(infs.4,filename = paste0("20250922_Export/Fig1_Heatmap_CaseRate/Fig1e_heatmap_infs_n.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 600)

tmap_save(infs.5, filename = paste0("20250922_Export/Fig1_Heatmap_CaseRate/Fig1f_heatmap_infs_n.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 600)






# ----- 11. Manually assign deciles (hospitalisations -----
plot.1  <- plot.1  %>% mutate(hosp_rate = ifelse(hosp_rate == 0, NA, hosp_rate),
                              hosp_rate = ifelse(hosp_rate < 10, NA, hosp_rate))
plot.2a <- plot.2a %>% mutate(hosp_rate = ifelse(hosp_rate == 0, NA, hosp_rate),
                              hosp_rate = ifelse(hosp_rate < 10, NA, hosp_rate))
plot.2b <- plot.2b %>% mutate(hosp_rate = ifelse(hosp_rate == 0, NA, hosp_rate),
                              hosp_rate = ifelse(hosp_rate < 10, NA, hosp_rate))
plot.3  <- plot.3  %>% mutate(hosp_rate = ifelse(hosp_rate == 0, NA, hosp_rate),
                              hosp_rate = ifelse(hosp_rate < 10, NA, hosp_rate))
plot.4  <- plot.4  %>% mutate(hosp_rate = ifelse(hosp_rate == 0, NA, hosp_rate),
                              hosp_rate = ifelse(hosp_rate < 10, NA, hosp_rate))
plot.5  <- plot.5  %>% mutate(hosp_rate = ifelse(hosp_rate == 0, NA, hosp_rate),
                              hosp_rate = ifelse(hosp_rate < 10, NA, hosp_rate))


# ----- 12. Plot Hospitalisations for each period separately -----
pal <- c("white", brewer.pal(10, "YlOrBr"))

# Hospitalisations
## Period 1 
breaks <- quantile(plot.1$hosp_rate, probs = seq(0, 1, 0.1), na.rm = TRUE)
breaks <- unique(breaks)
plot.1$decile <- cut(
  plot.1$hosp_rate,
  breaks = breaks,
  include.lowest = TRUE,
  labels = paste0("D", seq_len(length(breaks)-1)))
plot.1$decile <- addNA(plot.1$decile)
levels(plot.1$decile)[is.na(levels(plot.1$decile))] <- "< 10 observations"
plot.1$decile <- factor(plot.1$decile, levels = c("< 10 observations", paste0("D", seq_len(length(breaks)-1))))


hosp.1 <- tm_shape(plot.1) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "decile",
          style = "fixed",
          n = 10,
          palette = pal,
          title = "Hospitalisations per 100,000", 
          labels = c("Censored", "Lowest decile", rep("", 8), "Highest decile"),
          showNA = FALSE) +
  
  tm_layout(title                 = "Period 1 (1 Jan 2020 - 29 Jun 2020); Wildtype",
            title.fontface        = "bold",
            title.size            = .7,
            
            main.title            = "g",
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

## Period 2a
breaks <- quantile(plot.2a$hosp_rate, probs = seq(0, 1, 0.1), na.rm = TRUE)
breaks <- unique(breaks)
plot.2a$decile <- cut(
  plot.2a$hosp_rate,
  breaks = breaks,
  include.lowest = TRUE,
  labels = paste0("D", seq_len(length(breaks)-1)))
plot.2a$decile <- addNA(plot.2a$decile)
levels(plot.2a$decile)[is.na(levels(plot.2a$decile))] <- "< 10 observations"
plot.2a$decile <- factor(plot.2a$decile, levels = c("< 10 observations", paste0("D", seq_len(length(breaks)-1))))


hosp.2a <- tm_shape(plot.2a) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "decile",
          style = "fixed",
          n = 10,
          palette = pal,
          title = "Hospitalisations per 100,000", 
          labels = c("Censored", "Lowest decile", rep("", 8), "Highest decile"),
          showNA = FALSE)+
  tm_layout(title                 = "Period 2a (29 Jun 2020 - 30 Nov 2020); Wildtype",
            title.fontface        = "bold",
            title.size            = .7,
            
            main.title            = "h",
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


## Period 2b
breaks <- quantile(plot.2b$hosp_rate, probs = seq(0, 1, 0.1), na.rm = TRUE)
breaks <- unique(breaks)
plot.2b$decile <- cut(
  plot.2b$hosp_rate,
  breaks = breaks,
  include.lowest = TRUE,
  labels = paste0("D", seq_len(length(breaks)-1)))
plot.2b$decile <- addNA(plot.2b$decile)
levels(plot.2b$decile)[is.na(levels(plot.2b$decile))] <- "< 10 observations"
plot.2b$decile <- factor(plot.2b$decile, levels = c("< 10 observations", paste0("D", seq_len(length(breaks)-1))))


hosp.2b <- tm_shape(plot.2b) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "decile",
          style = "fixed",
          n = 10,
          palette = pal,
          title = "Hospitalisations per 100,000", 
          labels = c("Censored", "Lowest decile", rep("", 8), "Highest decile"),
          showNA = FALSE) +
  
  tm_layout(title                 = "Period 2b (30 Nov 2020 - 1 Feb 2021); Wildtype",
            title.fontface        = "bold",
            title.size            = .7,
            
            main.title            = "i",
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


## Period 3
breaks <- quantile(plot.3$hosp_rate, probs = seq(0, 1, 0.1), na.rm = TRUE)
breaks <- unique(breaks)
plot.3$decile <- cut(
  plot.3$hosp_rate,
  breaks = breaks,
  include.lowest = TRUE,
  labels = paste0("D", seq_len(length(breaks)-1)))
plot.3$decile <- addNA(plot.3$decile)
levels(plot.3$decile)[is.na(levels(plot.3$decile))] <- "< 10 observations"
plot.3$decile <- factor(plot.3$decile, levels = c("< 10 observations", paste0("D", seq_len(length(breaks)-1))))

hosp.3 <- tm_shape(plot.3) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "decile",
          style = "fixed",
          n = 10,
          palette = pal,
          title = "Hospitalisations per 100,000", 
          labels = c("Censored", "Lowest decile", rep("", 8), "Highest decile"),
          
          showNA = FALSE) +
  
  tm_layout(title                 = "Period 3 (1 Feb 2021 - 5 Jul 2021); Alpha",
            title.fontface        = "bold",
            title.size            = .7,
            
            main.title            = "j",
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

## Period 4
breaks <- quantile(plot.4$hosp_rate, probs = seq(0, 1, 0.1), na.rm = TRUE)
breaks <- unique(breaks)
plot.4$decile <- cut(
  plot.4$hosp_rate,
  breaks = breaks,
  include.lowest = TRUE,
  labels = paste0("D", seq_len(length(breaks)-1)))
plot.4$decile <- addNA(plot.4$decile)
levels(plot.4$decile)[is.na(levels(plot.4$decile))] <- "< 10 observations"
plot.4$decile <- factor(plot.4$decile, levels = c("< 10 observations", paste0("D", seq_len(length(breaks)-1))))

hosp.4 <- tm_shape(plot.4) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "decile",
          style = "fixed",
          n = 10,
          palette = pal,
          title = "Hospitalisations per 100,000", 
          labels = c("Censored", "Lowest decile", rep("", 8), "Highest decile"),
          showNA = FALSE) +
  
  tm_layout(title                 = "Period 4 (5 Jul 2021 - 4 Oct 2021); Delta",
            title.fontface        = "bold",
            title.size            = .7,
            
            main.title            = "k",
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

## Period 5
breaks <- quantile(plot.5$hosp_rate, probs = seq(0, 1, 0.1), na.rm = TRUE)
breaks <- unique(breaks)
plot.5$decile <- cut(
  plot.5$hosp_rate,
  breaks = breaks,
  include.lowest = TRUE,
  labels = paste0("D", seq_len(length(breaks)-1)))
plot.5$decile <- addNA(plot.5$decile)
levels(plot.5$decile)[is.na(levels(plot.5$decile))] <- "< 10 observations"
plot.5$decile <- factor(plot.5$decile, levels = c("< 10 observations", paste0("D", seq_len(length(breaks)-1))))

hosp.5 <- tm_shape(plot.5) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "decile",
          style = "fixed",
          n = 10,
          palette = pal,
          title = "Hospitalisations per 100,000", 
          labels = c("Censored", "Lowest decile", rep("", 8), "Highest decile"),
          showNA = FALSE) +
  
  tm_layout(title                 = "Period 5 (4 Oct 2021 - 31 Dec 2021); Delta",
            title.fontface        = "bold",
            title.size            = .7,
            
            main.title            = "l",
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

# ----- 13. Save plots -----
tmap_save(hosp.1, filename = paste0("20250922_Export/Fig1_Heatmap_CaseRate/Fig1g_heatmap_hosp.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 450)

tmap_save(hosp.2a, filename = paste0("20250922_Export/Fig1_Heatmap_CaseRate/Fig1h_heatmap_hosp.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 450)

tmap_save(hosp.2b, filename = paste0("20250922_Export/Fig1_Heatmap_CaseRate/Fig1i_heatmap_hosp.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 450)

tmap_save(hosp.3, filename = paste0("20250922_Export/Fig1_Heatmap_CaseRate/Fig1j_heatmap_hosp.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 450)

tmap_save(hosp.4,filename = paste0("20250922_Export/Fig1_Heatmap_CaseRate/Fig1k_heatmap_hosp.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 450)

tmap_save(hosp.5, filename = paste0("20250922_Export/Fig1_Heatmap_CaseRate/Fig1l_heatmap_hosp.jpeg"), 
          width = 15, height = 6.5, units = "in", dpi = 450)


