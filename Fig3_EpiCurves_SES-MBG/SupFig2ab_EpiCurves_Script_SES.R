# Import libraries
libs <- c("ggplot2", "tidyverse", "janitor", "lubridate")
lapply(libs, library, character.only = TRUE)

# Set working directory
setwd("H:\\Carsten - First glance at data")

# DATA IMPORT AND PREP
source("CrucialScripts_DONOTMOVE/DataPrep.R")
source("CrucialScripts_DONOTMOVE/Mob_Thresholds.R")
source("CrucialScripts_DONOTMOVE/Str_Index.R")

tot_pop <- read_csv("H:/Carsten - First glance at data/CrucialScripts_DONOTMOVE/Data Hosp_Rep_Cases/20240805_tot_pop_V4.csv")
tot_pop <- tot_pop %>% filter(Age >= 18)



# ----- 0. Data preparation -----
# Check extracted quantiles
# Q1 = -2.3300764 to -0.3774767 , 
# Q2 = -0.3774767 to 0.2172156, 
# Q3 = 0.2172156 to 0.4768423, 
# Q4 = 0.4768423 to 0.7315786,
# Q5 = 0.7315786 to 1.0359707

quantile(tot_pop$SES_I, probs = seq(0,1,.2), na.rm = TRUE)

# ----- 1. Assign SES quitiles for infections and hospitalisations and calculate rates
# Infections
plot_infections <- RepCases %>%
  mutate(SES_Q = case_when(
    SES_mean < quantile(tot_pop$SES_I, probs = c(0.2), na.rm = TRUE) ~ "Q1", 
    SES_mean >= quantile(tot_pop$SES_I, probs = c(0.2), na.rm = TRUE) & SES_mean < quantile(tot_pop$SES_I, probs = c(0.4), na.rm = TRUE) ~ "Q2",
    SES_mean >= quantile(tot_pop$SES_I, probs = c(0.4), na.rm = TRUE) & SES_mean < quantile(tot_pop$SES_I, probs = c(0.6), na.rm = TRUE) ~ "Q3",
    SES_mean >= quantile(tot_pop$SES_I, probs = c(0.6), na.rm = TRUE) & SES_mean < quantile(tot_pop$SES_I, probs = c(0.8), na.rm = TRUE) ~ "Q4",
    SES_mean >= quantile(tot_pop$SES_I, probs = c(0.8), na.rm = TRUE) ~ "Q5"),
    date_confirmed = as.Date(COVID19_dateconfirmed),
    week_begin     = floor_date(date_confirmed, "weeks", week_start = 1)) %>%
  group_by(date_confirmed, SES_Q) %>% # Count positive cases per day
  summarize(case_obs = n()) 

# Hospitalisations
plot_hospitalisations <- HospCases %>%
  mutate(SES_Q = case_when(
    SES_I < quantile(tot_pop$SES_I, probs = c(0.2), na.rm = TRUE) ~ "Q1", 
    SES_I >= quantile(tot_pop$SES_I, probs = c(0.2), na.rm = TRUE) & SES_I < quantile(tot_pop$SES_I, probs = c(0.4), na.rm = TRUE) ~ "Q2",
    SES_I >= quantile(tot_pop$SES_I, probs = c(0.4), na.rm = TRUE) & SES_I < quantile(tot_pop$SES_I, probs = c(0.6), na.rm = TRUE) ~ "Q3",
    SES_I >= quantile(tot_pop$SES_I, probs = c(0.6), na.rm = TRUE) & SES_I < quantile(tot_pop$SES_I, probs = c(0.8), na.rm = TRUE) ~ "Q4",
    SES_I >= quantile(tot_pop$SES_I, probs = c(0.8), na.rm = TRUE) ~ "Q5"),
    date_confirmed = as.Date(LBZOpnamedatum),
    week_begin     = floor_date(date_confirmed, "weeks", week_start = 1)) %>%
  group_by(date_confirmed, SES_Q) %>% # Count positive cases per day
  summarize(hosp_obs = n()) 


# ----- 2. Create template dataframe: all dates in 2020 and 2021 -----
df.day <- data.frame(day_begin = as.Date("2020-01-01")) %>%
  complete(day_begin = seq.Date(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  mutate(day_id = c(1:length(day_begin)),
         day_begin = as.Date(day_begin))

# Create 5 entries for every date (one for each SES quintile)
SES_categories <- c("Q1", "Q2", "Q3", "Q4", "Q5")

df.day <- df.day %>%
  slice(rep(1:n(), each = 5)) %>%
  group_by(day_begin) %>%
  mutate(rep_id = row_number(),
         SES_Q  = SES_categories[rep_id]) %>%
  ungroup()


# ----- 3. Join infections and hospitalisations to template -----
plot <- df.day %>%
  left_join(plot_infections,       by = c("day_begin" = "date_confirmed",
                                          "SES_Q"     = "SES_Q")) %>%
  left_join(plot_hospitalisations, by = c("day_begin" = "date_confirmed",
                                          "SES_Q"     = "SES_Q")) %>%
  mutate(case_obs_f = ifelse(case_obs < 5, NA, case_obs),
         hosp_obs_f = ifelse(hosp_obs < 5, NA, hosp_obs))



# ----- 4. Compute 7-day rolling averages -----
# Do not forget to filter out dates with <3 observations.
plot <- plot %>%
  group_by(SES_Q) %>%
  mutate(case_obs_ra = slider::slide_index(case_obs_f, .i = day_begin, .f = ~mean(.x, na.rm = TRUE), .before = 6, .complete = FALSE), 
         case_obs_ra = round(unlist(case_obs_ra), 3),
         hosp_obs_ra = slider::slide_index(hosp_obs_f, .i = day_begin, .f = ~mean(.x, na.rm = TRUE), .before = 6, .complete = FALSE), 
         hosp_obs_ra = round(unlist(hosp_obs_ra), 3),
         case_obs_ra_f  = ifelse(case_obs_ra < 5, NA, case_obs_ra),
         hosp_obs_ra_f  = ifelse(hosp_obs_ra < 5, NA, hosp_obs_ra))


# Write resulting dataset to a .csv format
#write.csv(plot, paste0(format(Sys.Date(), "%Y%m%d"), "_Fig3_Data_Epicurve_SES_V4.csv"))

# ----- 5. Plot results for infections and hospitalisations -----

#plot <- read_csv("H:/Carsten - First glance at data/Manuscript/Fig3_EpiCurves_SES-MBG/Fig3_Data_Epicurve_SES_V4.csv")



Sys.setlocale("LC_TIME", "C")

custom_palette     <- c("#17bebb", "#8BC061", "#FFC107", "#F28220", "#e54339")
custom_palette_rev <- c("#e54339", "#F28220", "#FFC107", "#8BC061", "#17bebb")

# Infections
ggplot() +
  geom_line(data = plot, aes(x = day_begin, y = case_obs_ra_f, colour = SES_Q), linewidth = 1) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y",
               limits = c(as.Date("2019-12-31"), as.Date("2022-01-03")),
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 40000, 500)) +
  theme_classic() +
  labs(x = element_blank(), 
       y = "Number of infections",
       colour = "SES-score \n(Quintile)",
       fill = "Stringency \nIndex",
       tag = "a") +
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold")) +
  scale_color_manual(name = "SES-score \n(Quintile)", values = custom_palette_rev, labels = c("Q1", "Q2", "Q3", "Q4", "Q5")) +
  geom_rect(data = si_df, aes(xmin = start_date, xmax = end_date,
                              ymin = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.05 * max(plot$case_obs_ra_f, na.rm = TRUE),
                              ymax = max(plot$case_obs_ra_f, na.rm = TRUE) + 2 * 0.05 * diff(range(plot$case_obs_ra_f, na.rm = TRUE)), 
                              fill = si), alpha = .75) +
  
  scale_fill_gradient2(low = "white", mid = "tomato", high = "purple4", midpoint = (max(si_df$si)/2)) +
  
  geom_vline(xintercept = as.Date("2020-06-29"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2020-11-30"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-02-01"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-07-05"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2021-10-04"), colour = "black", linetype = 2, linewidth = 0.5) +
  
  annotate("text", x = as.Date("2020-04-20"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.035*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 1",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2020-09-15"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.035*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 2a", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-01-01"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.035*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 2b", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-04-18"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.035*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 3",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-08-18"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.035*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 4",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-11-20"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.035*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 5",  colour = "black", fontface = "bold") 

# Save plot
ggname <- "Fig3a_EpiCurve_SES_Infs_V4.jpeg"
ggsave(ggname, width = 15, height = 6.5, units = "in", dpi = 600)



## Hospitalisations
ggplot() +
  geom_line(data = plot, aes(x = day_begin, y = hosp_obs_ra_f, colour = SES_Q), linewidth = 1) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y",
               limits = c(as.Date("2019-12-31"), as.Date("2022-01-03")),
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 40000, 25)) +
  theme_classic() +
  labs(x = element_blank(), 
       y = "Number of hospitalisations",
       fill = "Stringency \nIndex",
       tag = "b") +
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold")) +
  scale_color_manual(name = "SES-score \n(Quintile)", values = custom_palette_rev, labels = c("Q1", "Q2", "Q3", "Q4", "Q5")) +
  geom_rect(data = si_df, aes(xmin = start_date, xmax = end_date,
                              ymin = max(plot$hosp_obs_ra_f, na.rm = TRUE) + 0.05 * max(plot$hosp_obs_ra_f, na.rm = TRUE),
                              ymax = max(plot$hosp_obs_ra_f, na.rm = TRUE) + 2 * 0.05 * diff(range(plot$hosp_obs_ra_f, na.rm = TRUE)), 
                              fill = si), alpha = .75) +
  
  scale_fill_gradient2(low = "white", mid = "tomato", high = "purple4", midpoint = (max(si_df$si)/2)) +
  geom_vline(xintercept = as.Date("2020-06-29"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2020-11-30"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-02-01"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-07-05"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2021-10-04"), colour = "black", linetype = 2, linewidth = 0.5) +
  
  annotate("text", x = as.Date("2020-04-20"), y = max(plot$hosp_obs_ra_f, na.rm = TRUE) + 0.035*max(plot$hosp_obs_ra_f, na.rm = TRUE), label = "Period 1",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2020-09-15"), y = max(plot$hosp_obs_ra_f, na.rm = TRUE) + 0.035*max(plot$hosp_obs_ra_f, na.rm = TRUE), label = "Period 2a", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-01-01"), y = max(plot$hosp_obs_ra_f, na.rm = TRUE) + 0.035*max(plot$hosp_obs_ra_f, na.rm = TRUE), label = "Period 2b", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-04-18"), y = max(plot$hosp_obs_ra_f, na.rm = TRUE) + 0.035*max(plot$hosp_obs_ra_f, na.rm = TRUE), label = "Period 3",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-08-18"), y = max(plot$hosp_obs_ra_f, na.rm = TRUE) + 0.035*max(plot$hosp_obs_ra_f, na.rm = TRUE), label = "Period 4",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-11-20"), y = max(plot$hosp_obs_ra_f, na.rm = TRUE) + 0.035*max(plot$hosp_obs_ra_f, na.rm = TRUE), label = "Period 5",  colour = "black", fontface = "bold") 


# Save plot
ggname <- "Fig3b_EpiCurve_SES_Hosp_V4.jpeg"
ggsave(ggname, width = 15, height = 6.5, units = "in", dpi = 600)





































