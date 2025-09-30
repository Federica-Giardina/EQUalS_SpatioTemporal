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
SSB.ref <- read_sav("K:/Utilities/Code_Listings/SSBreferentiebestanden/LANDAKTUEELREFV13.sav")

tot_pop <- tot_pop %>% filter(Age >= 18)
SSB.ref <- SSB.ref %>% select(LAND, LANDTYPE, LANDAKTUEEL5)


# ----- 0. Data preparation -----
# Add Information on counry of origin
RepCases <- RepCases %>%
  mutate(HERKOMSTLAND = as.character(GBAHERKOMSTLAND)) %>%
  left_join(SSB.ref, by = c("HERKOMSTLAND" = "LAND")) %>%
  mutate(
    MBG = case_when(GBAGEBOORTELAND != "6030" ~ "Migrant",
                    GBAGEBOORTELAND == "6030" & GBAHERKOMSTLAND != "6030" ~ "Child of migrant(s)",
                    GBAHERKOMSTLAND == "6030" ~ "Not a Migrant"),
    MBG_group = 
      case_when(MBG == "Migrant" & LANDAKTUEEL5 == "2" ~ "Marocco",
                MBG == "Migrant" & LANDAKTUEEL5 == "3" ~ "Turkiye",
                MBG == "Migrant" & LANDAKTUEEL5 == "4" ~ "Surinam",
                MBG == "Migrant" & LANDAKTUEEL5 == "5" ~ "Antilles",
                MBG == "Migrant" &
                  LANDAKTUEEL5 != "2" &
                  LANDAKTUEEL5 != "3" &
                  LANDAKTUEEL5 != "4" & 
                  LANDAKTUEEL5 != "5" ~ "Other",
                MBG == "Child of Migrant(s)" ~ "Child of migrant(s)",
                MBG == "Not a Migrant" ~ "Not a Migrant"))



# ----- 1. Calculated number of infections and hospitalisations among MBG-groups -----
# Infections
plot_infections <- RepCases %>%
  mutate(date_confirmed = as.Date(COVID19_dateconfirmed),
         week_begin     = floor_date(date_confirmed, "weeks", week_start = 1)) %>%
  group_by(date_confirmed, MBG) %>%
  summarize(case_obs = n()) 

# Hospitalisations
plot_hospitalisations <- HospCases %>%
  mutate(date_confirmed = as.Date(LBZOpnamedatum),
         week_begin     = floor_date(date_confirmed, "weeks", week_start = 1)) %>%
  group_by(date_confirmed, MBG) %>%
  summarize(hosp_obs = n()) 


# ----- 2. Create template dataframe: all dates in 2020 and 2021 -----
df.day <- data.frame(day_begin = as.Date("2020-01-01")) %>%
  complete(day_begin = seq.Date(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  mutate(day_id = c(1:length(day_begin)),
         day_begin = as.Date(day_begin))

# Create 3 entries for every date (one for each migrant group)
MBG_categories <- c("Migrant", "Child of migrant(s)", "Not a Migrant")

df.day <- df.day %>%
  slice(rep(1:n(), each = 3)) %>%
  group_by(day_begin) %>%
  mutate(rep_id = row_number(),
         MBG_C  = MBG_categories[rep_id]) %>%
  ungroup()


# ----- 3. Join infections and hospitalisations to template -----
plot <- df.day %>%
  left_join(plot_infections,       by = c("day_begin" = "date_confirmed",
                                          "MBG_C"     = "MBG")) %>%
  left_join(plot_hospitalisations, by = c("day_begin" = "date_confirmed",
                                          "MBG_C"     = "MBG")) %>%
  mutate(case_obs_f = ifelse(case_obs < 5, NA, case_obs),
         hosp_obs_f = ifelse(hosp_obs < 5, NA, hosp_obs))
 #  mutate(hosp_obs = ifelse(is.na(hosp_obs), 0, hosp_obs),
 #         case_obs = ifelse(is.na(case_obs), 0, case_obs))


# ----- 4. Compute case rates per 100.000 in category and 7-day rolling averages -----
# 4.1 - Calculate case rates / 100.000 using the total population dataset imported earlier.
# 4.2 - Calculate rolling averages using the slider package.
# 4.3 - Do not forget to filter out dates with <3 observations.

plot <- plot %>%
  group_by(MBG_C) %>%
  mutate(MBG_case_rate = case_when(MBG_C == "Migrant"             ~ round(case_obs_f * 100000 / sum(tot_pop$MBG == "Migrant",              na.rm = TRUE) , 5),
                                   MBG_C == "Child of migrant(s)" ~ round(case_obs_f * 100000 / sum(tot_pop$MBG == "Child of migrant(s)",  na.rm = TRUE) , 5),
                                   MBG_C == "Not a Migrant"       ~ round(case_obs_f * 100000 / sum(tot_pop$MBG == "Not a Migrant",        na.rm = TRUE) , 5)),
         MBG_hosp_rate = case_when(MBG_C == "Migrant"             ~ round(hosp_obs_f * 100000 / sum(tot_pop$MBG == "Migrant",              na.rm = TRUE) , 5),
                                   MBG_C == "Child of migrant(s)" ~ round(hosp_obs_f * 100000 / sum(tot_pop$MBG == "Child of migrant(s)",  na.rm = TRUE) , 5),
                                   MBG_C == "Not a Migrant"       ~ round(hosp_obs_f * 100000 / sum(tot_pop$MBG == "Not a Migrant",        na.rm = TRUE) , 5)),
         case_rate_ra = slider::slide_index(MBG_case_rate, .i = day_begin, .f = ~mean(.x, na.rm = TRUE), .before = 6, .complete = FALSE), 
         case_rate_ra = round(unlist(case_rate_ra), 3),
         hosp_rate_ra = slider::slide_index(MBG_hosp_rate, .i = day_begin, .f = ~mean(.x, na.rm = TRUE), .before = 6, .complete = FALSE), 
         hosp_rate_ra = round(unlist(hosp_rate_ra), 3)#,
         #case_rate_ra_f  = ifelse(case_rate_ra < 3, NA, case_rate_ra),
         #hosp_rate_ra_f  = ifelse(hosp_rate_ra < 3, NA, hosp_rate_ra)
         ) %>%
  ungroup() %>%
  mutate(MBG_C = factor(MBG_C, levels = c("Migrant", "Child of migrant(s)", "Not a Migrant")))


# Write resulting dataset to a .csv format
write.csv(plot, paste0(format(Sys.Date(), "%Y%m%d"), "_Fig3_Data_Epicurve_MBG_V4.csv"))

Sys.setlocale("LC_TIME", "C")

#plot <- read_csv("H:/Carsten - First glance at data/Manuscript/Fig3_EpiCurves_SES-MBG/Fig3_Data_Epicurve_MBG_V2.csv")

# ----- 5. Plot results for infections and hospitalisations -----
# Define custom colour palettes
custom_palette      <- c("#17bebb",  "#FFC107", "#e54339")
custom_palette_rev  <- rev(custom_palette)
custom_palette_full <- c("#e54339", "#F28220", "#FFC107", "#8BC061", "#17bebb")

# Infections
ggplot() +
  geom_line(data = plot, aes(x = as.Date(day_begin), y = case_rate_ra, colour = MBG_C), linewidth = 1) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y",
               limits = c(as.Date("2019-12-31"), as.Date("2022-01-03")),
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 200, 25)) +
  theme_classic() +
  labs(x = element_blank(), 
       y = "Number of infections (per 100,000 population)",
       colour = "MBG cat",
       fill = "Stringency \nIndex",
       tag = "c") +
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold")) +
  
  scale_color_manual(name = "Migration background", values = custom_palette, labels = c("Migrant", "Child of migrant(s)", "Not a Migrant")) +
  geom_rect(data = si_df, aes(xmin = start_date, xmax = end_date,
                              ymin = max(plot$case_rate_ra, na.rm = TRUE) + 0.05 * max(plot$case_rate_ra, na.rm = TRUE),
                              ymax = max(plot$case_rate_ra, na.rm = TRUE) + 2 * 0.05 * diff(range(plot$case_rate_ra, na.rm = TRUE)), 
                              fill = si), alpha = .75) +
  scale_fill_gradient2(low = "white", mid = "tomato", high = "purple4", midpoint = (max(si_df$si)/2)) +
  geom_vline(xintercept = as.Date("2020-06-29"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2020-11-30"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-02-01"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-07-05"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2021-10-04"), colour = "black", linetype = 2, linewidth = 0.5) +
  
  annotate("text", x = as.Date("2020-04-20"), y = max(plot$case_rate_ra, na.rm = TRUE) + 0.035*max(plot$case_rate_ra, na.rm = TRUE), label = "Period 1",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2020-09-15"), y = max(plot$case_rate_ra, na.rm = TRUE) + 0.035*max(plot$case_rate_ra, na.rm = TRUE), label = "Period 2a", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-01-01"), y = max(plot$case_rate_ra, na.rm = TRUE) + 0.035*max(plot$case_rate_ra, na.rm = TRUE), label = "Period 2b", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-04-18"), y = max(plot$case_rate_ra, na.rm = TRUE) + 0.035*max(plot$case_rate_ra, na.rm = TRUE), label = "Period 3",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-08-18"), y = max(plot$case_rate_ra, na.rm = TRUE) + 0.035*max(plot$case_rate_ra, na.rm = TRUE), label = "Period 4",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-11-20"), y = max(plot$case_rate_ra, na.rm = TRUE) + 0.035*max(plot$case_rate_ra, na.rm = TRUE), label = "Period 5",  colour = "black", fontface = "bold") 

# Save plot
ggname <- "Fig3c_EpiCurve_MBG_Infs_V4.jpeg"

ggsave(ggname, width = 15, height = 6.5, units = "in", dpi = 600)



# Hospitalisations
ggplot() +
  geom_line(data = plot, aes(x = day_begin, y = hosp_rate_ra, colour = MBG_C), linewidth = 1) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y",
               limits = c(as.Date("2019-12-31"), as.Date("2022-01-03")),
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 250, .5)) +
  theme_classic() +
  labs(x = element_blank(), 
       y = "Number of hospitalisations (per 100,000 population)",
       fill = "Stringency \nIndex",
       tag = "d") +
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold")) +
  scale_color_manual(name = "MBG category", 
                     values = custom_palette, 
                     labels = c("Migrant", "Child of migrant(s)", "Not a Migrant")) +
  geom_rect(data = si_df, aes(xmin = start_date, xmax = end_date,
                              ymin = max(plot$hosp_rate_ra, na.rm = TRUE) + 0.05 * max(plot$hosp_rate_ra, na.rm = TRUE),
                              ymax = max(plot$hosp_rate_ra, na.rm = TRUE) + 2 * 0.05 * diff(range(plot$hosp_rate_ra, na.rm = TRUE)), 
                              fill = si), alpha = .75) +
  scale_fill_gradient2(low = "white", mid = "tomato", high = "purple4", midpoint = (max(si_df$si)/2)) +
  geom_vline(xintercept = as.Date("2020-06-29"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2020-11-30"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-02-01"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-07-05"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2021-10-04"), colour = "black", linetype = 2, linewidth = 0.5) +
  
  annotate("text", x = as.Date("2020-04-20"), y = max(plot$hosp_rate_ra, na.rm = TRUE) + 0.035*max(plot$hosp_rate_ra, na.rm = TRUE), label = "Period 1",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2020-09-15"), y = max(plot$hosp_rate_ra, na.rm = TRUE) + 0.035*max(plot$hosp_rate_ra, na.rm = TRUE), label = "Period 2a", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-01-01"), y = max(plot$hosp_rate_ra, na.rm = TRUE) + 0.035*max(plot$hosp_rate_ra, na.rm = TRUE), label = "Period 2b", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-04-18"), y = max(plot$hosp_rate_ra, na.rm = TRUE) + 0.035*max(plot$hosp_rate_ra, na.rm = TRUE), label = "Period 3",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-08-18"), y = max(plot$hosp_rate_ra, na.rm = TRUE) + 0.035*max(plot$hosp_rate_ra, na.rm = TRUE), label = "Period 4",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-11-20"), y = max(plot$hosp_rate_ra, na.rm = TRUE) + 0.035*max(plot$hosp_rate_ra, na.rm = TRUE), label = "Period 5",  colour = "black", fontface = "bold") 


# Save plot
ggname <- "Fig3d_EpiCurve_MBG_Hosp_V4.jpeg"
ggsave(ggname, width = 15, height = 6.5, units = "in", dpi = 600)









