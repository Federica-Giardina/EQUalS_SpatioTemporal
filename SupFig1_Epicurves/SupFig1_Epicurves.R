# Import libraries
libs <- c("dplyr", "ggplot2")
lapply(libs, library, character.only = TRUE)

# Import Stringency Index data
source("C:/Users/z710214/OneDrive - Radboudumc/z710214/My Documents/PhD/S5 - EQUalS/Exports/20241217 Export/20241217 Export/Scripts/Str_I_script.R")

# Set time to English (for axes)
Sys.setlocale("LC_TIME", "C") 

# Import data
data <- read.csv("C:/Users/Z710214/OneDrive - Radboudumc/z710214/My Documents/PhD/S5 - EQUalS/Exports/20241227 Export adjusted/cases_week.csv", sep = ";")

# Define y-ranges for stringency index
y_max <- max(data$hosp_obs_ra_f, na.rm = TRUE)
y_overlay_si.min  <- max(data$hosp_obs_ra_f, na.rm = TRUE) * 1.05
y_overlay_si.max  <- max(data$hosp_obs_ra_f, na.rm = TRUE) * 1.075


# ----- 1. Specify ends of periods -----
p1.end  <- as.Date("2020-06-29")
p2a.end <- as.Date("2020-11-30")
p2b.end <- as.Date("2021-02-01")
p3.end  <- as.Date("2021-07-05")
p4.end  <- as.Date("2021-10-04")
p5.end  <- as.Date("2021-12-31")

# ----- 2. Define highlighted and shaded areas ----
data <- data %>% mutate(date = as.Date(date, format = "%d-%m-%Y"))

data.p1  <- data %>% mutate(highlight = ifelse(date >= as.Date("2020-01-01") & date <= p1.end, hosp_obs_ra_f, NA),
                            other     = ifelse(date <  as.Date("2020-01-01") | date >  p1.end, hosp_obs_ra_f, NA))
data.p2a  <- data %>% mutate(highlight = ifelse(date >= p1.end  & date <= p2a.end, hosp_obs_ra_f, NA),
                             other     = ifelse(date <  p1.end  | date >  p2a.end, hosp_obs_ra_f, NA))
data.p2b  <- data %>% mutate(highlight = ifelse(date >= p2a.end & date <= p2b.end, hosp_obs_ra_f, NA),
                             other     = ifelse(date <  p2a.end | date >  p2b.end, hosp_obs_ra_f, NA))
data.p3   <- data %>% mutate(highlight = ifelse(date >= p2b.end & date <= p3.end, hosp_obs_ra_f, NA),
                             other     = ifelse(date <  p2b.end | date >  p3.end, hosp_obs_ra_f, NA))
data.p4   <- data %>% mutate(highlight = ifelse(date >= p3.end  & date <= p4.end, hosp_obs_ra_f, NA),
                             other     = ifelse(date <  p3.end  | date >  p4.end, hosp_obs_ra_f, NA))
data.p5   <- data %>% mutate(highlight = ifelse(date >= p4.end  & date <= p5.end, hosp_obs_ra_f, NA),
                             other     = ifelse(date <  p4.end  | date >  p5.end, hosp_obs_ra_f, NA))

# ----- 3. Define data for plotting! -----
# Change according to period to be highlighted.
plot    <- data.p1
p.begin <- as.Date("2020-01-01")
p.end   <- p1.end

# ----- 4. Plot Figure! -----
ggplot() +
  geom_line(data = plot, aes(x = date, y = highlight), colour = "#fe9929",  size = 2.50) + 
  geom_line(data = plot, aes(x = date, y = other),     colour = "darkgrey", size = 2.00) + 
  theme_classic() +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y",
               limits = c(as.Date("2020-01-01"), as.Date("2022-01-03")),
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), 
                     breaks = seq(0, 40000, 50),
                     limits = c(0, 560)) + 
  labs(x = element_blank(), y = "Number of COVID-19 hospitalisations", fill = "Stringency \nIndex") +
  theme(legend.position = "none",
        axis.text.x = element_blank()) + 
  #geom_rect(data = si_df, aes(xmin = as.Date(start_date), xmax = as.Date(end_date),
  #                          ymin = y_overlay_si.min, ymax = y_overlay_si.max,
  #                          fill = si), alpha = .75) +
  #scale_fill_gradient2(low = "white", mid = "tomato", high = "Purple4", midpoint = (max(si_df$si)/2)) +
  
  annotate("rect", 
           xmin = p.begin,
           xmax = p.end,
           ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.50) +
  
  #annotate("rect", xmin = min(plot$date), xmax = p.begin - 1,
  #         ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
  #annotate("rect", xmin = p.end + 1, xmax = max(plot$date),
  #         ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2020-06-29"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2020-11-30"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-02-01"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-07-05"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2021-10-04"), colour = "black", linetype = 2, linewidth = 0.5) +
  annotate("text", x = as.Date("2020-04-20"), y = max(plot$hosp_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$hosp_obs_ra_f, na.rm = TRUE), label = "Period 1",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2020-09-15"), y = max(plot$hosp_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$hosp_obs_ra_f, na.rm = TRUE), label = "Period 2a", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-01-01"), y = max(plot$hosp_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$hosp_obs_ra_f, na.rm = TRUE), label = "Period 2b", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-04-18"), y = max(plot$hosp_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$hosp_obs_ra_f, na.rm = TRUE), label = "Period 3",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-08-18"), y = max(plot$hosp_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$hosp_obs_ra_f, na.rm = TRUE), label = "Period 4",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-11-20"), y = max(plot$hosp_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$hosp_obs_ra_f, na.rm = TRUE), label = "Period 5",  colour = "black", fontface = "bold") 


# ----- 5. Save plotted object! -----
# Set Working directory
setwd("C:/Users/Z710214/OneDrive - Radboudumc/z710214/My Documents/PhD/S5 - EQUalS/Writing (Carsten)/")

#  Specify Figure name 
ggname <- "Fig1_Epicurve_hosp_p1.jpeg"
#ggname <- "Fig1_Epicurve_hosp_p2a.jpeg"
#ggname <- "Fig1_Epicurve_hosp_p2b.jpeg"
#ggname <- "Fig1_Epicurve_hosp_p3.jpeg"
#ggname <- "Fig1_Epicurve_hosp_p4.jpeg"
#ggname <- "Fig1_Epicurve_hosp_p5.jpeg"

# Save ggplot2 object
ggsave(ggname, width = 15, height = 6.5, units = "in", dpi = 600)





# Infections
# ----- 6. Define y-ranges for Str. Index plotting -----
y_max <- max(data$case_obs_ra_f, na.rm = TRUE)
y_overlay_si.min  <- max(data$case_obs_ra_f, na.rm = TRUE) * 1.05
y_overlay_si.max  <- max(data$case_obs_ra_f, na.rm = TRUE) * 1.075

# ----- 7. Define highlighted and shaded areas ----
data.p1  <- data %>% mutate(highlight = ifelse(date >= as.Date("2020-01-01") & date <= p1.end, case_obs_ra_f, NA),
                            other     = ifelse(date <  as.Date("2020-01-01") | date >  p1.end, case_obs_ra_f, NA))
data.p2a  <- data %>% mutate(highlight = ifelse(date >= p1.end  & date <= p2a.end, case_obs_ra_f, NA),
                             other     = ifelse(date <  p1.end  | date >  p2a.end, case_obs_ra_f, NA))
data.p2b  <- data %>% mutate(highlight = ifelse(date >= p2a.end & date <= p2b.end, case_obs_ra_f, NA),
                             other     = ifelse(date <  p2a.end | date >  p2b.end, case_obs_ra_f, NA))
data.p3   <- data %>% mutate(highlight = ifelse(date >= p2b.end & date <= p3.end, case_obs_ra_f, NA),
                             other     = ifelse(date <  p2b.end | date >  p3.end, case_obs_ra_f, NA))
data.p4   <- data %>% mutate(highlight = ifelse(date >= p3.end  & date <= p4.end, case_obs_ra_f, NA),
                             other     = ifelse(date <  p3.end  | date >  p4.end, case_obs_ra_f, NA))
data.p5   <- data %>% mutate(highlight = ifelse(date >= p4.end  & date <= p5.end, case_obs_ra_f, NA),
                             other     = ifelse(date <  p4.end  | date >  p5.end, case_obs_ra_f, NA))

# ----- 8. Define data for plotting! -----
# Change according to period to be highlighted.
plot    <- data.p1
p.begin <- as.Date("2020-01-01")
p.end   <- p1.end

# ----- 9. Plot Figure! -----
ggplot() +
  geom_line(data = plot, aes(x = date, y = highlight), colour = "#fe9929",  size = 2.50) + 
  geom_line(data = plot, aes(x = date, y = other),     colour = "darkgrey", size = 2.00) + 
  theme_classic() +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y",
               limits = c(as.Date("2020-01-01"), as.Date("2022-01-03")),
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 40000, 1250),
                     limits = c(0, 17250)) + 
  labs(x = element_blank(), y = "Number of notified COVID-19 infections", fill = "Stringency \nIndex") +
  theme(legend.position = "none",
        axis.text.x = element_blank()) + 
  #geom_rect(data = si_df, aes(xmin = as.Date(start_date), xmax = as.Date(end_date),
  #                          ymin = y_overlay_si.min, ymax = y_overlay_si.max,
  #                          fill = si), alpha = .75) +
  #scale_fill_gradient2(low = "white", mid = "tomato", high = "Purple4", midpoint = (max(si_df$si)/2)) +
  
  annotate("rect", 
           xmin = p.begin,
           xmax = p.end,
           ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.50) +
  
  #annotate("rect", xmin = min(plot$date), xmax = p.begin - 1,
  #         ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
  #annotate("rect", xmin = p.end + 1, xmax = max(plot$date),
  #         ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +  geom_vline(xintercept = as.Date("2020-06-29"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2020-06-29"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2020-11-30"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-02-01"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-07-05"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2021-10-04"), colour = "black", linetype = 2, linewidth = 0.5) +
  annotate("text", x = as.Date("2020-04-20"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 1",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2020-09-15"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 2a", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-01-01"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 2b", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-04-18"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 3",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-08-18"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 4",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-11-20"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 5",  colour = "black", fontface = "bold") 


# ----- 10. Save plotted object! -----
# Set Working directory
setwd("C:/Users/Z710214/OneDrive - Radboudumc/z710214/My Documents/PhD/S5 - EQUalS/Writing (Carsten)/")

#  Specify Figure name 
#ggname <- "Fig1_Epicurve_infs_p1.jpeg"
#ggname <- "Fig1_Epicurve_infs_p2a.jpeg"
#ggname <- "Fig1_Epicurve_infs_p2b.jpeg"
#ggname <- "Fig1_Epicurve_infs_p3.jpeg"
#ggname <- "Fig1_Epicurve_infs_p4.jpeg"
#ggname <- "Fig1_Epicurve_infs_p5.jpeg"

# Save ggplot2 object
ggsave(ggname, width = 15, height = 6.5, units = "in", dpi = 600)





# ----- 11. Plot Figure without ONE specific shaded period! -----
ggplot() +
  geom_line(data = data, aes(x = date, y = case_obs_ra_f), colour = "#fe9929",  size = 1.5) + 
  theme_classic() +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y",
               limits = c(as.Date("2020-01-01"), as.Date("2022-01-03")),
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), 
                     breaks = seq(0, 40000, 2500),
                     limits = c(0, NA)) + 
  labs(x = element_blank(), y = "Number of COVID-19 hospitalisations", fill = "Stringency \nIndex", tag = "a") +
  theme(legend.position = "right", 
        plot.tag = element_text(face = "bold")) + 
  geom_rect(data = si_df, aes(xmin = as.Date(start_date), xmax = as.Date(end_date),
                              ymin = y_overlay_si.min, ymax = y_overlay_si.max,
                              fill = si), alpha = .75) +
  scale_fill_gradient2(low = "white", mid = "tomato", high = "Purple4", midpoint = (max(si_df$si)/2)) +
  
  geom_vline(xintercept = as.Date("2020-06-29"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2020-11-30"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-02-01"), colour = "black", linetype = 2, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2021-07-05"), colour = "black", linetype = 2, linewidth = 0.5) + 
  geom_vline(xintercept = as.Date("2021-10-04"), colour = "black", linetype = 2, linewidth = 0.5) +
  annotate("text", x = as.Date("2020-04-20"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 1",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2020-09-15"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 2a", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-01-01"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 2b", colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-04-18"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 3",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-08-18"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 4",  colour = "black", fontface = "bold") +
  annotate("text", x = as.Date("2021-11-20"), y = max(plot$case_obs_ra_f, na.rm = TRUE) + 0.03*max(plot$case_obs_ra_f, na.rm = TRUE), label = "Period 5",  colour = "black", fontface = "bold") 


# ----- 12. Save plotted object! -----
# Set Working directory
setwd("C:/Users/Z710214/OneDrive - Radboudumc/z710214/My Documents/PhD/S5 - EQUalS/Writing (Carsten)/")

ggname <- "SupFig1a_Epicurve_infs.jpeg"
ggname <- "SupFig1b_Epicurve_hosp.jpeg"

# Save ggplot2 object
ggsave(ggname, width = 15, height = 6.5, units = "in", dpi = 600, device = "eps")
ggsave(ggname, width = 15, height = 6.5, units = "in", dpi = 600)






  