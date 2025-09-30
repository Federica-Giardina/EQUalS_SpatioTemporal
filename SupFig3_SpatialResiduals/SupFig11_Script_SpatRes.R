# IMPORT LIBRARIES NOT YET IMPORTED
libs <- c("tmap")
lapply(libs, library, character.only = TRUE)

# Set Working Directory
setwd("H:\\Carsten - First glance at data")

# Data import and preparation
source("CrucialScripts_DONOTMOVE/DataPrep.R")
source("CrucialScripts_DONOTMOVE/Mob_Thresholds.R")

# Import models
InfsAgg3 <- readRDS("RESULTS JULY 2025/00_ModRes/res_InfsAgg3.rds")
InfsDag3 <- readRDS("RESULTS JULY 2025/00_ModRes/res_InfsDag3.rds")
HospAgg3 <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg3.rds")
HospDag3 <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospDag3.rds")
HospAgg1 <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg1.rds")
HospAgg5 <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg5_post.rds")

# Run script separately for figures
# Specify values
data               <- Cases.nb.wk %>% filter(week_begin > as.Date("2020-05-25")) # Remove filter for hospitalisations
data$y             <- data$case_obs # case_obs OR hosp_obs
type               <- "InfsDag3"
result             <- InfsDag3
t                  <- length(unique(as.integer(factor(data$week_id))))
i                  <- 3228
r                  <- 1:t

# Create MESH/SPDE objects
source("CrucialScripts_DONOTMOVE/st_mesh-spde-stk_objects_aggmod.R")
stk_obj            <- stk.dag # if dag model, stk.dag!





# ----- 1. Prepare mapping data -----
## Load & Clean mapping data
map <- read_sf("K:/Utilities/Tools/GISHulpbestanden/Gemeentewijkbuurt/2021/wk_2021.shp") %>% 
  mutate(wc2021 = substring(STATCODE, 3)) %>%
  select(wc2021, geometry)

## Remove areas that are filtered out in main dataset (n = 20)
remove_ids <- c("003406", "024316", "030732", "038810", "040536", 
                "051801", "059711", "059922", "059924", "059925",
                "059926", "061307", "068753", "071808", "074309", 
                "077722", "078512", "078522", "193016", "197908")

## Filter out areas and assign correct id's for modelling
map_filtered <- map %>%
  filter(!(wc2021 %in% remove_ids)) %>% 
  group_by(wc2021) %>%
  mutate(nbh_id = cur_group_id()) %>%
  ungroup()

## Filter unselected areas and assign NA for nbh_id 
map_unused <- map %>%
  filter(wc2021 %in% remove_ids) %>%
  mutate(nbh_id = NA)

## Bind map_filtered and map_unused
map_new <- rbind(map_filtered, map_unused)

## Select relevant columns
data.f <- data %>% select(wc2021, nbh_id, week_id, week_nbh_id, y, tot_pop) 





# 2. ----- Extract fitted and observed values and calculate residuals -----
### Problem:  account for different spatial structure due to Mesh.
### Solution: use inla.stack.index on the original stack object to match neighbourhoods to Mesh.
index_obs           <- inla.stack.index(stk_obj, tag = "est")$data
fitted_obs          <- result$summary.fitted.values[index_obs, "mean"]

data.f$fitted_prob  <- fitted_obs
data.f$fitted_count <- data.f$fitted_prob * data.f$tot_pop
data.f$residual     <- data.f$y - data.f$fitted_count

data.f <- data.f %>%
  mutate(period = case_when(
    week_id > 0  & week_id <= 26 ~ "Period 1 (1 Jan 2020 - 29 Jun 2020); Wildtype",
    week_id > 26 & week_id <= 47 ~ "Period 2a (29 Jun 2020 - 30 Nov 2020); Wildtype",
    week_id > 47 & week_id <= 56 ~ "Period 2b (30 Nov 2020 - 1 Feb 2021); Wildtype", 
    week_id > 56 & week_id <= 78 ~ "Period 3 (1 Feb 2021 - 5 Jul 2021); Alpha",
    week_id > 78 & week_id <= 91 ~ "Period 4 (5 Jul 2021 - 4 Oct 2021); Delta", 
    week_id > 91 & week_id <= 104 ~ "Period 5 (4 Oct 2021 - 31 Dec 2021); Delta"))


# Aggregate residuals over neighbourhoods over time
residual.week <- data.f %>%
  group_by(nbh_id) %>%
  summarise(residual.mean = mean(residual, na.rm = TRUE))

# Join residuals to map_new geometry
map.residuals <- map_new %>%
  left_join(residual.week, by = c("nbh_id"))




# ----- 3. Specify colour palettes -----
#pal <- brewer.pal(9, "YlOrBr")
#pal <- rev(brewer.pal(10, "BrBG"))
pal.cust.1 <- c("#08306b", "#2171b5", "#6baed6", "#c6dbef", "#f7fbff", "#fff5eb",
                "#fdcc8a", "#fc8d59", "#e34a33", "#b30000", "#7f0000")
pal.cust.2 <- c("#08306b", "#2171b5", "#6baed6", "#c6dbef", "#f7fbff",
                "#fff5eb", "#fdcc8a", "#fc8d59", "#e34a33", "#7f0000")


# ----- 4. Plot standard residuals over time -----
norres <- tm_shape(map.residuals) +
  tm_borders(alpha = 0.25) +
  tm_fill(col    = "residual.mean",
          style  = "fixed",
          midpoint = 0,
          n = 10,
          breaks = seq(-1.25, 1.25, by = 0.25),
          showNA = FALSE,
          palette = pal.cust.1,
          title = ""
          ) + 
  tm_layout(
    title                 = "Residual means averaged over time",
    title.fontface        = "bold",
    title.size            = .7,
    
    #main.title            = "a          -         Main Infections model",
    #main.title            = "b          -         Main Hospitalisations model",
    #main.title            = "c          -         Hospitalisations (0.1% mob. threshold)",
    #main.title            = "d          -         Hospitalisations (0.5% mob. threshold)",
    main.title            = "e          -         Disaggregated Infections model",
    #main.title            = "f          -         Disaggregated Hospitalisations model",
    
    main.title.position   = "left", 
    main.title.fontface   = "bold",
    main.title.size       = 1.25,
    
    legend.title.fontface = "bold",
    legend.title.size     = .7,
    legend.frame          = FALSE,
    legend.position       = c("left", "top"),
    legend.text.size      = .55,
    
    frame = FALSE)
    
    
# ----- 5. Calculate deviance residuals for a more robust interpretation ----- 
# Define safe Log function to avoid Log(0)
safe_log <- function(x) ifelse (x == 0, 0, log(x))

# Compute deviance residuals
data.f$dev_residual <- with(data.f, {
  y        <- y
  n        <- tot_pop
  y_hat    <- fitted_count
  
  # Clamp predictions to avoid Log(0) or devision by 0
  y_hat    <- pmin(pmax(y_hat, 1e-6), n - 1e-6)
  
  # Compute deviance components
  term1    <- y       * safe_log(y / y_hat)
  term2    <- (n - y) * safe_log((n - y) / (n - y_hat))
  deviance <- 2       * (term1 + term2)
  
  # Final deviance residual
  sign(y - y_hat) * sqrt(deviance)
  
})

# Aggregate residuals by region over time
residual.dev.week <- data.f %>%
  group_by(nbh_id) %>%
  summarise(mean_dev_residual = mean(dev_residual, na.rm = TRUE))

# Join dev.residuals to spatial geometry
map.dev.residuals <- map_new %>%
  left_join(residual.dev.week, by = c("nbh_id"))

# ----- 6. Plot residuals averaged over time -----
devres <-
  tm_shape(map.dev.residuals) +
  tm_borders(alpha = 0.25) +
  tm_fill(col    = "mean_dev_residual",
          style  = "fixed",
          midpoint = 0,
          n = length(pal.cust.1),
          breaks = seq(-1.5, 1.5, by = 0.3),
          showNA = FALSE,
          palette = pal.cust.2,
          title = ""
  ) + 
  tm_layout(
    title                 = "Deviance residuals averaged over time",
    title.fontface        = "bold",
    title.size            = .7,
     
    main.title            = " ",
    main.title.position   = "left", 
    main.title.fontface   = "bold",
    main.title.size       = 1.25,
    
    legend.title.fontface = "bold",
    legend.title.size     = .7,
    legend.frame          = FALSE,
    legend.position       = c("left", "top"),
    legend.text.size      = .55,
    
    frame = FALSE)
  




# ----- 7. Combine maps and save -----  

#norres
#devres
tm <- tmap_arrange(norres, devres, nrow = 1)
tm

# Define filename
#filename = "SupFig11a_SpatRes_InfsAgg3.jpeg"
#filename = "SupFig11b_SpatRes_HospAgg3.jpeg"
#filename = "SupFig11c_SpatRes_InfsDag3.jpeg"
#filename = "SupFig11d_SpatRes_HospDag3.jpeg"
#filename = "SupFig11e_SpatRes_HospAgg1.jpeg"
#filename = "SupFig11f_SpatRes_HospAgg5.jpeg"

# Save plots
tmap_save(
  tm,
  filename = filename, 
  width    = 15,
  height   = 6.5,
  units    = "in",
  dpi      = 600)
