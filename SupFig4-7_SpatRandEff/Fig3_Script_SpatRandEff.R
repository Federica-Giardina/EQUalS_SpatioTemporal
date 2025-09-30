# IMPORT LIBRARIES NOT YET IMPORTED
libs <- c("tmap")
lapply(libs, library, character.only = TRUE)

# Set Working Directory
setwd("H:\\Carsten - First glance at data")

# Data import and prepartion
source("CrucialScripts_DONOTMOVE/DataPrep.R")
source("CrucialScripts_DONOTMOVE/Mob_Thresholds.R")

# Models
InfsAgg3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_InfsAgg3.rds")
InfsDag3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_InfsDag3.rds")

HospAgg3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg3.rds")
HospDag3     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospDag3.rds")

HospAgg1     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg1.rds")
HospAgg5     <- readRDS("RESULTS JULY 2025/00_ModRes/res_HospAgg5_post.rds")

# ----- 1. First convert mesh to neighbourhood format -----
# The mesh is identical for all models, so it doesn't matter which model is used.
data               <- Cases.nb.wk #%>% filter(week_begin > as.Date("2020-05-25")) # REMOVE FILTER IF HOSP!
data$y             <- data$hosp_obs # case_obs OR hosp_obs
name               <- "HospAgg3"
result             <- HospAgg3
week_begin         <- as.Date("2020-01-06") # Hosps: as.Date("2020-01-06") OR Infs: as.Date("2020-06-01")
t                  <- length(unique(as.integer(factor(data$week_id))))
m                  <- 1302
i_nbh              <- 3228
i_mun              <- 354
r                  <- 1:t

source("CrucialScripts_DONOTMOVE/st_mesh-spde-stk_objects_aggmod.R")

# ----- 2. Specify models used -----
#name1              <- "InfsAgg3"
#name2              <- "HospAgg3"
#result1            <- InfsAgg3
#result2            <- HospAgg3 

#name1              <- "HospAgg1"
#name2              <- "HospAgg5"
#result1            <- HospAgg1
#result2            <- HospAgg5 

name1              <- "InfsDag3"
name2              <- "HospDag3"
result1            <- InfsDag3
result2            <- HospDag3 




# ----- 3. Import maps for plotting -----
map.nbh <- read_sf("K:/Utilities/Tools/GISHulpbestanden/Gemeentewijkbuurt/2021/wk_2021.shp") # On "neighbourhood resolution.
map.mun <- read_sf("K:/Utilities/Tools/GISHulpbestanden/Gemeentewijkbuurt/2021/gm_2021.shp") # On "neighbourhood resolution.

# Clean mapping data
map.nbh <- map.nbh %>% 
  mutate(wc2021 = substring(STATCODE, 3)) %>%
  select(wc2021, geometry)

# Remove areas that are filtered out in main dataset (n = 20)
remove_ids <- c("003406", "024316", "030732", "038810", "040536", 
                "051801", "059711", "059922", "059924", "059925",
                "059926", "061307", "068753", "071808", "074309", 
                "077722", "078512", "078522", "193016", "197908")

# Filter out areas and assign correct id's for modelling
map.nbh.f <- map.nbh %>%
  filter(!(wc2021 %in% remove_ids)) %>% 
  group_by(wc2021) %>%
  mutate(nbh_id = cur_group_id()) %>%
  ungroup()

# Filter unselected areas and assign NA for nbh_id 
map.excl <- map.nbh %>%
  filter(wc2021 %in% remove_ids) %>%
  mutate(nbh_id = NA)

# Bind map_filtered and map_unused
map.nbh.n <- rbind(map.nbh.f, map.excl)

# Assign group id's to municipalities too
map.mun.n <- map.mun %>%
  group_by(GM2021) %>%
  mutate(mun_id = cur_group_id()) %>%
  ungroup() %>%
  select(mun_id, geometry)

# Double check number of rows in result prior to continuing
print(nrow(result1$summary.random$nbh_id)/2) # Should be 3228
print(nrow(result1$summary.random$mun_id)/2) # Should be 354
print(nrow(result2$summary.random$nbh_id)/2) # Should be 3228
print(nrow(result2$summary.random$mun_id)/2) # Should be 354


# ----- 4. Extract spatial effects from models -----
# Neighbourhoods
SpatRandEff.nbh.infs <- result1$summary.random$nbh_id[1:i_nbh, c("ID", "mean")]
SpatRandEff.nbh.hosp <- result2$summary.random$nbh_id[1:i_nbh, c("ID", "mean")]
# Municipalities
SpatRandEff.mun.infs <- result1$summary.random$mun_id[1:i_mun, c("ID", "mean")]
SpatRandEff.mun.hosp <- result2$summary.random$mun_id[1:i_mun, c("ID", "mean")]


# ----- 5. Join spatial effects to geometry -----
# Neighbourhoods
map.nbh.n.infs <- map.nbh.n %>% left_join(SpatRandEff.nbh.infs, by = c("nbh_id" = "ID" ))
map.nbh.n.hosp <- map.nbh.n %>% left_join(SpatRandEff.nbh.hosp, by = c("nbh_id" = "ID" ))
# Municipalities
map.mun.n.infs <- map.mun.n %>% left_join(SpatRandEff.mun.infs, by = c("mun_id" = "ID"))
map.mun.n.hosp <- map.mun.n %>% left_join(SpatRandEff.mun.hosp, by = c("mun_id" = "ID"))

# Perform sanity check: missing values...
sum(is.na(map.nbh.n.infs$mean)) # Should be 20 (areas removed)
sum(is.na(map.mun.n.infs$mean)) # Should be 0
sum(is.na(map.nbh.n.hosp$mean)) # Should be 20 (areas removed)
sum(is.na(map.mun.n.hosp$mean)) # Should be 0


# ----- 6. Combine neighbourhood and municpal values -----
# Define key variable for joining
df.ref <- data %>% 
  filter(week_id == 83) %>%
  select(nbh_id, mun_id)

# Join reference id's from municipalities not neighbourhoods and municipal effects and add means.
# Infections
SpatRandEff.com.infs <- SpatRandEff.nbh.infs %>%
  left_join(df.ref, by = c("ID" = "nbh_id")) %>%
  left_join(SpatRandEff.mun.infs, by = c("mun_id" = "ID")) %>%
  mutate(mean = mean.x + mean.y) %>% 
  select(ID, mean)
# Hospitalisations
SpatRandEff.com.hosp <- SpatRandEff.nbh.hosp %>%
  left_join(df.ref, by = c("ID" = "nbh_id")) %>%
  left_join(SpatRandEff.mun.hosp, by = c("mun_id" = "ID")) %>%
  mutate(mean = mean.x + mean.y) %>% 
  select(ID, mean)

# Create map geometry for combined plot and add mean
# Infections
map.com.n.infs <- rbind(map.nbh.f, map.excl) %>% left_join(SpatRandEff.com.infs, by = c("nbh_id" = "ID"))
# Hospitalisations
map.com.n.hosp <- rbind(map.nbh.f, map.excl) %>% left_join(SpatRandEff.com.hosp, by = c("nbh_id" = "ID"))


# ----- 7. Prepare data for plotting -----
# Create a pretty scale/legend based on the minimum and maximum values
global_min.infs <- min(c(map.nbh.n.infs$mean, map.mun.n.infs$mean, map.com.n.infs$mean), na.rm = TRUE)
global_max.infs <- max(c(map.nbh.n.infs$mean, map.mun.n.infs$mean, map.com.n.infs$mean), na.rm = TRUE)
global_min.hosp <- min(c(map.nbh.n.hosp$mean, map.mun.n.hosp$mean, map.com.n.hosp$mean), na.rm = TRUE)
global_max.hosp <- max(c(map.nbh.n.hosp$mean, map.mun.n.hosp$mean, map.com.n.hosp$mean), na.rm = TRUE)

# Set limits
limits_pretty.infs <- pretty(c(global_min.infs, global_max.infs), n = 8)
limits_pretty.hosp <- pretty(c(global_min.hosp, global_max.hosp), n = 8)


# ----- . Specify colour palette consistent with other Figures -----
pal <- brewer.pal(9, "YlOrBr")






# ----- 8. Plot maps -----
# Neighbourhood-level spatial random effects
# Infections
plot.SpatRandEff.nbh.infs <- 
  tm_shape(map.nbh.n.infs) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "mean", 
          palette = pal,
          midpoint = 0,
          style = "fixed",
          title = "", showNA = FALSE,
          breaks = limits_pretty.infs) +
  
  tm_layout(title                 = "Neighbourhood-level random effects",
            title.fontface        = "bold",
            title.size            = 1.10,
           
            main.title            = "Disaggregated infections model",
            #main.title            = "Hospitalisations model (0.1% mob. threshold)",
            #main.title            = "a          -          Infections model",
            main.title.position   = "left", 
            main.title.fontface   = "bold",
            main.title.size       = 1.25,
            
            legend.title.fontface = "bold",
            legend.title.size     = .75,
            legend.frame          = FALSE,
            legend.position       = c("left", "top"),
            legend.text.size      = 1.00,

            frame = TRUE)




# Hospitalisations
plot.SpatRandEff.nbh.hosp <- 
  tm_shape(map.nbh.n.hosp) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "mean", palette = pal, midpoint = 0,
          style = "fixed", title = "", showNA = FALSE,
          breaks = limits_pretty.hosp) +
  tm_layout(title                 = "Neighbourhood-level random effects",
            title.fontface        = "bold",
            title.size            = 1.10,
            
            main.title            = "Disaggregated Hospitalisations model",
            #main.title            = "Hospitalisations model (0.5% mob. threshold)",
            #main.title            = "b          -          Hospitalisations model model",
            main.title.position   = "left", 
            main.title.fontface   = "bold",
            main.title.size       = 1.25,
            
            legend.title.fontface = "bold",
            legend.title.size     = .75,
            legend.frame          = FALSE,
            legend.position       = c("left", "top"),
            legend.text.size      = 1.00,
            
            frame = TRUE)



# Municipal-level spatial random effects
# Infections
plot.SpatRandEff.mun.infs <- 
  tm_shape(map.mun.n.infs) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "mean", palette = pal, midpoint = 0,
          style = "fixed", title = "", showNA = FALSE,
          breaks = limits_pretty.infs) +
  tm_layout(title            = "Municipal-level random effects",
            title.fontface   = "bold",
            title.size       = 1.10,

            main.title            = " ",
            main.title.position   = "left", 
            main.title.fontface   = "bold",
            main.title.size       = 1.25,
            
            legend.title.fontface = "bold",
            legend.title.size     = .75,
            legend.frame          = FALSE,
            legend.position       = c("left", "top"),
            
            legend.show = FALSE,
            
            frame = TRUE)

# Hospitalisations
plot.SpatRandEff.mun.hosp <- 
  tm_shape(map.mun.n.hosp) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "mean", palette = pal, midpoint = 0,
          style = "fixed", title = "Value", showNA = FALSE,
          breaks = limits_pretty.hosp) +
  tm_layout(title            = "Municipal-level random effects",
            title.fontface   = "bold",
            title.size       = 1.10,
            
            main.title            = " ",
            main.title.position   = "left", 
            main.title.fontface   = "bold",
            main.title.size       = 1.25,
            
            legend.title.fontface = "bold",
            legend.title.size     = .75,
            legend.frame          = FALSE,
            legend.position       = c("left", "top"),
            
            legend.show = FALSE,
            
            frame = TRUE)



# Combined spatial random effects
# Infections
plot.SpatRandEff.com.infs <- 
  tm_shape(map.com.n.infs) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "mean", palette = pal, midpoint = 0,
          style = "fixed", title = "Value", showNA = FALSE,
          breaks = limits_pretty.infs) +
  tm_layout(title            = "Combined spatial random effects",
            title.fontface   = "bold",
            title.size       = 1.10,
            
            main.title            = " ",
            main.title.position   = "left", 
            main.title.fontface   = "bold",
            main.title.size       = 1.25,
            
            legend.title.fontface = "bold",
            legend.title.size     = .75,
            legend.frame          = FALSE,
            legend.position       = c("left", "top"),
            
            legend.show = FALSE,
            
            frame = TRUE)

# Hospitalisations
plot.SpatRandEff.com.hosp <- 
  tm_shape(map.com.n.hosp) +
  tm_borders(alpha = 0.25) +
  tm_fill(col = "mean", palette = pal, midpoint = 0,
          style = "fixed", title = "Value", showNA = FALSE,
          breaks = limits_pretty.hosp) +
  tm_layout(title            = "Combined spatial random effects",
            title.fontface   = "bold",
            title.size       = 1.10,
            
            main.title            = " ",
            main.title.position   = "left", 
            main.title.fontface   = "bold",
            main.title.size       = 1.25,
            
            legend.title.fontface = "bold",
            legend.title.size     = .75,
            legend.frame          = FALSE,
            legend.position       = c("left", "top"),
            
            legend.show = FALSE,
            
            frame = TRUE)


# ----- 9. Combine plots -----
# Infections (Panel a)
tmap.SpatRanEff.infs <- 
  tmap_arrange(
    plot.SpatRandEff.nbh.infs,    
    plot.SpatRandEff.mun.infs,
    plot.SpatRandEff.com.infs,
    nrow = 1)
# Hospitalisations (Panel b)
tmap.SpatRanEff.hosp <- 
  tmap_arrange(
    plot.SpatRandEff.nbh.hosp,    
    plot.SpatRandEff.mun.hosp,
    plot.SpatRandEff.com.hosp,
    nrow = 1)


# ----- 10. Save resulting plots -----

#tmap_name1 <- "20250922_Export/Fig5_SpatRandEff/Fig5a_SpatRandEff_infs.jpeg"
#tmap_name2 <- "20250922_Export/Fig5_SpatRandEff/Fig5b_SpatRandEff_hosp.jpeg"
tmap_name1 <- "20250922_Export/SupFig2-5_SpatRandEff/SupFig3_SpatRandEff_hosp1.jpeg"
tmap_name2 <- "20250922_Export/SupFig2-5_SpatRandEff/SupFig4_SpatRandEff_hosp5.jpeg"
#tmap_name1 <- "20250922_Export/SupFig2-5_SpatRandEff/SupFig5_SpatRandEff_DagInfs.jpeg"
#tmap_name2 <- "20250922_Export/SupFig2-5_SpatRandEff/SupFig6_SpatRandEff_DagHosp.jpeg"

tmap_save(
  tmap.SpatRanEff.infs,
  filename = tmap_name1, 
  width    = 15,
  height   = 6.5,
  units    = "in",
  dpi      = 600)

tmap_save(
  tmap.SpatRanEff.hosp,
  filename = tmap_name2, 
  width    = 15,
  height   = 6.5,
  units    = "in",
  dpi      = 600)










