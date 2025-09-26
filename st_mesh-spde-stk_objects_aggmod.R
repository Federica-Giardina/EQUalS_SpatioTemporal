setwd("H:\\Carsten - First glance at data")

# Scripts to be called in call file:
#source("CrucialScripts_DONOTMOVE/DataPrep.R")
#source("CrucialScripts_DONOTMOVE/Mob_Thresholds.R")

# Variables to be defined in call file:
#data   <- Cases.nb.wk #%>% filter(week_begin > as.Date("2020-05-25")) # REMOVE FILTER IF HOSP!
#data$y <- data$case_obs                                               # case_obs OR hosp_obs

# Import mapping data (.shp, clean start)
map <- read_sf("K:/Utilities/Tools/GISHulpbestanden/Gemeentewijkbuurt/2021/wk_2021.shp") 

# Clean mapping data
map <- map %>% 
  mutate(wc2021 = substring(STATCODE, 3)) %>%
  select(wc2021, geometry)

# Specify areas that are filtered out in main dataset (n = 20)
remove_ids <- c("003406", "024316", "030732", "038810", "040536", 
                "051801", "059711", "059922", "059924", "059925",
                "059926", "061307", "068753", "071808", "074309", 
                "077722", "078512", "078522", "193016", "197908")

# Remove areas from the map geometry
map_filtered <- map %>% filter(!(wc2021 %in% remove_ids))



# I. Define parameters for mesh
centroids     <- st_coordinates(st_centroid(map_filtered))
boundary      <- st_union(map_filtered)

dmat          <- as.numeric(st_distance(st_centroid(map_filtered)))
dmat          <- dmat[dmat > 0]
median_dist   <- median(dmat)
 
cutoff        <-   median_dist / 20
max.edge      =  c(median_dist / 20, median_dist)
offset        <- c(median_dist * 0.01, median_dist)

data$week_idx <- as.integer(factor(data$week_id))
data$nbh_id   <- as.integer(factor(data$nbh_id))

t             <- length(unique(data$week_id))
i             <- nrow(centroids)


# II. Construct mesh
mesh_s <- inla.mesh.2d(loc      = centroids, 
                       boundary = boundary, 
                       max.edge = max.edge, 
                       cutoff   = cutoff, 
                       offset   = offset)

# Sanity check: plot mesh
plot(mesh_s)
plot(st_geometry(map_filtered), add = TRUE)



# III. Create a SPDE model object using this mesh
spde_st <- inla.spde2.pcmatern(mesh        = mesh_s, 
                               alpha       = 2, 
                               prior.range = c(50, 0.5),
                               prior.sigma = c(1,  0.01))


# IV. Create index for projection of spde mesh on original spatial definition
st_index <- inla.spde.make.index(name = "time_space_id_new", 
                                 n.spde= spde_st$n.spde, 
                                 n.group = length(unique(data$week_idx)))

# Giving:  
# 1) "time_space_id_new"       holding mesh node IDs, repeated for each time indice (n = 83 or 104) and 
# 2) "time_space_id_new.group" holding the time point for each entry. 


# V. Create projection matrix "A" 

# The A-matrix consists of weighted values on how to combine values from nearby mesh-nodes to spatially interpolate nbh.
# Resulting in: i*t = 3228*104 = 335712 rows, and n_nodes*t = 1302*104 = 135408 columns.

coords_st <- centroids[match(data$wc2021, st_centroid(map_filtered)$wc2021), ]

A_st<- inla.spde.make.A(mesh    = mesh_s, 
                        loc     = coords_st, 
                        group   = data$week_idx, 
                        n.group = length(unique(data$week_idx)))


# VI. Create stack object
stk <- inla.stack(data    = list(y = data$y, ntrials = data$tot_pop),
                  A       = list(1, A_st),
                  effects = list(data.frame(SES_HH_Q1_prop_stand = data$SES_HH_Q1_prop_stand,     
                                            MBG_M_prop_stand     = data$MBG_M_prop_stand,           
                                            ste_mvs              = data$ste_mvs,                   
                                            Age_65.00_prop_stand = data$Age_65.00_prop_stand, 
                                            nbh_id               = data$nbh_id,
                                            mun_id               = data$mun_id,              
                                            week_idx             = data$week_idx,
                                            week_SES             = data$week_SES,            
                                            week_MBG             = data$week_MBG, 
                                            week_age             = data$week_age), 
                                 st_index), 
                  tag     = "est")



# VII. Extract df from stack object
stack_df <- inla.stack.data(stk)
















stk.dag <- inla.stack(data = list(y       = data$y,
                                  ntrials = data$tot_pop),
                      A = list(1, A_st),
                      effects = list(data.frame(SES_HH_Q1_prop_stand = data$SES_HH_Q1_prop_stand, 
                                                MBG_group_Antilles_prop_stand = data$MBG_group_Antilles_prop_stand,
                                                MBG_group_Marocco_prop_stand  = data$MBG_group_Marocco_prop_stand,
                                                MBG_group_Surinam_prop_stand  = data$MBG_group_Surinam_prop_stand,
                                                MBG_group_Turkiye_prop_stand  = data$MBG_group_Turkiye_prop_stand,
                                                MBG_group_Other_W_prop_stand  = data$MBG_group_Other_W_prop_stand,
                                                MBG_group_Other_nW_prop_stand = data$MBG_group_Other_nW_prop_stand,
                                                
                                                ste_mvs              = data$ste_mvs,                   
                                                Age_65.00_prop_stand = data$Age_65.00_prop_stand, 
                                                nbh_id               = data$nbh_id,
                                                mun_id               = data$mun_id,              
                                                week_idx             = data$week_idx,
                                                week_SES             = data$week_SES,            
                                                
                                                week_MBG1            = data$week_MBG1,
                                                week_MBG2            = data$week_MBG2,
                                                week_MBG3            = data$week_MBG3,
                                                week_MBG4            = data$week_MBG4,
                                                week_MBG5            = data$week_MBG5,
                                                week_MBG6            = data$week_MBG6,
                                                
                                                week_age             = data$week_age), 
                                     st_index), tag = "est")
