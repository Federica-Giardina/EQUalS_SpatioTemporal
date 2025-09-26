
source("CrucialScripts_DONOTMOVE/st_mesh-spde-stk_objects_aggmod.R")




# VI. Extract all spatiotemporal random effects (for now just posterior means, later draw inla.posterior.sample())
st_rand.eff.model_nodes <- result$summary.random$time_space_id_new.repl$mean

# Each value is the estimated effect at a mesh node at a specific time point

# This gives a vector of length n_nodes * n_weeks, ordered by:
# [mesh_node_1_week_1, ..., mesh_node_n_week1, 
#  mesh_node_1_week_2, ..., mesh_node_n_week2,
#  ...
#  mesh_node_1_week_t, ..., mesh_node_n_week_t]


# VII. Reshape into a matrix - just to  check (Giving field_matrix[i, t] = value at mesh node i and time t)
matrix_st_rand.eff.model <- 
  matrix(
    data = st_rand.eff.model_nodes, 
    nrow = spde_st$n.spde, 
    ncol = length(unique(data$week_id)))

# VI. Project to neighbourhoods for all time points
st_rand.eff.nbh <- as.vector(A_st %*% st_rand.eff.model_nodes)

# VII. Create dataframe for all space-time combination and combine with rand.eff values
# It should be ordered as nbh 1 to 3228 for each time point.
df.rand_eff.st <- 
  data.frame(
    nbh_id      = rep(1:i, times = t),
    week_id     = rep(1:t, each  = i),
    st_rand_eff = st_rand.eff.nbh)

df.rand_eff.st$week_id <- as.integer(factor(df.rand_eff.st$week_id))


# VIII. Plot rand.eff for specific t
# Check whether rand.eff.nbh are relatively smooth over space

# Select a couple of time points for plotting
df_st_rand.eff.nbh_subset <- df.rand_eff.st %>% filter(week_id %in% c(1, 25, 75, 100, 104))

# Join values with map
map_filtered <- map_filtered %>%
  group_by(wc2021) %>%
  mutate(nbh_id = cur_group_id())

map.plot <- map_filtered %>%
  left_join(df_st_rand.eff.nbh_subset, by = c("nbh_id" = "nbh_id"))



# Plot data
ggplot(map.plot) +
  geom_sf(aes(fill = st_rand_eff), colour = NA) +
  facet_wrap(~ week_id) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Projected spatiotemporal random effect from mesh to neighbourhoods",
       fill  = "st rand.eff value") +
  theme_minimal()

# Another way of plotting
ggplot(data    = df_st_rand.eff.nbh_subset, 
       mapping = aes(x      = factor(nbh_id),
                     y      = st_rand_eff, 
                     group  = factor(week_id), 
                     colour = factor(week_id))) +
  scale_colour_brewer(palette = "Pastel1") +
  geom_line() +
  geom_point() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  theme_classic() 

# Better visualisation perhaps  
ggplot(data    = df_st_rand.eff.nbh_subset,
       mapping = aes(x      = factor(nbh_id),
                     y      = st_rand_eff,
                     colour = factor(week_id))) +
  geom_point(size = .1) +
  facet_wrap(~ factor(week_id)) +
  scale_color_brewer(palette = "Pastel1") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 



























































