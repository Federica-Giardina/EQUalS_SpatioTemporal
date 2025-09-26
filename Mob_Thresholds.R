# MOBILITY DATA

# Data preparation
## Import mobility data
mobdat.mun <- read_csv("H:/Carsten - First glance at data/CrucialScripts_DONOTMOVE/20241107 mobdat_mun_incl.csv")

## Transform "0" (abroad) to "9998" so it does not interfere with grouping
## Create municipality id based on current grouping (ascending). This is essential for modelling.
mobdat <- mobdat.mun %>%
  mutate(VertGem = ifelse(VertGem == 0, 9998, VertGem),
         AankGem = ifelse(AankGem == 0, 9998, AankGem)) %>%
  arrange(VertGem) %>% group_by(VertGem) %>% mutate(VertGem_ID = cur_group_id()) %>%
  arrange(AankGem) %>% group_by(AankGem) %>% mutate(AankGem_ID = cur_group_id())  %>%
  select(VertGem, AankGem, VertGem_ID, AankGem_ID, prop.all.ar) %>%
  ungroup()

## Select limited number of variables to keep comprehensible for now.
#Cases.nb.wk <- Cases.nb.wk

## Link .nb in data to the .mun they are in. This is essential when including .mob data for analysis.
### Import reference file
ref <- read_csv("H:/Carsten - First glance at data/CrucialScripts_DONOTMOVE/wc_to_gem.csv")
ref <- ref %>%
  mutate(mun = as.numeric(gem2021)) %>%
  select(wc2021, mun)
### Create id for .mun

#mun_levels <- as.character(1:352)

ref <- ref %>%
  group_by(mun) %>%
  mutate(mun_id = cur_group_id())


### Join .mun to .nb to case data using ref
Cases.nb.wk <- Cases.nb.wk %>%
  left_join(ref, by = c("wc2021"))



# CREATE MOBILITY MATRIX
## Prepare matrix data
mobdat.mun.matrix <- mobdat %>%
  select(AankGem_ID, VertGem_ID, prop.all.ar) %>%
  #filter(AankGem_ID != 354,
  #       VertGem_ID != 354,
  #       AankGem_ID != 353,
  #       VertGem_ID != 353) %>%
  arrange(AankGem_ID, VertGem_ID) %>%
  pivot_wider(names_from = AankGem_ID, values_from = prop.all.ar)

## Convert to matrix without first column (gem_id)
m <- as.matrix(mobdat.mun.matrix[,2:355], nrow = 354, ncol = 354)
## Create symmetric matrix
t <- m + t(m)

### Convert .mob proportions to cut-off values
t.co    <- ifelse(t < 0.003, 0, 1)
t.co.01 <- ifelse(t < 0.001, 0, 1)
t.co.05 <- ifelse(t < 0.005, 0, 1)


# Convert matrix to neighbours list
#mobility_nb_03 <- mat2listw(t.co, style = "B")$neighbours
#mobility_nb_01 <- mat2listw(t.co.01, style = "B")$neighbours
#mobility_nb_05 <- mat2listw(t.co.05, style = "B")$neighbours
# Save as .graph file for INLA
#nb2INLA("mobility_graph_03.graph", mobility_nb_03)
#nb2INLA("mobility_graph_01.graph", mobility_nb_01)
#nb2INLA("mobility_graph_05.graph", mobility_nb_05)

# read back in as INLA graph object
#mobility_graph_03 <- inla.read.graph("mobility_graph_03.graph")
#mobility_graph_01 <- inla.read.graph("mobility_graph_01.graph")
#mobility_graph_05 <- inla.read.graph("mobility_graph_05.graph")





