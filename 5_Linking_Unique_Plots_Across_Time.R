

trees<-list.files("R:\\Adam Eichenwald\\FIA_TREES_CN_ID", full.name=TRUE)%>%
  lapply(fread)%>%
  rbindlist()
trees

library(dplyr)
library(igraph)
test<-treetable%>%
  select(INVYR, CN,PREV_TRE_CN,PLT_CN,STATUSCD,ACTUALHT,
         SPCD, DIA, SPGRPCD)%>%
  mutate(CN=as.character(CN))%>%
  inner_join(trees%>%
               mutate(CN = as.character(CN))%>%
               data.frame())
# Assuming your data frame is named 'df' and has the following columns:
# 'plot_ID' - the current plot identifier
# 'tree_ID' - the unique tree identifier

# Step 1: Create a pairwise combination of plot_IDs based on shared trees
plot_pairs <- test %>%
  filter(INVYR>2001)%>%
  select(PLT_CN, FULL_TREE_ID) %>%
  distinct() %>%
  inner_join(., ., by = "FULL_TREE_ID") %>%
  filter(PLT_CN.x != PLT_CN.y) %>%
  select(PLT_CN.x, PLT_CN.y)%>%
  drop_na(TREE_ID)
library(igraph)
# Step 2: Create a graph where each node is a plot_ID and edges are created if two plots share a tree
g <- graph_from_data_frame(plot_pairs, directed = FALSE)

# Step 3: Identify connected components (each component corresponds to a consistent plot across surveys)
components <- components(g)

# Step 4: Map each plot_ID to its component ID
component_mapping <- data.frame(PLT_CN = names(components$membership),
                                new_plot_ID = components$membership)

# Step 5: Join this new consistent plot ID back to the original data frame
test <- test %>%
  filter(INVYR>=2001)%>%
  mutate(PLT_CN = as.character(PLT_CN))%>%
  left_join(component_mapping, by = "PLT_CN")
# 
# test<-test%>%
#   mutate(new_plot_ID = ifelse(is.na(new_plot_ID)==TRUE, PLT_CN,new_plot_ID))
fwrite(test, "treetable_with_treeID_plotID.csv")
# Now, 'new_plot_ID' will be consistent across surveys for the same plot

