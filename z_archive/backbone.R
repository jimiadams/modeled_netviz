library(igraph)
library(backbone)

################################################################

# This is a trimmed (down to the elements I'm going to use in my viz) version of Jenn Lena's original dataset (dissertation, 2004 Poetics). First getting it in order.

################################################################
samples <- readRDS("JL_samples.rds")
edges_df <- samples[, c("src_id", "tar_id", "yr")]
g <- graph_from_data_frame(edges_df, directed = F)

## converting to 2-mode
V(g)$type <- V(g)$name %in% samples$tar_id        # setting the SAMPLING artists as the deprecated mode

## Adding source genre
V(g)$genre <- samples$src_gen[match(V(g)$name, samples$src_id)]
## Adding artist & song names as node attributes
V(g)$artist <- samples$src_art[match(V(g)$name, samples$src_id)]
V(g)$song <- samples$src_sng[match(V(g)$name, samples$src_id)]
V(g)$artist <- samples$tar_art[match(V(g)$name, samples$tar_id)]
V(g)$song <- samples$tar_sng[match(V(g)$name, samples$tar_id)]

################################################################

# Ok, now we have a artist-by-artist version of the samples graph, but importantantly it's effectively bi-partite (while the same artist may show up in both the sampled and sampling IDs were generated separately, so they're not "matched" across).

################################################################

library(backbone)
library(igraph)

## This is a check on the weighting conversion (it worked, so commenting out):
#im <- as_incidence_matrix(g)
#table(im)

E(g)$weight <- 1 #needs a base
## Collpsing multi-edges to an edge weight:
gw <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE, 
                       edge.attr.comb = list(weight = "sum", "ignore"))
mean_weight <- mean(igraph::E(g)$weight)  #Find average edge weight


## apparently, backbone can't handle a weighted bipartite network directly. So, 
mw <- as.matrix(as_biadjacency_matrix(gw, attr = "weight"))
prj <- mw %*% t(mw)
bb <- backbone_from_weighted(projected_matrix, model = "disparity", alpha = 0.05,  parameter = mean_weight)

################################################################

# We've got a backbone among the sampled artists.

################################################################
bbg <- graph_from_adjacency_matrix(bb, mode="undirected")
comp <- components(bbg)
bbgc <- subgraph(bbg, V(bbg)[comp$membership == which.max(comp$csize)])
