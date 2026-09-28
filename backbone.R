
# Backbone
Let's pull off the **sampled artists'** backbone.
```{r backbone}

artist_incidence <- (t(mw) > 0) * 1

# model option 1 (less likely to use)
bb_sdsm_obj <- backbone::backbone_from_projection(
  B = artist_incidence,   
  model = "sdsm",     
  alpha = 0.05,               
  mtc = "bonferroni", 
  narrative = TRUE
)
bb_sdsm <- (bb_sdsm_obj == 1) * 1

# model 2 option (more restrictive)
bb_hyper_obj <- backbone::backbone_from_projection(
  B = artist_incidence,   # Native layout (Songs x Artists)
  model = "fixedrow",     # Hypergeometric distribution evaluates column overlaps
  alpha = 0.05,               
  mtc = "bonferroni",     
  narrative = TRUE
)
# Isolate strictly the positive significant artist-to-artist ties (14 edges)
bb_hyper <- (as.matrix(bb_hyper_obj) == 1) * 1

bb <- bb_hyper

```

# Generating a "hybrid" plot

``` {r plots2}
source("src/igraph-triangles.R")

# 1. Map the active backbone matrix straight into a standard unipartite graph
bb_graph <- graph_from_adjacency_matrix(bb, mode = "undirected")
bb_edges <- igraph::as_data_frame(bb_graph, what = "edges")

# 2. FILTER: Only keep edges where BOTH artists are verified inside the giant component
bb_edges_filtered <- bb_edges %>%
  filter(from %in% V(g_core)$name & to %in% V(g_core)$name)

# 3. Pull the specific artist IDs that are active in this selected backbone core
backbone_artists <- unique(c(bb_edges_filtered$from, bb_edges_filtered$to))


# 4. BAKE CONTRASTING STYLES DIRECTLY ONTO NODES AS NATIVE ATTRIBUTES
master_genre_colors <- genre_colors[V(g_core)$genre]

# Node Fills & Outlines:
# - Rap song triangles and non-backbone artist circles fade to a quiet 35% alpha.
# - Active backbone artists stay 100% solid, sharp, and opaque.
V(g_core)$color <- ifelse(
  V(g_core)$type,
  scales::alpha("white", 0.35),
  ifelse(V(g_core)$name %in% backbone_artists, master_genre_colors, scales::alpha(master_genre_colors, 0.35))
)

# Sync frames perfectly to fills to eliminate high-contrast background border noise
V(g_core)$frame.color <- V(g_core)$color


# 5. STYLE RAW BACKGROUND EDGES
E(g_core)$color <- "gray90"  # Solid soft gray avoids intensive alpha blending lag
E(g_core)$width <- 0.4


# 6. WELD FILTERED BACKBONE PATHS NATIVELY INTO THE GRAPH MASCHINE
bb_edge_vector <- as.vector(t(as.matrix(bb_edges_filtered[, c("from", "to")])))
g_hybrid       <- igraph::add_edges(g_core, bb_edge_vector)

# Target the newly appended backbone edges using the tail function
new_edges <- tail(E(g_hybrid), nrow(bb_edges_filtered))

# Bake the bold black emphasis properties straight onto those new edge slots
E(g_hybrid)$color[new_edges] <- "black"
E(g_hybrid)$width[new_edges] <- 2.5

plot(g_hybrid, 
     layout = layout_core, 
     vertex.size = V(g_hybrid)$vsize, 
     vertex.label = NA,
     vertex.shape = c("circle", "triangle")[V(g_hybrid)$type + 1], 
     vertex.color = V(g_hybrid)$color,
     vertex.frame.color = core_frame_colors, # <-- Added to clear out outline noise
     margin = 0.15)

# 3. Titles and clean PNG output export
title(main = "Bipartite Network with Projected Backbone Overlay", line = 2.5, cex.main = 1.4)
title(main = "hypergeomtric p<0.05, w/ Bonferroni correction", line = 1.0, cex.main = 1.0, font.main = 1)

dev.copy(png, "hybrid_backbone_network.png", width = 10, height = 10, units = "in", res = 300)
dev.off()

```

# Generating the *hybrid* plot

# Generating the *explanatory* plot
