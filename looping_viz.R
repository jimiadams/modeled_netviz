# ============================================================
# Compare independent vs. jointly coupled multilayer layouts
# ============================================================

library(graphlayouts)
library(patchwork)

# ------------------------------------------------------------
# 1. Build the combined multilayer graph
# ------------------------------------------------------------

# Use the empirical within-layer projections.
# The direct bipartite ties are added separately and downweighted.

between_weights <- c(0, 0.01, 0.025, 0.05, 0.10)

song_names   <- as.character(V(g_song)$name)
artist_names <- as.character(V(g_artist)$name)

W_artist <- mw %*% t(mw)

W_song <- t(mw) %*% mw

make_joint_graph <- function(between_weight) {
  
  joint_weight <- rbind(
    cbind(
      W_song,
      t(mw) * between_weight
    ),
    cbind(
      mw * between_weight,
      W_artist
    )
  )
  
  joint_names <- c(
    paste0("song__", song_names),
    paste0("artist__", artist_names)
  )
  
  dimnames(joint_weight) <- list(
    joint_names,
    joint_names
  )
  
  g <- igraph::graph_from_adjacency_matrix(
    joint_weight,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )
  
  V(g)$lvl <- c(
    rep(1, length(song_names)),
    rep(2, length(artist_names))
  )
  
  V(g)$mode <- c(
    rep("song", length(song_names)),
    rep("artist", length(artist_names))
  )
  
  V(g)$original_name <- c(
    song_names,
    artist_names
  )
  
  g
}


# ------------------------------------------------------------
# 2. Independent-layout baseline
# ------------------------------------------------------------

set.seed(123)

layout_independent_song <- layout_with_fr(
  g_song,
  weights = E(g_song)$weight
)

layout_independent_artist <- layout_with_fr(
  g_artist,
  weights = E(g_artist)$weight
)


# ------------------------------------------------------------
# 3. Joint layouts at different coupling strengths
# ------------------------------------------------------------

joint_graphs <- lapply(
  between_weights[-1],
  make_joint_graph
)

names(joint_graphs) <- paste0(
  "between_",
  between_weights[-1]
)

joint_layouts <- lapply(
  joint_graphs,
  function(g) {
    
    set.seed(123)
    
    graphlayouts::layout_as_multilevel(
      g,
      type = "all",
      alpha = 25,
      beta = 45
    )
  }
)

names(joint_layouts) <- names(joint_graphs)


# ------------------------------------------------------------
# 4. Convert every layout to the same node-coordinate format
# ------------------------------------------------------------

make_layout_df <- function(
    xy,
    song_names,
    artist_names,
    prefix = c("independent", "joint")
) {
  
  prefix <- match.arg(prefix)
  
  if (prefix == "joint") {
    
    tibble(
      name = c(song_names, artist_names),
      mode = c(
        rep("song", length(song_names)),
        rep("artist", length(artist_names))
      ),
      x = xy[, 1],
      y = xy[, 2]
    )
    
  } else {
    
    tibble(
      name = c(song_names, artist_names),
      mode = c(
        rep("song", length(song_names)),
        rep("artist", length(artist_names))
      ),
      x = c(
        layout_independent_song[, 1],
        layout_independent_artist[, 1]
      ),
      y = c(
        layout_independent_song[, 2],
        layout_independent_artist[, 2]
      )
    )
  }
}


layout_comparison <- list()

# independent baseline
layout_comparison$independent <- make_layout_df(
  NULL,
  song_names,
  artist_names,
  prefix = "independent"
)

# joint alternatives
for (nm in names(joint_layouts)) {
  
  layout_comparison[[nm]] <- make_layout_df(
    joint_layouts[[nm]],
    song_names,
    artist_names,
    prefix = "joint"
  )
}


# ------------------------------------------------------------
# 5. Quick diagnostic: compare the actual coordinate structures
# ------------------------------------------------------------

# This lets you inspect the layouts before we worry about
# final plane geometry.

layout_comparison

plot_multilayer_layout <- function(
    layout_df,
    title = NULL
) {
  
  # Split coordinates back into the two layers
  song_nodes_plot <- layout_df %>%
    filter(mode == "song")
  
  artist_nodes_plot <- layout_df %>%
    filter(mode == "artist")
  
  # observed inter-layer ties
  observed_xy <- observed_edges %>%
    left_join(
      song_nodes_plot %>%
        rename(
          x_from = x,
          y_from = y
        ),
      by = c("from" = "name")
    ) %>%
    left_join(
      artist_nodes_plot %>%
        rename(
          x_to = x,
          y_to = y
        ),
      by = c("to" = "name")
    )
  
  # projected song ties
  song_edges_plot <- song_edges %>%
    left_join(
      song_nodes_plot %>%
        rename(
          x_from = x,
          y_from = y
        ),
      by = c("from" = "name")
    ) %>%
    left_join(
      song_nodes_plot %>%
        rename(
          x_to = x,
          y_to = y
        ),
      by = c("to" = "name")
    )
  
  # projected artist ties
  artist_edges_plot <- artist_edges %>%
    left_join(
      artist_nodes_plot %>%
        rename(
          x_from = x,
          y_from = y
        ),
      by = c("from" = "name")
    ) %>%
    left_join(
      artist_nodes_plot %>%
        rename(
          x_to = x,
          y_to = y
        ),
      by = c("to" = "name")
    )
  
  # core backbone
  core_edges_plot <- core_edges %>%
    left_join(
      artist_nodes_plot %>%
        rename(
          x_from = x,
          y_from = y
        ),
      by = c("from" = "name")
    ) %>%
    left_join(
      artist_nodes_plot %>%
        rename(
          x_to = x,
          y_to = y
        ),
      by = c("to" = "name")
    )
  
  # NOTE:
  # For this diagnostic comparison, we do NOT yet force the
  # coordinates into arbitrary rectangular bounds. That is
  # precisely what we're trying to decide.
  
  ggplot() +
    
    # sampling layer plane
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 0,
      ymax = Inf,
      fill = "grey92",
      alpha = 0.7
    ) +
    
    # sampled-artists layer plane
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = 0,
      fill = "grey92",
      alpha = 0.7
    ) +
    
    # direct sampling ties
    geom_segment(
      data = observed_xy,
      aes(
        x = x_from,
        y = y_from,
        xend = x_to,
        yend = y_to
      ),
      color = "grey55",
      alpha = 0.035,
      linewidth = 0.25
    ) +
    
    # song projection
    geom_segment(
      data = song_edges_plot,
      aes(
        x = x_from,
        y = y_from,
        xend = x_to,
        yend = y_to
      ),
      color = "grey50",
      alpha = 0.20,
      linewidth = 0.35
    ) +
    
    # artist projection outside core
    geom_segment(
      data = artist_edges_plot %>%
        filter(!core_edge),
      aes(
        x = x_from,
        y = y_from,
        xend = x_to,
        yend = y_to
      ),
      color = "grey50",
      alpha = 0.05,
      linewidth = 0.20
    ) +
    
    # core relationships
    geom_segment(
      data = core_edges_plot,
      aes(
        x = x_from,
        y = y_from,
        xend = x_to,
        yend = y_to,
        linewidth = weight
      ),
      color = "grey20",
      alpha = 0.75
    ) +
    
    # sampling songs
    geom_point(
      data = song_nodes_plot,
      aes(
        x = x,
        y = y
      ),
      color = "grey55",
      shape = 17,
      size = 2.3,
      alpha = 0.85
    ) +
    
    # sampled artists
    geom_point(
      data = artist_nodes_plot,
      aes(
        x = x,
        y = y,
        size = size_scaled,
        color = genre_color
      ),
      shape = 16,
      alpha = 0.85
    ) +
    
    scale_size_continuous(
      range = c(2, 5),
      guide = "none"
    ) +
    
    scale_linewidth_continuous(
      range = c(0.3, 1.6),
      guide = "none"
    ) +
    
    coord_equal() +
    
    labs(title = title) +
    
    theme_void() +
    
    theme(
      plot.title = element_text(
        size = 13,
        hjust = 0
      )
    )
}

p_independent <- plot_multilayer_layout(
  layout_comparison$independent,
  "Independent FR"
)

p_001 <- plot_multilayer_layout(
  layout_comparison$between_0.01,
  "Joint: 0.01"
)

p_0025 <- plot_multilayer_layout(
  layout_comparison$between_0.025,
  "Joint: 0.025"
)

p_005 <- plot_multilayer_layout(
  layout_comparison$between_0.05,
  "Joint: 0.05"
)

p_010 <- plot_multilayer_layout(
  layout_comparison$between_0.1,
  "Joint: 0.10"
)