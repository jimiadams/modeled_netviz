# ============================================================
# Heuristic multilayer modeled visualization
# 
# Purpose:
#   Build a two-plane stand-in using the objects already created
#   in the Lena sampling workflow.
#
# Core logic:
#   - gw  = observed weighted bipartite network
#   - mw  = sampled artist x sampling song weighted incidence
#   - W_artist = sampled-artist projection
#   - W_song   = sampling-song projection
#   - gas_core = empirically identified sampled-artist core/backbone
#
# The plot:
#   - two literal planes
#   - independent FR layouts within each layer
#   - neutral within-layer projected ties
#   - highly de-emphasized observed sampling ties between layers
#   - sampled-artist nodes sized by sampling strength
#   - supplied core/backbone relationships emphasized on sampled layer
#   - separate qualitative palettes for the two genre systems
# ============================================================

library(igraph)
library(dplyr)
library(tibble)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# 0. EXPECTED OBJECTS ALREADY IN YOUR ENVIRONMENT
# ------------------------------------------------------------
# gw       : weighted bipartite igraph object
#            V(gw)$type == TRUE  -> sampling songs
#            V(gw)$type == FALSE -> sampled artists
#
# mw       : weighted biadjacency matrix already created as:
#            mw <- t(as.matrix(as_biadjacency_matrix(gw, attr = "weight")))
#
# gas_core : 26-node sampled-artist core/backbone graph
#
# gw currently carries sampled-artist metadata including:
#            V(gw)$artist
#            V(gw)$genre
#
# NOTE:
# You still need a vector/lookup for the 13 sampling-side
# genres. I leave that as `sampling_genre_lookup` below so
# you can attach your actual field when you decide which
# target-side genre variable to use.

# ------------------------------------------------------------
# 1. WITHIN-LAYER PROJECTIONS
# ------------------------------------------------------------

# mw is sampled artists x sampling songs
W_artist <- mw %*% t(mw)
diag(W_artist) <- 0

W_song <- t(mw) %*% mw
diag(W_song) <- 0

g_artist <- graph_from_adjacency_matrix(
  W_artist,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_song <- graph_from_adjacency_matrix(
  W_song,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

# ------------------------------------------------------------
# 2. ATTACH SAMPLED-ARTIST ATTRIBUTES TO THE PROJECTION
# ------------------------------------------------------------

artist_meta <- tibble(
  name = as.character(V(gw)$name),
  artist = V(gw)$artist,
  genre = V(gw)$genre,
  bipartite_type = V(gw)$type
) |>
  filter(!bipartite_type) |>
  distinct(name, .keep_all = TRUE)

artist_meta <- artist_meta |>
  mutate(
    name = as.character(name)
  )

artist_projection_meta <- tibble(
  name = as.character(V(g_artist)$name)
) |>
  left_join(
    artist_meta |>
      select(name, artist, genre),
    by = "name"
  )

V(g_artist)$artist <- artist_projection_meta$artist
V(g_artist)$genre <- artist_projection_meta$genre

# ------------------------------------------------------------
# 3. ATTACH SAMPLING-SONG ATTRIBUTES
# ------------------------------------------------------------

song_meta <- tibble(
  name = as.character(V(gw)$name),
  artist = V(gw)$artist,
  song_title = V(gw)$song_title,
  year = V(gw)$year,
  bipartite_type = V(gw)$type
) |>
  filter(bipartite_type) |>
  distinct(name, .keep_all = TRUE)

song_projection_meta <- tibble(
  name = as.character(V(g_song)$name)
) |>
  left_join(
    song_meta |>
      select(name, artist, song_title, year),
    by = "name"
  )

V(g_song)$artist <- song_projection_meta$artist
V(g_song)$song_title <- song_projection_meta$song_title
V(g_song)$year <- song_projection_meta$year

# ------------------------------------------------------------
# 4. CORE / BACKBONE INFORMATION
# ------------------------------------------------------------

# gas_core is your empirically identified 26-node source-artists
# core/backbone. Its node names should match the sampled-artist
# names in g_artist / gw.

core_names <- as.character(V(gas_core)$name)

V(g_artist)$is_core <- V(g_artist)$name %in% core_names

# Keep the supplied core graph as the emphasized skeleton.
# The full artist projection remains underneath it.

# ------------------------------------------------------------
# 5. NODE SIZE = SAMPLING STRENGTH
# ------------------------------------------------------------

# Weighted sampling strength of sampled artists in the observed
# bipartite graph.
sampled_vertices_gw <- V(gw)[!V(gw)$type]

sampled_strength <- strength(
  gw,
  v = sampled_vertices_gw,
  weights = E(gw)$weight
)

sampled_strength_lookup <- tibble(
  name = as.character(sampled_vertices_gw$name),
  sampling_strength = as.numeric(sampled_strength)
)

artist_projection_nodes <- tibble(
  name = as.character(V(g_artist)$name)
) |>
  left_join(
    sampled_strength_lookup,
    by = "name"
  )

# Log compression for the strongly right-skewed distribution.
artist_projection_nodes <- artist_projection_nodes |>
  mutate(
    size_scaled = rescale(
      log1p(sampling_strength),
      to = c(2.0, 5.0)
    )
  )

# ------------------------------------------------------------
# 6. SAMPLING-SIDE GENRE LOOKUP
# ------------------------------------------------------------
# IMPORTANT:
# Replace `sampling_genre_lookup` with your actual target-song /
# sampling-side genre variable.
#
# It must have:
#     name  = the same IDs used for V(g_song)$name
#     genre = one of your 13 sampling-side genre categories
#
# Example structure:
#
# sampling_genre_lookup <- tibble(
#   name = as.character(...),
#   genre = ...
# )
#
# For now, this assumes you have already created that lookup.

# ---- EDIT THIS BLOCK TO MATCH YOUR DATA ----
sampling_genre_lookup <- tibble(
  name = character(),
  genre = character()
)
# --------------------------------------------

song_projection_nodes <- tibble(
  name = as.character(V(g_song)$name)
) |>
  left_join(
    sampling_genre_lookup,
    by = "name"
  )

# ------------------------------------------------------------
# 7. INDEPENDENT FR LAYOUTS
# ------------------------------------------------------------

set.seed(123)

layout_song <- layout_with_fr(
  g_song,
  weights = E(g_song)$weight
)

layout_artist <- layout_with_fr(
  g_artist,
  weights = E(g_artist)$weight
)

# ------------------------------------------------------------
# 8. CONTAIN EACH LAYER WITHIN ITS OWN PLANE
# ------------------------------------------------------------
# Normalize each independently, then map into the interior of
# a literal rectangular/parallelogram plane.

song_xy <- tibble(
  name = as.character(V(g_song)$name),
  x = layout_song[, 1],
  y = layout_song[, 2]
)

artist_xy <- tibble(
  name = as.character(V(g_artist)$name),
  x = layout_artist[, 1],
  y = layout_artist[, 2]
)

# Sampling-song plane
sx_range <- range(song_xy$x, na.rm = TRUE)
sy_range <- range(song_xy$y, na.rm = TRUE)

song_xy <- song_xy |>
  mutate(
    x = scales::rescale(x, to = c(-3.85, 3.85)),
    y = scales::rescale(y, to = c(0.80, 2.35))
  )

# Sampled-artist plane
ax_range <- range(artist_xy$x, na.rm = TRUE)
ay_range <- range(artist_xy$y, na.rm = TRUE)

artist_xy <- artist_xy |>
  mutate(
    x = scales::rescale(x, to = c(-3.85, 3.85)),
    y = scales::rescale(y, to = c(-2.35, -0.80))
  )

# ------------------------------------------------------------
# 9. NODE TABLES
# ------------------------------------------------------------

song_nodes <- song_xy |>
  left_join(
    song_projection_nodes |>
      select(name, genre),
    by = "name"
  )

artist_nodes <- artist_xy |>
  left_join(
    artist_projection_nodes |>
      select(name, sampling_strength, size_scaled),
    by = "name"
  ) |>
  left_join(
    artist_projection_meta |>
      select(name, artist, genre),
    by = "name"
  ) |>
  mutate(
    is_core = name %in% core_names
  )

# ------------------------------------------------------------
# 10. EDGE COORDINATES
# ------------------------------------------------------------

# A. Sampling-song projected ties
song_edges <- as_data_frame(g_song, what = "edges") |>
  select(from, to, weight) |>
  left_join(
    song_xy |> rename(x_from = x, y_from = y),
    by = c("from" = "name")
  ) |>
  left_join(
    song_xy |> rename(x_to = x, y_to = y),
    by = c("to" = "name")
  )

# B. Sampled-artist projected ties
artist_edges <- as_data_frame(g_artist, what = "edges") |>
  select(from, to, weight) |>
  left_join(
    artist_xy |> rename(x_from = x, y_from = y),
    by = c("from" = "name")
  ) |>
  left_join(
    artist_xy |> rename(x_to = x, y_to = y),
    by = c("to" = "name")
  )

artist_edges <- artist_edges |>
  mutate(
    core_edge = from %in% core_names & to %in% core_names
  )

# C. Actual observed bipartite sampling ties
observed_edges <- as_data_frame(gw, what = "edges") |>
  select(from, to, weight)

observed_edges <- observed_edges |>
  left_join(
    song_xy |> rename(x_from = x, y_from = y),
    by = c("from" = "name")
  ) |>
  left_join(
    artist_xy |> rename(x_to = x, y_to = y),
    by = c("to" = "name")
  ) |>
  filter(
    !is.na(x_from),
    !is.na(y_from),
    !is.na(x_to),
    !is.na(y_to)
  )

# ------------------------------------------------------------
# 11. ACTUAL CORE/BACKBONE EDGES
# ------------------------------------------------------------

core_edges <- as_data_frame(gas_core, what = "edges") |>
  select(from, to, weight) |>
  left_join(
    artist_xy |> rename(x_from = x, y_from = y),
    by = c("from" = "name")
  ) |>
  left_join(
    artist_xy |> rename(x_to = x, y_to = y),
    by = c("to" = "name")
  ) |>
  filter(
    !is.na(x_from),
    !is.na(y_from),
    !is.na(x_to),
    !is.na(y_to)
  )

# ------------------------------------------------------------
# 12. GENRE PALETTES
# ------------------------------------------------------------
# Eight sampled-side genres and thirteen sampling-side genres
# should be perceptually distinct systems. These are placeholders;
# replace with your preferred final palettes.

sampled_palette <- c(
  "#0072B2", "#009E73", "#56B4E9", "#E69F00",
  "#CC79A7", "#F0E442", "#D55E00", "#666666"
)

sampling_palette <- c(
  "#7A0177", "#AE017E", "#DD3497", "#F768A1", "#FA9FB5",
  "#006D2C", "#31A354", "#74C476", "#1D91C0", "#41B6C4",
  "#225EA8", "#A6761D", "#B15928"
)

# Named palettes for the actual factor values.
sampled_genre_values <- sort(unique(na.omit(artist_nodes$genre)))
sampling_genre_values <- sort(unique(na.omit(song_nodes$genre)))

sampled_palette_named <- setNames(
  sampled_palette[seq_along(sampled_genre_values)],
  sampled_genre_values
)

sampling_palette_named <- setNames(
  sampling_palette[seq_along(sampling_genre_values)],
  sampling_genre_values
)

# ------------------------------------------------------------
# 13. LITERAL PLANES
# ------------------------------------------------------------

planes <- tibble(
  layer = c("sampling", "sampled"),
  xmin = c(-4.20, -4.20),
  xmax = c(4.20, 4.20),
  ymin = c(0.55, -2.55),
  ymax = c(2.60, -0.45)
)

# ------------------------------------------------------------
# 14. PLOT
# ------------------------------------------------------------

p <- ggplot() +

  # ---- literal planes ----
  geom_rect(
    data = planes,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    fill = "grey90",
    color = NA,
    alpha = 0.78
  ) +

  # ---- direct observed sampling ties: very faint ----
  geom_segment(
    data = observed_edges,
    aes(
      x = x_from,
      y = y_from,
      xend = x_to,
      yend = y_to
    ),
    color = "grey55",
    alpha = 0.035,
    linewidth = 0.35
  ) +

  # ---- sampling-side projected ties ----
  geom_segment(
    data = song_edges,
    aes(
      x = x_from,
      y = y_from,
      xend = x_to,
      yend = y_to
    ),
    color = "grey45",
    alpha = 0.22,
    linewidth = 0.45
  ) +

  # ---- full sampled-artist projected ties, subdued ----
  geom_segment(
    data = artist_edges |> filter(!core_edge),
    aes(
      x = x_from,
      y = y_from,
      xend = x_to,
      yend = y_to
    ),
    color = "grey45",
    alpha = 0.055,
    linewidth = 0.25
  ) +

  # ---- emphasized core/backbone relationships ----
  geom_segment(
    data = core_edges,
    aes(
      x = x_from,
      y = y_from,
      xend = x_to,
      yend = y_to,
      linewidth = weight
    ),
    color = "grey25",
    alpha = 0.78
  ) +

  # ---- sampling layer nodes ----
  geom_point(
    data = song_nodes,
    aes(
      x = x,
      y = y,
      color = genre
    ),
    shape = 17,
    size = 2.8,
    alpha = 0.92
  ) +

  # ---- sampled artist layer: peripheral nodes ----
  geom_point(
    data = artist_nodes |> filter(!is_core),
    aes(
      x = x,
      y = y,
      color = genre,
      size = size_scaled
    ),
    shape = 16,
    alpha = 0.45
  ) +

  # ---- sampled artist layer: core nodes ----
  geom_point(
    data = artist_nodes |> filter(is_core),
    aes(
      x = x,
      y = y,
      color = genre,
      size = size_scaled
    ),
    shape = 16,
    alpha = 0.98,
    stroke = 0.65
  ) +

  # Separate node scales; tune as needed.
  scale_size_continuous(
    range = c(2.2, 5),
    name = "Sampling strength"
  ) +

  scale_color_manual(
    values = c(
      sampled_palette_named,
      sampling_palette_named
    ),
    na.value = "grey50"
  ) +

  scale_linewidth_continuous(
    range = c(0.35, 2.0),
    guide = "none"
  ) +

  coord_equal(
    xlim = c(-4.8, 5.8),
    ylim = c(-3.05, 3.0),
    expand = FALSE
  ) +

  labs(
    title = "Heuristic multilayer modeled representation",
    subtitle = paste(
      "Within-layer ties are projected similarities; faint cross-layer ties retain",
      "the observed sampling relationships."
    ),
    color = "Genre"
  ) +

  annotate(
    "text",
    x = -4.05,
    y = 2.78,
    label = "Sampling layer",
    hjust = 0,
    fontface = "bold",
    size = 5
  ) +

  annotate(
    "text",
    x = -4.05,
    y = -0.22,
    label = "Sampled-artist layer",
    hjust = 0,
    fontface = "bold",
    size = 5
  ) +

  theme_void() +
  theme(
    plot.title = element_text(size = 18, hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0),
    legend.position = "right"
  )

p

# ------------------------------------------------------------
# 15. SAVE
# ------------------------------------------------------------

ggsave(
  filename = "heuristic_multilayer_modeled_standin.pdf",
  plot = p,
  width = 13,
  height = 9,
  units = "in"
)

ggsave(
  filename = "heuristic_multilayer_modeled_standin.png",
  plot = p,
  width = 13,
  height = 9,
  units = "in",
  dpi = 300
)
