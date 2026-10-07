W <- t(mw) %*% mw
diag(W) <- 0

g_C <- graph_from_adjacency_matrix(
  W,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

V(g_C)$artist <- V(gw)$artist[
  match(V(g_C)$name, V(gw)$name)
]

V(g_C)$genre <- V(gw)$genre[
  match(V(g_C)$name, V(gw)$name)
]

## C: heuristic modeled network
## ----------------------------

# Core membership in the observed source-artist projection
is_core <- V(g_C)$name %in% as.character(V(gas_core)$name)

n_core <- sum(is_core)
n_per  <- sum(!is_core)

# Observed adjacency, treated only as the source for structural summaries
A <- as.matrix(as_adjacency_matrix(
  g_C,
  attr = NULL,
  sparse = FALSE
))

A[A > 0] <- 1
diag(A) <- 0

# Estimate the three core/periphery densities
p_cc <- sum(A[is_core, is_core]) / (n_core * (n_core - 1))
p_cp <- sum(A[is_core, !is_core]) / (n_core * n_per)
p_pp <- sum(A[!is_core, !is_core]) / (n_per * (n_per - 1))

c(
  core_core = p_cc,
  core_periphery = p_cp,
  periphery_periphery = p_pp
)


P <- matrix(
  c(
    p_cc, p_cp,
    p_cp, p_pp
  ),
  nrow = 2,
  byrow = TRUE
)

g_C_model <- igraph::sample_sbm(
  n = n_core + n_per,
  pref.matrix = P,
  block.sizes = c(n_core, n_per),
  directed = FALSE,
  loops = FALSE
)


source_genre <- V(g_C)$genre

genre_core <- source_genre[is_core]
genre_per  <- source_genre[!is_core]

V(g_C_model)$modeled_core <- c(
  rep(TRUE, n_core),
  rep(FALSE, n_per)
)

V(g_C_model)$sampled_genre <- c(
  sample(genre_core, n_core, replace = FALSE),
  sample(genre_per,  n_per,  replace = FALSE)
)

core_idx <- which(V(g_C_model)$modeled_core)
per_idx  <- which(!V(g_C_model)$modeled_core)

theta_core <- seq(0, 2*pi, length.out = length(core_idx) + 1)[-(length(core_idx) + 1)]
theta_per  <- seq(0, 2*pi, length.out = length(per_idx)  + 1)[-(length(per_idx) + 1)]

lay <- matrix(NA, vcount(g_C_model), 2)

lay[core_idx, 1] <- 0.35 * cos(theta_core)
lay[core_idx, 2] <- 0.35 * sin(theta_core)

lay[per_idx, 1] <- cos(theta_per)
lay[per_idx, 2] <- sin(theta_per)

plot(
  g_C_model,
  layout = lay,
  vertex.label = NA,
  vertex.size = scales::rescale(
    degree(g_C_model),
    to = c(3, 10)
  ),
  vertex.color = as.factor(V(g_C_model)$sampled_genre),
  edge.width = 0.5,
  edge.color = "grey70"
)