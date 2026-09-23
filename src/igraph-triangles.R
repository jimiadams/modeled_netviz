library(igraph)

# 1. Clear out any previous broken triangle registries
if ("triangle" %in% names(shapes())) {
  try(assign("triangle", NULL, envir = igraph:::.shapes), silent=TRUE)
}

# 2. Define the triangle layout using direct, unscaled size values
plot_triangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) vertex.color <- vertex.color[v]
  
  # Crucial Fix: Fetch the pre-calculated internal size exactly as igraph handles it
  vertex.size <- params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) vertex.size <- vertex.size[v]
  
  for (i in 1:nrow(coords)) {
    x <- coords[i, 1]
    y <- coords[i, 2]
    
    # Use vertex.size directly with NO internal multiplier factor
    rad <- vertex.size[if(length(vertex.size) == 1) 1 else i]
    col <- vertex.color[if(length(vertex.color) == 1) 1 else i]
    
    # Generate pure, unscaled polygon coordinates
    polygon(x = c(x, x - rad, x + rad),
            y = c(y + rad, y - rad, y - rad),
            col = col, border = "black")
  }
}

# 3. Register natively with igraph's core layout engine using empty parameters list
add_shape("triangle", 
          clip = shapes("circle")$clip, 
          plot = plot_triangle,
          parameters = list())
