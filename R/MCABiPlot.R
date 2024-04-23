MCABiPlot <- function() {
  ind_coords <- res.mca$ind$coord[,1:2]
  var_coords <- res.mca$var$coord[,1:2]
  meta <- res.mca$call$X

  original_colors <- c("green", "orange")
  color_palette <- colorRampPalette(original_colors)
  color_index <- as.integer(as.factor(meta$currentPatient))
  colors <- color_palette(max(color_index))

  ind_formatted <- data.frame(
    'Dim1' = ind_coords[,1],
    'Dim2' = ind_coords[,2],
    'category' = meta$subspecialty,
    'color' = colors[color_index]
  )

  var_formatted <- data.frame(
    'Dim1' = var_coords[,1],
    'Dim2' = var_coords[,2],
    'category' = rownames(var_coords),
    'color' = rgb(1,0,0,0.8)
  )

  return(rbind(ind_formatted, var_formatted))
}
