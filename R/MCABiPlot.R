MCABiPlot <- function() {
  coords <- res.mca$ind$coord[,1:2]
  meta <- res.mca$call$X

  original_colors <- c("green", "orange")
  color_palette <- colorRampPalette(original_colors)
  color_index <- as.integer(as.factor(meta$currentPatient))
  colors <- color_palette(max(color_index))
  return(
    data.frame(
      'Dim1' = coords[,1],
      'Dim2' = coords[,2],
      'category' = meta$subspecialty,
      'color' = colors[color_index]
    )
  )
}
