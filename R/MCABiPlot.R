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
    'color' = colors[color_index],
    'size' = 40,
    'symbol' = 'circle'
  )

  var_formatted <- data.frame(
    'Dim1' = var_coords[,1],
    'Dim2' = var_coords[,2],
    'category' = rownames(var_coords),
    'color' = rgb(1,0,0,0.8),
    'size' = 30,
    'symbol' = 'square'
  )

  formatted <- rbind(ind_formatted, var_formatted)
  formatted[grepl('sus', rownames(formatted)),]$symbol <- 'x-dot'
  formatted[grepl('sus', rownames(formatted)),]$color <- rgb(27,158,119,255, maxColorValue = 255)
  formatted[grepl('sns', rownames(formatted)),]$symbol <- 'cross-dot'
  formatted[grepl('sns', rownames(formatted)),]$color <- rgb(217,95,2,255, maxColorValue = 255)
  formatted[grepl('^ACP', rownames(formatted)),]$symbol <- 'diamond-x'
  formatted[grepl('^ACP', rownames(formatted)),]$color <- rgb(117,112,179,255, maxColorValue = 255)

  dups <- duplicated(formatted[,c(1,2)])
  formatted[dups, 1] <- jitter(formatted[dups, 1], factor = 30)
  formatted[dups, 2] <- jitter(formatted[dups, 2], factor = 30)
  return(formatted)
}
