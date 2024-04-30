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
    'size' = 30,
    'symbol' = 'circle'
  )

  var_formatted <- data.frame(
    'Dim1' = var_coords[,1],
    'Dim2' = var_coords[,2],
    'category' = rownames(var_coords),
    'color' = rgb(1,0,0,0.8),
    'size' = 20,
    'symbol' = 'square'
  )

  formatted <- rbind(ind_formatted, var_formatted)
  formatted$category <- as.character(formatted$category)
  formatted[grepl('^AI-', rownames(formatted)),]$category <- 'AI'
  formatted[grepl('^AI-', rownames(formatted)),]$symbol <- 'star'
  formatted[grepl('^AI-', rownames(formatted)),]$size <- 30
  formatted[grepl('^AI-', rownames(formatted)),]$color <- rgb(99,99,99,255, maxColorValue = 255)
  formatted[grepl('Ground Truth', rownames(formatted)),]$category <- 'AI-Ground Truth'
  formatted[grepl('Ground Truth', rownames(formatted)),]$symbol <- 'hexagon'
  formatted[grepl('Ground Truth', rownames(formatted)),]$size <- 30
  formatted[grepl('Ground Truth', rownames(formatted)),]$color <- rgb(158,202,225,255, maxColorValue = 255)
  formatted <- formatted %>%
    filter(!grepl('.NA$', category)) %>%
    filter(!grepl('^20', category)) %>%
    filter(!grepl('TRUE', category)) %>%
    filter(!grepl('FALSE', category)) %>%
    filter(!grepl('overall', category)) %>%
    filter(!grepl('ability', category)) %>%
    filter(!grepl('preference', category))
  formatted[grepl('sus', rownames(formatted)),]$symbol <- 'x-dot'
  formatted[grepl('sus', rownames(formatted)),]$color <- rgb(27,158,119,255, maxColorValue = 255)
  formatted[grepl('sns', rownames(formatted)),]$symbol <- 'cross-dot'
  formatted[grepl('sns', rownames(formatted)),]$color <- rgb(217,95,2,255, maxColorValue = 255)
  formatted[grepl('^ACP', rownames(formatted)),]$symbol <- 'diamond-x'
  formatted[grepl('^ACP', rownames(formatted)),]$color <- rgb(117,112,179,255, maxColorValue = 255)


  dups <- duplicated(formatted[,c(1,2)])
  formatted[dups, 1] <- jitter(formatted[dups, 1], factor = 100)
  formatted[dups, 2] <- jitter(formatted[dups, 2], factor = 100)
  formatted$name <- sapply(strsplit(rownames(formatted), "-"), "[", 1)
  return(formatted)
}
