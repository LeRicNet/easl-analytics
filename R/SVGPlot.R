SVGPlot <- function(type = 'barcodes', ...) {
  switch (type,
    'barcodes' = cowplot::plot_grid(plotlist=lapply(sessions, plotSessionBarcode)),
    'learnability' = plotTrialDurationPerUser(sessions = sessions),
    'sns' = plotNumeracyOverview(sessions = sessions),
    'annotations' = plotSelectionHeatmapByPatient(...)
  )
}
