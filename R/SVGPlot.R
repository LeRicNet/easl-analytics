SVGPlot <- function(type = 'barcodes') {
  switch (type,
    'barcodes' = plotSessionBarcode(),
    'learnability' = plotTrialDurationPerUser(sessions = sessions),
    'sns' = plotNumeracyOverview(sessions = sessions)
  )
}
