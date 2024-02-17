read_session_xml <- function(xml_file_name) {
  session.recording = xml2::as_list(xml2::read_xml(xml_file_name))
  xml_df <- dplyr::bind_cols(
    unlist(session.recording$result$lastVisits$row$actionDetails
           )
    )
  return (dplyr::bind_rows(lapply(
    session.recording$result$lastVisits$row$actionDetails,
    function(event) {
      tibble::tibble(
        'session' = gsub('.xml', '', rev(unlist(strsplit(xml_file_name, '/')))[1]),
        'eventCategory' = unlist(event$eventCategory),
        'eventAction' = unlist(event$eventAction),
        'eventName' = unlist(event$eventName),
        'timestamp' = unlist(event$timestamp),
        'serverTimePretty' = unlist(event$serverTimePretty)
      )
    })))
}
