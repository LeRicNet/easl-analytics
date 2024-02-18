#' Read EASL Session XML
#'
#' This function reads an XML file and extracts EASL session recording details.
#' It returns a dataframe with session details including event category, action, name, timestamp, and server time.
#'
#' @param xml_file_name A string. The name of the XML file to be read.
#'
#' @return A dataframe with columns: 'session', 'eventCategory', 'eventAction', 'eventName', 'timestamp', 'serverTimePretty'.
#' @export
#'
#' @examples
#' \dontrun{
#' read_session_xml("session.xml")
#' }
#'
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
