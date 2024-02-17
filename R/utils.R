get_study_administration_data <- function(session_df) {
  return(
    session_df %>%
      dplyr::filter(eventCategory == 'Study Administration')
  )
}

calculateNumeracy <- function(session_df) {
  .sns_df <- session_df %>%
    dplyr::filter(eventCategory == 'SNS') %>%
    mutate(
      question = sapply(strsplit(sapply(strsplit(eventName, '_'), '[', 2), '-'), '[', 1),
      value = as.numeric(sapply(strsplit(eventName, '-'), '[', 2))
    )
  .sns_df$value[7] = 6 - .sns_df$value[7]
  return(tibble(
    session = .sns_df$session,
    overall = mean(.sns_df$value),
    ability = mean(.sns_df$value[1:4]),
    preference = mean(.sns_df$value[5:8])
  ))
}

extractConfidenceDifficulty <- function(sessions) {
  dplyr::bind_rows(lapply(sessions, function(session) {
    session %>%
      dplyr::filter(eventCategory == 'Task Survey Responses') %>%
      dplyr::filter(grepl('-', eventName)) %>%
      dplyr::mutate(ai_enabled = c(T, T, rep(c(T,T,F,F), 5))[1:nrow(.)]) %>%
      dplyr::mutate(task_survey = ifelse(grepl('_1', eventName),
                                         'difficulty', 'confidence'),
                    value = as.numeric(sapply(strsplit(eventName, '-'), '[', 2)))
  }))
}
