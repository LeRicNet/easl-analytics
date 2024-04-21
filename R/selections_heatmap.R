#' Plot Selection Heatmap By Patient
#'
#' This function generates a selection heatmap for a given patient or for all patients if no specific patient is provided.
#' The heatmap represents the ATPC profile features for each session ID. The function uses the `ggplot2` and `cowplot` packages for plotting.
#'
#' @param patient (Optional) The patient for whom the heatmap is to be plotted. If not provided, the function will plot heatmaps for all unique patients in the `user_responses` data frame.
#' @param ai_enabled (Optional) A boolean flag indicating whether AI is enabled or not. If set to TRUE (default), the function will plot separate heatmaps for when AI is enabled and disabled.
#'
#' @return A `ggplot` object representing the heatmap(s).
#'
#' @examples
#' plotSelectionHeatmapByPatient(patient = "John Doe", ai_enabled = TRUE)
#'
#' @export
plotSelectionHeatmapByPatient <- function(patient = NULL, ai_enabled = TRUE) {
  if (is.null(patient) & ai_enabled == TRUE) {
    plist <- lapply(unique(user_responses$currentPatient), function(patient) {
      dplyr::bind_rows(user_responses, ai_responses, ai_groundtruth) %>%
        dplyr::mutate(grp = ifelse(grepl('AI', sessionID), sessionID, 'User')) %>%
        dplyr::filter(currentPatient == patient) %>%
        dplyr::select(sessionID,grp, tumor_location:pcf_involvement) %>%
        reshape2::melt(id.vars = c('sessionID', 'grp')) %>%
        ggplot2::ggplot(aes(variable, sessionID, fill=value)) +
        ggplot2::geom_tile(col='white') +
        ggplot2::coord_flip() +
        ggplot2::theme_linedraw(base_size=14) +
        ggplot2::theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              legend.position = 'none') +
        ggplot2::labs(
          title = patient,
          x = 'ATPC Profile feature'
        ) +
        ggplot2::facet_wrap(~grp, nrow=1, scales = 'free_x', strip.position = 'bottom')
    })

    ai_enabled <- cowplot::plot_grid(
      plotlist=plist[c(1,2,4,6,8,10)]
    )

    ai_disabled <- cowplot::plot_grid(
      plotlist=plist[c(3,5,7,9)]
    )

    return(cowplot::plot_grid(ai_enabled, ai_disabled, ncol = 1, labels = c('AI enabled', 'AI disabled')))
  } else if (!is.null(patient)) {
    dplyr::bind_rows(user_responses, ai_responses, ai_groundtruth) %>%
      dplyr::mutate(grp = ifelse(grepl('AI', sessionID), sessionID, 'User')) %>%
      dplyr::filter(currentPatient == patient) %>%
      dplyr::select(sessionID, grp, tumor_location:pcf_involvement) %>%
      reshape2::melt(id.vars = c('sessionID', 'grp')) %>%
      ggplot2::ggplot(ggplot2::aes(variable, sessionID, fill=value)) +
      ggplot2::geom_tile(col='white') +
      ggplot2::coord_flip() +
      ggplot2::theme_linedraw(base_size=14) +
      ggplot2::theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            legend.position = 'none') +
      ggplot2::labs(
        title = patient,
        x = 'ATPC Profile feature'
      ) +
      ggplot2::facet_wrap(~grp, nrow=1, scales = 'free_x', strip.position = 'bottom')
  }
}

plotSelectionsHeatmap <- function(session_df, filename = NA, height = NA, width = NA) {
  trials_df <- getTrialsDataFrame(session_df = session_df)
  # Define the color range
  colors <- colorRampPalette(c("gray", "white", "black"))(255)

  # Set breaks for the color scale
  breaks <- c(1, 4)
  return(
    pheatmap::pheatmap(
      trials_df,
      cluster_rows=F,
      cluster_cols=F,
      display_numbers = T,
      number_format = '%.f',
      fontsize_number = 14,
      legend = F,
      main = 'selections (n) per feature and trial',
      color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 3, name =
                                                "RdYlBu")))(3),
      breaks = c(-1, 0:4),
      filename = filename,
      height = height,
      width = width
      )
  )
}

getTrialsSchedule <- function(session_df, return.list=F) {
  study_admin <- get_study_administration_data(session_df)

  begin_time <- study_admin %>%
    dplyr::filter(eventName == 'beginStudy') %>%
    dplyr::select(timestamp) %>%
    as.numeric()

  next_patient_triggers <- study_admin %>%
    dplyr::filter(eventName %in% c('beginStudy', 'nextPatient'))

  session_trials <- next_patient_triggers %>%
    dplyr::mutate(
      trial = c('begin', paste0('trial_', 1:(nrow(.)-1), '_end'))
    )

  trial_schedule <- lapply(1:nrow(session_trials), function(i) {
    ai.enabled <- c(1,2,4,6,8,10)
    if (i < nrow(session_trials)) {
      return(
        list(
          'session' = session_df$session,
          'start' = session_trials$trial[i],
          'start_time' = as.numeric(session_trials$timestamp[i]),
          'end' = session_trials$trial[i+1],
          'end_time' = as.numeric(session_trials$timestamp[i+1]),
          'ai_enabled' = ifelse(i %in% ai.enabled, T, F)
        )
      )
    }
  })
  if (return.list) {
    return(trial_schedule)
  } else {
    return(dplyr::bind_rows(trial_schedule))
  }
}

getTrialsDataFrame <- function(session_df) {
  study_admin <- get_study_administration_data(session_df)
  trial_schedule <- getTrialsSchedule(session_df, return.list = T)

  trials_list <- lapply(trial_schedule, function(trial) {
    if (!is.null(trial)) {
      start <- trial$start_time
      end <- trial$end_time
      .trial <- session_df %>%
        mutate(timestamp = as.numeric(timestamp)) %>%
        filter(eventCategory == 'Feature Form') %>%
        filter(timestamp > start, timestamp < end) %>%
        mutate(
          feature_group = sapply(strsplit(eventName, '-'), '[', 2),
          value = sapply(strsplit(eventName, '-'), '[', 3)
        )

      .trial_table <- table(.trial$feature_group)
      .trial_table <- .trial_table %>%
        as.data.frame() %>%
        mutate(Var1=factor(Var1, levels=c(
          'tumor_location',
          'tumor_type',
          'hypoinvasion',
          'skullbase_invasion_involvement',
          'mamillarybody_invasion_involvement',
          'opticchiasm_invasion_involvement',
          'hypo_edema',
          'chiasm_edema',
          'acf_involvement',
          'mcf_involvement',
          'pcf_involvement'
        )))

      # template
      template <- tibble::tibble(
        Var1=factor(c(
          'tumor_location',
          'tumor_type',
          'hypoinvasion',
          'skullbase_invasion_involvement',
          'mamillarybody_invasion_involvement',
          'opticchiasm_invasion_involvement',
          'hypo_edema',
          'chiasm_edema',
          'acf_involvement',
          'mcf_involvement',
          'pcf_involvement'
        )), Freq=0)

      template[(template$Var1 %in% .trial_table$Var1),]$Freq = .trial_table$Freq
      return(template)
    }
  })
  trials_list <- trials_list[!sapply(trials_list, is.null)]
  trials_df <- as.data.frame(lapply(trials_list, function(x) x$Freq))
  names(trials_df) <- paste0('trial_', 1:ncol(trials_df))
  rownames(trials_df) <- trials_list[[1]]$Var1
  return(trials_df)
}
