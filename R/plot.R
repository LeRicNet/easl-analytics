plotSessionBarcode <- function(session_df) {
  return(
    session_df %>%
      tidyr::drop_na() %>%
      ggplot2::ggplot(ggplot2::aes(timestamp, eventCategory, fill=eventCategory)) +
      ggplot2::geom_tile() +
      ggplot2::theme_bw(base_size=20) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        legend.position = 'none') +
      ggplot2::xlab('time/steps') +
      ggplot2::ylab('event category')
  )
}

plotTrialDurationPerUser <- function(sessions, return.data=FALSE) {
  .df <- dplyr::bind_rows(lapply(sessions, function(session) {
    .df <- getTrialsSchedule(session) %>%
      unique() %>%
      dplyr::mutate(duration_sec = end_time - start_time,
                    duration_min = duration_sec / 60)
    template <- tibble(
      sessionID = unique(.df$session),
      session_start = session$timestamp[1],
      trial_completed = paste0('trial_', 1:10, '_end'),
      duration_sec = 0
    )

    template[template$trial_completed %in% .df$end,]$duration_sec = .df$duration_sec

    return(template)
  }))

  if (return.data) {
   return(.df)
  } else {
    .df %>%
      group_by(session_start) %>%
      mutate(trial_completed = factor(sapply(strsplit(trial_completed, '_'), '[', 2), levels=c(1:10))) %>%
      mutate(duration_min = duration_sec / 60) %>%
      ggplot(aes(trial_completed, duration_min)) +
      geom_boxplot(alpha=0.2) +
      geom_point(aes(col=session_start), alpha=0.2) +
      geom_smooth(aes(col=session_start, group=session_start), method='lm', se = F) +
      theme_linedraw(base_size = 18) +
      theme(legend.position = 'none') +
      labs(
        x = 'trial',
        y = 'duration (min)'
      )
  }
}

plotNumeracyOverview <- function(sessions) {
  return(
    dplyr::bind_rows(lapply(sessions, calculateNumeracy)) %>%
      dplyr::mutate(id=factor(1:nrow(.))) %>%
      reshape2::melt(id.vars = 'id') %>%
      ggplot2::ggplot(aes(variable, value)) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_point(aes(col=id), size=5) +
      ggplot2::geom_line(aes(group=id, col=id)) +
      ggplot2::ylim(c(0,6)) +
      ggplot2::xlab('subjective numeracy score') +
      ggplot2::theme_linedraw(base_size=14) +
      ggplot2::theme(legend.position = 'none')
  )
}

plotNumeracyVsMeanSelections <- function(sessions) {
  trials <- lapply(sessions, getTrialsDataFrame)
  cowplot::plot_grid(plotlist=lapply(
    c('overall', 'ability', 'preference'),
    function(y) {
    tibble::tibble(mean_selections = unlist(lapply(trials, function(x) { mean(x[x>0]) }))) %>%
      dplyr::bind_cols(
        dplyr::bind_rows(lapply(sessions, calculateNumeracy))
      ) %>%
      dplyr::mutate(id=factor(1:nrow(.))) %>%
      ggplot(aes(get(y), mean_selections)) +
      ggplot2::geom_point(aes(col=id),size=5) +
      ggplot2::theme_linedraw(base_size = 14) +
      ggplot2::theme(legend.position = 'none') +
      ggplot2::xlab(y)
  }))
}

plotNumeracyVsTrialDuration <- function(sessions, metric='mean') {
  cowplot::plot_grid(plotlist=lapply(
    c('overall', 'ability', 'preference'),
    function(y) {
      .df <- dplyr::bind_rows(lapply(sessions, function(session) {
        getTrialsSchedule(session) %>%
          dplyr::mutate(duration_sec = end_time - start_time,
                 duration_min = duration_sec / 60) %>%
          dplyr::summarize(
            mean_duration_sec = mean(duration_sec),
            var_duration_sec = var(duration_sec),
            mean_duration_min = mean(duration_min),
            var_duration_min = var(duration_min)
          )
      })) %>%
        dplyr::bind_cols(
          dplyr::bind_rows(lapply(sessions, calculateNumeracy))
        )

      if (metric == 'mean') {
        .df %>%
          ggplot2::ggplot(aes(get(y), mean_duration_min)) +
          ggplot2::geom_point() +
          ggplot2::geom_smooth(method = 'loess') +
          ggplot2::labs(
            x = paste0('numeracy: ', y),
            y = 'mean duration/trial (min)',
          ) +
          ggplot2::theme_linedraw(base_size = 18)
      } else if (metric == 'var') {
        .df %>%
          ggplot2::ggplot(aes(get(y), var_duration_min)) +
          ggplot2::geom_point() +
          ggplot2::geom_smooth(method = 'loess') +
          ggplot2::labs(
            x = paste0('numeracy: ', y),
            y = 'variance duration/trial (min)',
          ) +
          ggplot2::theme_linedraw(base_size = 18)
      }

    }
  ))
}

plotPercentAgreement <- function(sessions, max_trials = 4) {
  trials <- lapply(sessions, getTrialsDataFrame)
  bind_rows(lapply(rownames(trials[[1]]), function(v) {
    .agreement <- agree(t(bind_rows(lapply(trials, function(t) { t[v,1:max_trials]}))))
    tibble(
      var = v,
      subjects = .agreement$subjects,
      raters = .agreement$raters,
      agreement_pct = .agreement$value
    )
  })) %>%
    arrange(desc(agreement_pct)) %>%
    mutate(var = factor(var, levels=var)) %>%
    ggplot(aes(var, agreement_pct)) +
    geom_bar(stat='identity') +
    theme_linedraw(base_size=18) +
    theme(axis.text.x = element_text(angle=50, hjust=1)) +
    labs(
      x = '',
      y = 'agreement (%)'
    )
}

plotSelectionFreqByFeature <- function(sessions) {
  trials <- lapply(sessions, getTrialsDataFrame)
  .tmp <- bind_rows(lapply(trials, rowMeans)) %>%
    mutate(id=factor(1:nrow(.))) %>%
    melt(id.vars = 'id') %>%
    group_by(variable) %>%
    mutate(mean = mean(value)) %>%
    arrange(desc(mean))

  .tmp %>%
    mutate(variable = factor(variable, levels=rev(unique(.tmp$variable)))) %>%
    ggplot(aes(variable, value)) +
    geom_boxplot(alpha=0.2) +
    geom_point(aes(col=id), size=5, alpha=0.2) +
    geom_point(aes(variable, mean), size=5) +
    geom_line(aes(variable, mean), group=1, size=1) +
    geom_hline(yintercept=1) +
    theme_linedraw(base_size=18) +
    labs(
      x=''
    ) +
    coord_flip()
}

plotConfidenceDifficulty <- function(sessions) {
  .df <- extractConfidenceDifficulty(sessions)
  return(
    .df %>%
      ggplot2::ggplot(ggplot2::aes(task_survey,
                                   value,
                          fill=ai_enabled)) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_point()
  )
}
