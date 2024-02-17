
### Data Organization
root_path = "/Users/princee/Library/CloudStorage/OneDrive-TheUniversityofColoradoDenver/Data/laic_sessions_Jan2024/session-xml/"
xml_files = list.files(root_path, full.names = T)
sessions <- lapply(xml_files, read_session_xml)

## Optional Data Breakdown
study_admin <- get_study_administration_data(session_df)
trials <- lapply(sessions, getTrialsDataFrame)

# Persona Characterization
plotNumeracyOverview(sessions)
plotNumeracyVsMeanSelections(sessions)
plotNumeracyVsTrialDuration(sessions, metric = 'mean')
plotNumeracyVsTrialDuration(sessions, metric = 'var')

# Session Barcode
lapply(sessions, plotSessionBarcode)
plotTrialDurationPerUser(sessions)

# Selection Heatmap
plotSelectionsHeatmap(sessions[[3]])

# Inter-Rater Reliability: this is wrong; this is a plot of the agreement of
#                          selection freqs, need to revise
plotPercentAgreement(sessions, max_trials = 4)

plotSelectionFreqByFeature(sessions)

plotConfidenceDifficulty(sessions)



.df <- bind_rows(lapply(trial_schedule, function(trial) {
  trial <- trial[!sapply(trial, is.null)]
  bind_rows(lapply(trial, function(t) {
    .df[.df$timestamp > t$start_time & .df$timestamp < t$end_time,]$ai_enabled = t$ai_enabled
    return(.df)
  }))
}))

.df <- extractConfidenceDifficulty(sessions)
.df %>%
  ggplot(aes(ai_enabled, value, col=task_survey)) +
  geom_boxplot() +
  geom_point(position = position_dodge(1)) +
  facet_wrap(~session)

numeracy_df <- bind_rows(lapply(sessions, calculateNumeracy))
numeracy_df

.df <- .df %>%
  left_join(numeracy_df, by = 'session', relationship = 'many-to-many')

a <- .df %>%
  filter(task_survey == 'difficulty', ai_enabled==F) %>%
  ggplot(aes(value, overall)) +
  geom_density_2d_filled() +
  xlab('confidence') +
  xlim(c(1,5)) +
  ylim(c(1,6)) +
  theme_linedraw() +
  ggtitle('AI disabled')

b <- .df %>%
  filter(task_survey == 'difficulty', ai_enabled==T) %>%
  ggplot(aes(value, overall)) +
  geom_density_2d_filled() +
  xlab('confidence') +
  xlim(c(1,5)) +
  ylim(c(1,6)) +
  theme_linedraw() +
  ggtitle('AI enabled')

cowplot::plot_grid(a, b, nrow=1)
subset(.df, ai_enabled == F & task_survey == 'confidence') %>% tail
subset(.df, ai_enabled == F & task_survey == 'confidence')$value


t.test(
  subset(.df, ai_enabled == T & task_survey == 'difficulty')$value,
  subset(.df, ai_enabled == F & task_survey == 'difficulty')$value
)

t.test(
  subset(.df, ai_enabled == T & task_survey == 'confidence')$value,
  subset(.df, ai_enabled == F & task_survey == 'confidence')$value
)


### Plots
#~*~*~*~*~*~*~*~*~

.tmp <- dplyr::bind_rows(lapply(sessions, function(session) {
  trial_schedule <- getTrialsSchedule(session)
  trial_schedule$duration_sec <- trial_schedule$end_time - trial_schedule$start_time
  trial_schedule$duration_min <- trial_schedule$duration_sec / 60
  trial_schedule$ai_enabled <- c(T, rep(c(T, F), 5))[1:nrow(trial_schedule)]
  return(trial_schedule)
}))

.tmp %>%
  ggplot(aes(ai_enabled, duration_min)) +
  geom_boxplot() +
  geom_point(size=4)

t.test(subset(.tmp, ai_enabled == T)$duration_sec,
subset(.tmp, ai_enabled == F)$duration_sec)

subset(.tmp, ai_enabled == F) %>%
  ggplot(aes(sample = duration_sec)) +
  stat_qq() +
  stat_qq_line()

params <- as.list(MASS::fitdistr(subset(.tmp, ai_enabled == F)$duration_sec, "t")$estimate)
subset(.tmp, ai_enabled == F) %>%
  ggplot(aes(sample = duration_sec)) +
  stat_qq(distribution = qt, dparams = params["df"]) +
  stat_qq_line(distribution = qt, dparams = params["df"])

xbar=mean(.tmp$duration_sec)
n = nrow(.tmp)
s = sd(.tmp$duration_sec)
margin <- qt(0.975,df=n-1)*s/sqrt(n)
lowerinterval <- xbar - margin
upperinterval <- xbar + margin

# Selection Heatmap
lapply(sessions, plotSelectionsHeatmap)
cowplot::plot_grid(plotlist = plist)
plotSelectionsHeatmap(sessions[[3]])

calculateNumeracy(session_df)

trials

bind_rows(lapply(rownames(trials[[1]]), function(v) {
  .agreement <- agree(t(bind_rows(lapply(trials, function(t) { t[v,1:4]}))))
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


trial_schedule <- getTrialsSchedule(sessions[[1]])
trial_schedule



template[template$trial_completed %in% .df$end,]$duration_sec = .df$duration_sec

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
  coord_flip()


trial_schedule
trials


