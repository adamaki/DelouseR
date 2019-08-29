# Cleaner fish tank delousing
# Adam Brooker 13th August 2019


setwd('G:/Projects/Lumpfish delousing/Data')
t5tots <- read.csv('DelousingTrial5_tots.csv')

t5tots$time <- dplyr::recode(t5tots$time, '1' = 0, '2' = 24, '3' = 48, '4' = 72, '5' = 96)
t5tots$treatment <- dplyr::recode(t5tots$treatment, 'con' = 'control', 'LL' = 'Large lumpfish', 'SL' = 'Small lumpfish', 'W' = 'Wrasse')

t5tots <- arrange(t5tots, time, treatment, replicate)



# summarise data by time, group and replicate tank
t5summ <- t5tots %>%
  group_by(time, tank, treatment, replicate) %>%
  dplyr::summarise(m_m = mean(total_m), sd_m = sd(total_m), m_f = mean(total_f), sd_f = sd(total_f), tot_m = mean(total), tot_sd = sd(total))

# summarise by time and group with rep as error and draw plot of total lice
t5summ %>%
  group_by(time, treatment) %>%
  dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = total_mean, colour = treatment)) +
  geom_line() +
  geom_errorbar(aes(x = time, ymin = total_mean-total_sd, ymax = total_mean+total_sd), width = 3, position = 'dodge') +
  scale_y_continuous(limits = c(0, 25), name = 'mean lice per fish') +
  scale_x_continuous(breaks = c(0, 24, 48, 72, 96), labels = c('0', '24', '48', '72', '96'), name = 'Time (h)') +
  ggtitle('Total lice') +
  theme_classic() +
  theme(legend.title = element_blank())


# summarise by time and group with rep as error and draw plot of male lice
t5summ %>%
  group_by(time, treatment) %>%
  dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = mean_m, colour = treatment)) +
  geom_line() +
  geom_errorbar(aes(x = time, ymin = mean_m-sd_m, ymax = mean_m+sd_m), width = 3, position = 'dodge') +
  scale_y_continuous(limits = c(0, 15), name = 'mean lice per fish') +
  scale_x_continuous(breaks = c(0, 24, 48, 72, 96), labels = c('0', '24', '48', '72', '96'), name = 'Time (h)') +
  ggtitle('Male lice') +
  theme_classic() +
  theme(legend.title = element_blank())


# summarise by time and group with rep as error and draw plot of female lice
t5summ %>%
  group_by(time, treatment) %>%
  dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = mean_f, colour = treatment)) +
  geom_line() +
  geom_errorbar(aes(x = time, ymin = mean_f-sd_f, ymax = mean_f+sd_f), width = 3, position = 'dodge') +
  scale_y_continuous(limits = c(0, 25), name = 'mean lice per fish') +
  scale_x_continuous(breaks = c(0, 24, 48, 72, 96), labels = c('0', '24', '48', '72', '96'), name = 'Time (h)') +
  ggtitle('Female lice') +
  theme_classic() +
  theme(legend.title = element_blank())

# plot all lice by tank over time
ggplot(data = t5summ, aes(x = time, y = tot_m, group = tank, colour = treatment)) +
  geom_line(size = 2) +
  geom_errorbar(aes(x = time, ymin = tot_m-tot_sd, ymax = tot_m+tot_sd), width = 0.1, position = 'dodge') +
  scale_y_continuous(limits = c(0, 30)) +
  ggtitle('All lice') +
  theme_classic()

