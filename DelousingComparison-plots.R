# Cleaner fish tank delousing
# Adam Brooker 20th April 2020

source('/Users/adambrooker/R Projects/DelouseR/Delousing-initiate.R')

# Load trial 1 data and reformat
t1data <- read.csv('/Users/adambrooker/Dropbox/1-IoA/cleanerfish/Projects/SAIC Lumpfish/Delousing Trials/T1 Summer/t1summerdata.csv')

t1data$time <- dplyr::recode(t1data$time, '1' = 0, '2' = 24, '3' = 48, '4' = 72, '5' = 96)
t1data$treatment <- dplyr::recode(t1data$treatment, 'con' = 'Control', 'LL' = 'Large lumpfish', 'SL' = 'Small lumpfish', 'W' = 'Wrasse')

t1data <- t1data %>% mutate(total.m = t1data %>% rowwise() %>% select(contains('.m')) %>% rowSums()) # new total male col
t1data <- t1data %>% mutate(total.f = t1data %>% rowwise() %>% select(contains('.f')) %>% rowSums()) # new total female col
t1data$total <- t1data$total.m + t1data$total.f

t1data <- arrange(t1data, time, treatment, replicate) # arrange data

# Load trial 2 data and reformat



# Figure 1. summarise % decrease in lice by time and group with rep as error and draw plot of total lice-----------

t1means <- t1data %>%
  group_by(time, tank, treatment, replicate) %>%
  dplyr::summarise(mean.m = mean(total.m), mean.f = mean(total.f), mean.t = mean(total.m + total.f))

s.means <- filter(t1means, time == 0) %>%
  ungroup() %>%
  select(-'time')
colnames(s.means) <- c('tank', 'treatment', 'replicate', 'start.m', 'start.f', 'start.t')

t1means <- t1means %>%
  left_join(s.means, by = c('tank', 'treatment', 'replicate'))
rm(s.means)

t1means <- t1means %>%
  dplyr::mutate(diff.m = (mean.m/start.m)*100, diff.f = (mean.f/start.f)*100, diff.t = (mean.t/start.t)*100)
  
t1dec.p <- t1means %>%
  group_by(time, treatment) %>%
#  dplyr::summarise(mean_m = mean(diff.m), sd_m = sd(diff.m), mean_f = mean(diff.f), sd_f = sd(diff.f), total_mean = mean(diff.t), total_sd = sd(diff.t)) %>% # standard deviation
  dplyr::summarise(mean_m = mean(diff.m), sd_m = sd(diff.m)/sqrt(3), mean_f = mean(diff.f), sd_f = sd(diff.f)/sqrt(3)) %>% # standard error
  gather(key = v, value = value, mean_m:sd_f) %>%
  separate(col = v, into = c('stat', 'gender')) %>%
  arrange(time) %>%
  spread(stat, value) %>%
  ggplot(aes(x = time, colour = treatment)) +
  geom_line(aes(y = mean, linetype = gender), size = 0.5) +
  geom_point(aes(y = mean, shape = treatment, size = treatment)) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 3, size = 0.5, position = 'dodge') +
  scale_y_continuous(limits = c(0, 170), breaks = seq(0, 180, 10), name = 'Change in lice No. (%)', expand = c(0, 0)) +
  scale_x_continuous(breaks = c(0, 24, 48, 72, 96), labels = c('0', '24', '48', '72', '96'), name = 'Time (h)', expand = c(0, 0)) +
  #ggtitle('Total lice') +
  scale_shape_manual(name = 'treatment', values = c(1, 16, 16, 16)) +
  scale_size_manual(name = 'treatment', values = c(3, 3.5, 2, 2)) +
  scale_linetype_manual(name = 'gender', values = c('dashed', 'dotted'), labels = c('\u2640', '\u2642')) +
  scale_colour_manual(name = 'treatment', values = c('black', 'black', 'black', 'grey60')) +
  theme_classic() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14))


  
  
  
# summarise lice numbers by time and group with rep as error and draw plot of total lice-------------
t1summ <- t1data %>%
  group_by(time, tank, treatment, replicate) %>%
  dplyr::summarise(m_m = mean(total.m), sd_m = sd(total.m), m_f = mean(total.f), sd_f = sd(total.f), tot_m = mean(total.m + total.f), tot_sd = sd(total.m + total.f))

t1summ %>%
  group_by(time, treatment) %>%
  dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = total_mean, colour = treatment)) +
  geom_line(size = 1) +
  geom_errorbar(aes(x = time, ymin = total_mean-total_sd, ymax = total_mean+total_sd), width = 3, position = 'dodge', size = 1) +
  scale_y_continuous(limits = c(0, 25), name = 'mean lice per fish') +
  scale_x_continuous(breaks = c(0, 24, 48, 72, 96), labels = c('0', '24', '48', '72', '96'), name = 'Time (h)') +
  ggtitle('Total lice') +
  theme_classic() +
  theme(legend.title = element_blank())

# summarise by time and group with rep as error and draw plot of male lice
t1summ %>%
  group_by(time, treatment) %>%
  dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = mean_m, colour = treatment)) +
  geom_line(size = 1) +
  geom_errorbar(aes(x = time, ymin = mean_m-sd_m, ymax = mean_m+sd_m), width = 3, position = 'dodge', size = 1) +
  scale_y_continuous(limits = c(0, 15), name = 'mean lice per fish') +
  scale_x_continuous(breaks = c(0, 24, 48, 72, 96), labels = c('0', '24', '48', '72', '96'), name = 'Time (h)') +
  ggtitle('Male lice') +
  theme_classic() +
  theme(legend.title = element_blank())


# summarise by time and group with rep as error and draw plot of female lice
t1summ %>%
  group_by(time, treatment) %>%
  dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = mean_f, colour = treatment)) +
  geom_line(size = 1) +
  geom_errorbar(aes(x = time, ymin = mean_f-sd_f, ymax = mean_f+sd_f), width = 3, position = 'dodge', size = 1) +
  scale_y_continuous(limits = c(0, 25), name = 'mean lice per fish') +
  scale_x_continuous(breaks = c(0, 24, 48, 72, 96), labels = c('0', '24', '48', '72', '96'), name = 'Time (h)') +
  ggtitle('Female lice') +
  theme_classic() +
  theme(legend.title = element_blank())