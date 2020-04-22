# Cleaner fish tank delousing
# Adam Brooker 20th April 2020

source('/Users/adambrooker/R Projects/DelouseR/Delousing-initiate.R')

# Load trial 1 data and reformat---------------------------------
t1data <- read.csv('/Users/adambrooker/Dropbox/1-IoA/cleanerfish/Projects/SAIC Lumpfish/Delousing Trials/T1 Summer/t1summerdata.csv')

t1data$time <- dplyr::recode(t1data$time, '1' = 0, '2' = 24, '3' = 48, '4' = 72, '5' = 96)
t1data$treatment <- dplyr::recode(t1data$treatment, 'con' = 'Control', 'LL' = 'Large lumpfish', 'SL' = 'Small lumpfish', 'W' = 'Wrasse')

t1data <- t1data %>% mutate(total.m = t1data %>% rowwise() %>% select(contains('.m')) %>% rowSums()) # new total male col
t1data <- t1data %>% mutate(total.f = t1data %>% rowwise() %>% select(contains('.f')) %>% rowSums()) # new total female col
t1data$total <- t1data$total.m + t1data$total.f

t1data <- arrange(t1data, time, treatment, replicate) # arrange data

# code to select 10 fish per tank from final 30-fish sample

t1data.s <- t1data %>%
  group_by(time, tank) %>%
  sample_n(10) %>%
  arrange(time, treatment, replicate, fish) %>%
  mutate(fish = seq(1, 10, 1))

t1data.s <- t1data %>%
  group_by(time, tank) %>%
  arrange(total) %>%
  slice(1:10) %>%
  arrange(time, treatment, replicate, fish) %>%
  mutate(fish = seq(1, 10, 1))


# Load trial 2 data and reformat----------------------------------
t2data <- read.csv('/Users/adambrooker/Dropbox/1-IoA/cleanerfish/Projects/SAIC Lumpfish/Delousing Trials/T2 Cryptic/t2crypticdata.csv')

t2data$time <- dplyr::recode(t2data$time, '0' = 0, '1' = 48, '2' = 96, '3' = 168)
t2data$treatment <- dplyr::recode(t2data$treatment, 'Con' = 'Control', 'LC' = 'Lumpfish cryptic', 'LP' = 'Lumpfish pigmented', 'WC' = 'Wrasse cryptic')

# Create total lice at each location columns--------------------

t2data <- t2data %>%
  mutate(dorsal_head.mt = dorsal_head.mc + dorsal_head.mp) %>%
  mutate(mid_head.mt = mid_head.mc + mid_head.mp) %>%
  mutate(ventral_head.mt = ventral_head.mc + ventral_head.mp) %>%
  mutate(dorsal_front.mt = dorsal_front.mc + dorsal_front.mp) %>%
  mutate(dorsal_mid.mt = dorsal_mid.mc + dorsal_mid.mp) %>%
  mutate(dorsal_rear.mt = dorsal_rear.mc + dorsal_rear.mp) %>%
  mutate(flank_front.mt = flank_front.mc + flank_front.mp) %>%
  mutate(flank_mid.mt = flank_mid.mc + flank_mid.mp) %>%
  mutate(flank_rear.mt = flank_rear.mc + flank_rear.mp) %>%
  mutate(ventral_front.mt = ventral_front.mc + ventral_front.mp) %>%
  mutate(ventral_mid.mt = ventral_mid.mc + ventral_mid.mp) %>%
  mutate(ventral_rear.mt = ventral_rear.mc + ventral_rear.mp) %>%
  mutate(tail.mt = tail.mc + tail.mp) %>%
  mutate(dorsal_head.ft = dorsal_head.fc + dorsal_head.fp) %>%
  mutate(mid_head.ft = mid_head.fc + mid_head.fp) %>%
  mutate(ventral_head.ft = ventral_head.fc + ventral_head.fp) %>%
  mutate(dorsal_front.ft = dorsal_front.fc + dorsal_front.fp) %>%
  mutate(dorsal_mid.ft = dorsal_mid.fc + dorsal_mid.fp) %>%
  mutate(dorsal_rear.ft = dorsal_rear.fc + dorsal_rear.fp) %>%
  mutate(flank_front.ft = flank_front.fc + flank_front.fp) %>%
  mutate(flank_mid.ft = flank_mid.fc + flank_mid.fp) %>%
  mutate(flank_rear.ft = flank_rear.fc + flank_rear.fp) %>%
  mutate(ventral_front.ft = ventral_front.fc + ventral_front.fp) %>%
  mutate(ventral_mid.ft = ventral_mid.fc + ventral_mid.fp) %>%
  mutate(ventral_rear.ft = ventral_rear.fc + ventral_rear.fp) %>%
  mutate(tail.ft = tail.fc + tail.fp)


t2data <- t2data %>% mutate(total.mc = t2data %>% rowwise() %>% select(contains('.mc')) %>% rowSums()) # new total cryptic male col
t2data <- t2data %>% mutate(total.fc = t2data %>% rowwise() %>% select(contains('.fc')) %>% rowSums()) # new total cryptic female col
t2data <- t2data %>% mutate(total.mp = t2data %>% rowwise() %>% select(contains('.mp')) %>% rowSums()) # new total pigmented male col
t2data <- t2data %>% mutate(total.fp = t2data %>% rowwise() %>% select(contains('.fp')) %>% rowSums()) # new total pigmented female col

t2data$total.m <- t2data$total.mc + t2data$total.mp
t2data$total.f <- t2data$total.fc + t2data$total.fp
t2data$total <- t2data$total.m + t2data$total.f

t2data <- arrange(t2data, time, treatment, replicate) # arrange data



# Figure 1. summarise % decrease in lice by time and group with rep as error and draw plot of total lice-----------

# trial 1 plot
t1means <- t1data.s %>%
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
  #t1means %>%
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
  scale_y_continuous(limits = c(0, 170), breaks = seq(0, 180, 10), name = 'No. of lice (%)', expand = c(0, 0)) +
  scale_x_continuous(breaks = c(0, 24, 48, 72, 96), labels = c('0', '24', '48', '72', '96'), name = 'Time (h)', expand = c(0, 0)) +
  #ggtitle('Total lice') +
  scale_shape_manual(name = 'treatment', values = c(0, 16, 16, 17)) +
  scale_size_manual(name = 'treatment', values = c(3, 3.5, 2, 3)) +
  scale_linetype_manual(name = 'gender', values = c('dashed', 'dotted'), labels = c('\u2640', '\u2642')) +
  scale_colour_manual(name = 'treatment', values = c('black', 'black', 'black', 'black')) +
  theme_classic() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14))

#trial 2 plot
t2means <- t2data %>%
  group_by(time, tank, treatment, replicate) %>%
  dplyr::summarise(mean.m = mean(total.m), mean.f = mean(total.f), mean.t = mean(total.m + total.f))

s.means <- filter(t2means, time == 0) %>%
  ungroup() %>%
  select(-'time')
colnames(s.means) <- c('tank', 'treatment', 'replicate', 'start.m', 'start.f', 'start.t')

t2means <- t2means %>%
  left_join(s.means, by = c('tank', 'treatment', 'replicate'))
rm(s.means)

t2means <- t2means %>%
  dplyr::mutate(diff.m = (mean.m/start.m)*100, diff.f = (mean.f/start.f)*100, diff.t = (mean.t/start.t)*100)

t2dec.p <- t2means %>%
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
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 130, 10), name = 'No. of lice (%)', expand = c(0, 0)) +
  scale_x_continuous(breaks = c(0, 48, 96, 168), labels = c('0', '48', '96', '168'), name = 'Time (h)', expand = c(0, 0)) +
  #ggtitle('Total lice') +
  scale_shape_manual(name = 'treatment', values = c(0, 1, 16, 2)) +
  scale_size_manual(name = 'treatment', values = c(3.5, 3, 3.5, 3)) +
  scale_linetype_manual(name = 'gender', values = c('dashed', 'dotted'), labels = c('\u2640', '\u2642')) +
  scale_colour_manual(name = 'treatment', values = c('black', 'black', 'black', 'black')) +
  theme_classic() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14))

#trial 3 plot

t3dec.p <- #t3means %>%
  #group_by(time, treatment) %>%
  #  dplyr::summarise(mean_m = mean(diff.m), sd_m = sd(diff.m), mean_f = mean(diff.f), sd_f = sd(diff.f), total_mean = mean(diff.t), total_sd = sd(diff.t)) %>% # standard deviation
  #dplyr::summarise(mean_m = mean(diff.m), sd_m = sd(diff.m)/sqrt(3), mean_f = mean(diff.f), sd_f = sd(diff.f)/sqrt(3)) %>% # standard error
  #gather(key = v, value = value, mean_m:sd_f) %>%
  #separate(col = v, into = c('stat', 'gender')) %>%
  #arrange(time) %>%
  #spread(stat, value) %>%
  #ggplot(aes(x = time, colour = treatment)) +
  ggplot() +
  #geom_line(aes(y = mean, linetype = gender), size = 0.5) +
  #geom_point(aes(y = mean, shape = treatment, size = treatment)) +
  #geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 3, size = 0.5, position = 'dodge') +
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 130, 10), name = 'No. of lice (%)', expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 168), breaks = c(0, 48, 96, 168), labels = c('0', '48', '96', '168'), name = 'Time (h)', expand = c(0, 0)) +
  #ggtitle('Total lice') +
  scale_shape_manual(name = 'treatment', values = c(0, 1, 16, 2)) +
  scale_size_manual(name = 'treatment', values = c(3.5, 3, 3.5, 3)) +
  scale_linetype_manual(name = 'gender', values = c('dashed', 'dotted'), labels = c('\u2640', '\u2642')) +
  scale_colour_manual(name = 'treatment', values = c('black', 'black', 'black', 'black')) +
  theme_classic() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14))

plot_grid(t1dec.p, t2dec.p, t3dec.p, labels = c('(a)', '(b)', '(c)'), hjust = c(-3.5, -3.5, -3.5), vjust = c(2, 2, 2))

  
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