# Cleaner fish tank delousing
# Adam Brooker 26th August 2020

source('G:/Projects/Lumpfish delousing/Delousing-initiate.R')
source('/Users/adambrooker/R Projects/DelouseR/Delousing-initiate.R')


# Load and reformat T5 cryptic lice trial ---------------------------------------
setwd('/Users/adambrooker/Dropbox/1-IoA/cleanerfish/Projects/SAIC Lumpfish/Delousing Trials/T5 Individual Lumpfish')


t5data <- read.csv('DelousingTrial5-IndLumpfish.csv')
t5data <- select(t5data, -weight_g, -length_mm)

t5data$time <- dplyr::recode(t5data$time, '1' = 0, '2' = 48, '3' = 96, '4' = 144)
#t3data$treatment <- dplyr::recode(t3data$treatment, 'Con' = 'Control', 'LC' = 'Lumpfish cryptic', 'LP' = 'Lumpfish pigmented', 'WC' = 'Wrasse cryptic')


t5data <- t5data %>% mutate(total.m = t5data %>% rowwise() %>% select(contains('.m')) %>% rowSums()) # new total male col
t5data <- t5data %>% mutate(total.f = t5data %>% rowwise() %>% select(contains('.f')) %>% rowSums()) # new total female col

t5data$total <- t5data$total.m + t5data$total.f
t5data <- filter(t5data, tank != 'C8') # remove C8 as lumpfish died
t5data$tank <- factor(t5data$tank, levels = c('C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C9', 'C10', 'C11', 'C12')) # convert ID to factor

# stats---------------------------------------------------------------------

# lice start levels
t5data %>%
  group_by(time) %>%
  dplyr::summarise(male.mean = mean(total.m), male.sd = sd(total.m), female.mean = mean(total.f), female.sd = sd(total.f), total.mean = mean(total.m + total.f), total.sd = sd(total.m +total.f)) %>%
  filter(time == '0')

# lice percent reduction
t5data %>%
  group_by(time, tank) %>%
  dplyr::summarise(mean.m = mean(total.m), mean.f = mean(total.f), mean.t = mean(total)) %>%
  filter(time == '0' | time == '144') %>%
  pivot_wider(names_from = time, values_from = c(mean.m, mean.f, mean.t)) %>%
  dplyr::mutate(reduc.m = ((mean.m_0 - mean.m_144)/mean.m_0)*100, reduc.f = ((mean.f_0 - mean.f_144)/mean.f_0)*100, reduc.t = ((mean.t_0 - mean.t_144)/mean.t_0)*100)

# summarise data and do lineplots--------------------------------------------------------------------------------

t5summ <- t5data %>%
  #select(-dorsal_head.mc:-length_mm)
  group_by(time, tank) %>%
  dplyr::summarise(m_m = mean(total.m), sd_m = sd(total.m), m_f = mean(total.f), sd_f = sd(total.f), tot_m = mean(total.m + total.f), tot_sd = sd(total.m + total.f))

t5summ$tank <- factor(t5summ$tank, levels = c('C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C9', 'C10', 'C11', 'C12')) # convert ID to factor

# summarise by time and group with rep as error and draw plot of total lice
totliceplot <- t5summ %>%
  #group_by(time, replicate) %>%
  #dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = tot_m, colour = tank)) +
  geom_line(size = 1) +
  geom_errorbar(aes(x = time, ymin = tot_m-tot_sd, ymax = tot_m+tot_sd), width = 3, position = 'dodge', size = 1) +
  scale_y_continuous(limits = c(0, 21), name = 'mean lice per fish', expand = c(0, 0)) +
  scale_x_continuous(breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)', expand = c(0, 0)) +
  ggtitle('Total lice') +
  theme_classic() #+
#theme(legend.title = element_blank())

# summarise by time and group with rep as error and draw plot of male lice
totmaleplot <- t5summ %>%
  #group_by(time, treatment) %>%
  #dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = m_m, colour = tank)) +
  geom_line(size = 1) +
  geom_errorbar(aes(x = time, ymin = m_m-sd_m, ymax = m_m+sd_m), width = 3, position = 'dodge', size = 1) +
  scale_y_continuous(limits = c(0, 12), name = 'mean lice per fish', expand = c(0, 0)) +
  scale_x_continuous(breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)', expand = c(0, 0)) +
  ggtitle('Male lice') +
  theme_classic() #+
#theme(legend.title = element_blank())


# summarise by time and group with rep as error and draw plot of female lice
totfemaleplot <- t5summ %>%
  #dplyr::filter(tank == 'C7' | tank == 'C8' | tank == 'C9') %>%
  #group_by(time, treatment) %>%
  #dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = m_f, colour = tank)) +
  geom_line(size = 1) +
  geom_errorbar(aes(x = time, ymin = m_f-sd_f, ymax = m_f+sd_f), width = 3, position = 'dodge', size = 1) +
  scale_y_continuous(limits = c(-1, 16), name = 'mean lice per fish', expand = c(0, 0)) +
  scale_x_continuous(breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)', expand = c(0, 0)) +
  ggtitle('Female lice') +
  theme_classic() +
  coord_cartesian(ylim = c(0, 16))
#theme(legend.title = element_blank())

# summarise % decrease in lice by time with variation between salmon as standard error---------------------------------------

t5means <- t5data %>%
  group_by(time, tank) %>%
  dplyr::summarise(mean.m = mean(total.m), mean.f = mean(total.f), mean.t = mean(total.m + total.f))

s.means <- filter(t5means, time == 0) %>%
  ungroup() %>%
  select(-'time')
colnames(s.means) <- c('tank', 'start.m', 'start.f', 'start.t')

t5data <- t5data %>%
  left_join(s.means, by = c('tank'))
rm(s.means)

t5data <- t5data %>%
  dplyr::mutate(diff.m = (total.m/start.m)*100, diff.f = (total.f/start.f)*100, diff.t = (total/start.t)*100)

t5dec.t <- t5data %>%
  group_by(time, tank) %>%
  dplyr::summarise(mean_t = mean(diff.m), sd_t = sd(diff.m)/sqrt(10), mean_f = mean(diff.f), sd_f = sd(diff.f)/sqrt(10), mean_t = mean(diff.t), sd_t = sd(diff.t)/sqrt(10)) %>% # standard error
  ggplot(aes(x = time, colour = tank)) +
  geom_line(aes(y = mean_t), size = 0.5) +
  geom_point(aes(y = mean_t)) +
  geom_errorbar(aes(ymin = mean_t-sd_t, ymax = mean_t+sd_t), width = 3, size = 0.5, position = 'dodge') +
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 130, 10), name = 'No. of lice (%)', expand = c(0, 0)) +
  scale_x_continuous(limits = c(-2, 150), breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)', expand = c(0, 0)) +
  theme_classic() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14)) +
  ggtitle('Total lice')

t5dec.m <- t5data %>%
  group_by(time, tank) %>%
  dplyr::summarise(mean_m = mean(diff.m), sd_m = sd(diff.m)/sqrt(10), mean_f = mean(diff.f), sd_f = sd(diff.f)/sqrt(10), mean_t = mean(diff.t), sd_t = sd(diff.t)/sqrt(10)) %>% # standard error
  ggplot(aes(x = time, colour = tank)) +
  geom_line(aes(y = mean_m), size = 0.5) +
  geom_point(aes(y = mean_m)) +
  geom_errorbar(aes(ymin = mean_m-sd_m, ymax = mean_m+sd_m), width = 3, size = 0.5, position = 'dodge') +
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 140, 10), name = 'No. of lice (%)', expand = c(0, 0)) +
  scale_x_continuous(limits = c(-2, 150), breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)', expand = c(0, 0)) +
  theme_classic() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14)) +
  ggtitle('Male lice')

t5dec.f <- t5data %>%
  group_by(time, tank) %>%
  dplyr::summarise(mean_f = mean(diff.m), sd_f = sd(diff.m)/sqrt(10), mean_f = mean(diff.f), sd_f = sd(diff.f)/sqrt(10), mean_t = mean(diff.t), sd_t = sd(diff.t)/sqrt(10)) %>% # standard error
  ggplot(aes(x = time, colour = tank)) +
  geom_line(aes(y = mean_f), size = 0.5) +
  geom_point(aes(y = mean_f)) +
  geom_errorbar(aes(ymin = mean_f-sd_f, ymax = mean_f+sd_f), width = 3, size = 0.5, position = 'dodge') +
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 130, 10), name = 'No. of lice (%)', expand = c(0, 0)) +
  scale_x_continuous(limits = c(-2, 150), breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)', expand = c(0, 0)) +
  theme_classic() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14)) +
  ggtitle('Female lice')



# Delousing rates----------------------------------------------------------------------------------------------

# create summary dataset of T0 mean lice counts
means <- t5data %>%
  group_by(time, tank, group) %>%
  dplyr::summarise(start.m = mean(total.m), start.f = mean(total.f), start.t = mean(total.m + total.f)) %>%
  filter(time == '0')

t5data <- left_join(t5data, means[,c(2, 4:6)], by = 'tank') # Join mean T0 totals to raw dataset to compare delousing rate

t5data <- t5data %>% mutate(diff.m = (total.m/start.m)*100, diff.f = (total.f/start.f)*100, diff.t = (total/start.t)*100) # calculate delousing rate columns based on tank mean at T0

# plot treatments by daily delousing rate histogram---------------------
t5data$dprop.m <- cut(t5data$diff.m, breaks = c(-1, seq(10, 90, 10), 800), labels = (c('0-10', '11-20', '21-30', '31-40', '41-50', '51-60', '61-70', '71-80', '81-90', '91-100')))
t5data$dprop.f <- cut(t5data$diff.f, breaks = c(-1, seq(10, 90, 10), 800), labels = (c('0-10', '11-20', '21-30', '31-40', '41-50', '51-60', '61-70', '71-80', '81-90', '91-100')))
t5data$dprop.t <- cut(t5data$diff.t, breaks = c(-1, seq(10, 90, 10), 800), labels = (c('0-10', '11-20', '21-30', '31-40', '41-50', '51-60', '61-70', '71-80', '81-90', '91-100')))
#t5data$dprop.t <- cut(t5data$dec.t, breaks = c(-1, seq(10, 90, 10), 800), labels = (c('91-100%', '81-90%', '71-80%', '61-70%', '51-60%', '41-50%', '31-40%', '21-30%', '11-20%', '0-10%')))

t5data$time <- as.factor(t5data$time)
delousepal <- rev(sequential_hcl(10, h = 245, c = c(81, 14), l = c(34, 93), 1.1))

dplot.f <- t5data %>%
  #filter(time != 0) %>%
  ggplot(aes(x = time, fill = dprop.f)) +
  geom_bar(position = 'fill', stat = 'count') +
  scale_x_discrete(name = 'Time (h)', expand = c(0, 0)) +
  scale_y_continuous(name = 'Proportion', expand = c(0, 0)) +
  facet_wrap(~tank, nrow = 2, ncol = 6) +
  scale_fill_manual(values = delousepal, name = '% female lice \n remaining') +
  theme_classic() +
  ggtitle('Delousing rates - female lice')

dplot.m <- t5data %>%
  #filter(time != 0) %>%
  ggplot(aes(x = time, fill = dprop.m)) +
  geom_bar(position = 'fill', stat = 'count') +
  scale_x_discrete(name = 'Time (h)', expand = c(0, 0)) +
  scale_y_continuous(name = 'Proportion', expand = c(0, 0)) +
  facet_wrap(~tank, nrow = 2, ncol = 6) +
  scale_fill_manual(values = delousepal, name = '% male lice \n remaining') +
  theme_classic() +
  ggtitle('Delousing rates - male lice')

dplot.t <- t5data %>%
  #filter(time != 0) %>%
  ggplot(aes(x = time, fill = dprop.t)) +
  geom_bar(position = 'fill', stat = 'count') +
  scale_x_discrete(name = 'Time (h)', expand = c(0, 0)) +
  scale_y_continuous(name = 'Proportion', expand = c(0, 0)) +
  facet_wrap(~tank, nrow = 2, ncol = 6) +
  scale_fill_manual(values = delousepal, name = '% all lice \n remaining') +
  theme_classic() +
  ggtitle('Delousing rates - all lice')


