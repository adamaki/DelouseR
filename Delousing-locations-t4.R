# Cleaner fish tank delousing
# Adam Brooker 7th August 2020

source('G:/Projects/Lumpfish delousing/Delousing-initiate.R')
source('/Users/adambrooker/R Projects/DelouseR/Delousing-initiate.R')


# Load and reformat T3 cryptic lice trial ---------------------------------------
setwd('/Users/adambrooker/Dropbox/1-IoA/cleanerfish/Projects/SAIC Lumpfish/Delousing Trials/T4 Winter')


t4data <- read.csv('DelousingTrial4-winter.csv')

t4data$time <- dplyr::recode(t4data$time, '1' = 0, '2' = 48, '3' = 96, '4' = 168)
#t3data$treatment <- dplyr::recode(t3data$treatment, 'Con' = 'Control', 'LC' = 'Lumpfish cryptic', 'LP' = 'Lumpfish pigmented', 'WC' = 'Wrasse cryptic')


t4data <- t4data %>% mutate(total.m = t4data %>% rowwise() %>% select(contains('.m')) %>% rowSums()) # new total male col
t4data <- t4data %>% mutate(total.f = t4data %>% rowwise() %>% select(contains('.f')) %>% rowSums()) # new total female col

t4data$total <- t4data$total.m + t4data$total.f
t4data$tank <- factor(t4data$tank, levels = c('C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C11', 'C12')) # convert ID to factor


# melt data into long format--------------------------------------------------------------------------------------------------
males <- t4data %>% 
  mutate(head.m = dorsal_head.m + mid_head.m + ventral_head.m) %>%
  select(-dorsal_head.m, -mid_head.m, -ventral_head.m) %>%
  select(-group) %>%
  select(-dorsal_head.f:-total) %>%
  melt(id = c('time', 'tank', 'replicate','fish'), value.name = 'male') %>%
  #filter(grepl('.mt', variable, fixed = T)) %>%
  dplyr::rename(location = variable)

females <- t4data %>% 
  mutate(head.f = dorsal_head.f + mid_head.f + ventral_head.f) %>%
  select(-dorsal_head.f, -mid_head.f, -ventral_head.f) %>%
  select(-group) %>%
  select(-dorsal_head.m:-tail.m) %>%
  select(-weight_g:-total) %>%
  melt(id = c('time', 'tank', 'replicate','fish'), value.name = 'female') %>%
  #filter(grepl('.mt', variable, fixed = T)) %>%
  dplyr::rename(location = variable)

t4melt <- bind_cols(males, female = females$female)
t4melt$location <- str_sub(t4melt$location, end = -3) # remove .m from locations
t4melt$location <- as.factor(t4melt$location)
t4melt$time <- as.factor(t4melt$time)
t4melt$location <- factor(t4melt$location, levels(t4melt$location)[c(7, 1, 2, 3, 4, 5, 6, 9, 10, 11, 8)])
t4melt$total <- t4melt$male + t4melt$female
rm(males, females)


# summarise data and barplot No. of lice by time and location----------------------------------------------------------------------
t4locsum <- t4melt %>%
  group_by(time, replicate, location) %>%
  dplyr::summarise(mean.m = mean(male), sd.m = sd(male), mean.f = mean(female), sd.f = sd(female), mean.t = mean(total), sd.t = sd(total)) 

#choose_palette()
fishpal <- c(sequential_hcl(3, h = -349, c = c(83, 50), l = c(34, 81), 0.7)[2], 
             sequential_hcl(3, h = -99, c = c(83, 50), l = c(34, 81), 0.7), 
             sequential_hcl(3, h = -232, c = c(83, 50), l = c(34, 81), 0.7), 
             sequential_hcl(3, h = 61, c = c(83, 50), l = c(34, 81), 0.7), 
             sequential_hcl(3, h = -53, c = c(83, 50), l = c(34, 81), 0.7)[2])


# all lice plot
ggplot(t4locsum, aes(x = time, y = mean.t, fill = location)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_x_discrete(breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)') +
  scale_y_continuous(name = 'mean No. of lice') +
  scale_fill_manual(values = fishpal) +
  theme_classic() +
  facet_wrap(~replicate)

# female plot
ggplot(t4locsum, aes(x = time, y = mean.f, fill = location)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_x_discrete(breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)') +
  scale_y_continuous(name = 'mean No. of female lice') +
  scale_fill_manual(values = fishpal) +
  theme_classic() +
  facet_wrap(~replicate)

# male plot
ggplot(t4locsum, aes(x = time, y = mean.m, fill = location)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_x_discrete(breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)') +
  scale_y_continuous(name = 'mean No. of male lice') +
  scale_fill_manual(values = fishpal) +
  theme_classic() +
  facet_wrap(~replicate)


# summarise data and do lineplots--------------------------------------------------------------------------------
t4summ <- t4data %>%
  #select(-dorsal_head.mc:-length_mm)
  group_by(time, tank, group, replicate) %>%
  dplyr::summarise(m_m = mean(total.m), sd_m = sd(total.m), m_f = mean(total.f), sd_f = sd(total.f), tot_m = mean(total.m + total.f), tot_sd = sd(total.m + total.f))

t4summ$tank <- factor(t4summ$tank, levels = c('C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C11', 'C12')) # convert ID to factor
t4summ$replicate <- as.factor(t4summ$replicate)

# summarise by time and group with rep as error and draw plot of total lice
totliceplot <- t4summ %>%
  group_by(time, group) %>%
  dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = total_mean, colour = group)) +
  geom_line(size = 1) +
  geom_errorbar(aes(x = time, ymin = total_mean-total_sd, ymax = total_mean+total_sd), width = 3, position = 'dodge', size = 1) +
  scale_y_continuous(limits = c(-0.01, 12), name = 'mean lice per fish', expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 170), breaks = c(0, 48, 96, 168), labels = c('0', '48', '96', '168'), name = 'Time (h)', expand = c(0, 0)) +
  ggtitle('Total lice') +
  theme_classic() #+
#theme(legend.title = element_blank())

# summarise by time and group with rep as error and draw plot of male lice
maleliceplot <- t4summ %>%
  group_by(time, group) %>%
  dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = mean_m, colour = group)) +
  geom_line(size = 1) +
  geom_errorbar(aes(x = time, ymin = mean_m-sd_m, ymax = mean_m+sd_m), width = 3, position = 'dodge', size = 1) +
  scale_y_continuous(limits = c(-0.01, 6), name = 'mean lice per fish', expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 170), breaks = c(0, 48, 96, 168), labels = c('0', '48', '96', '168'), name = 'Time (h)', expand = c(0, 0)) +
  ggtitle('Male lice') +
  theme_classic() #+

femaleliceplot <- t4summ %>%
  group_by(time, group) %>%
  dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = mean_f, colour = group)) +
  geom_line(size = 1) +
  geom_errorbar(aes(x = time, ymin = mean_f-sd_f, ymax = mean_f+sd_f), width = 3, position = 'dodge', size = 1) +
  scale_y_continuous(limits = c(-0.01, 7), name = 'mean lice per fish', expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 170), breaks = c(0, 48, 96, 168), labels = c('0', '48', '96', '168'), name = 'Time (h)', expand = c(0, 0)) +
  ggtitle('Female lice') +
  theme_classic() #+



