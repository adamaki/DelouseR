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
#random slice
t1data.s <- t1data %>%
  group_by(time, tank) %>%
  sample_n(10) %>%
  arrange(time, treatment, replicate, fish) %>%
  mutate(fish = seq(1, 10, 1))

#top slice
t1data.s <- t1data %>%
  group_by(time, tank) %>%
  arrange(total) %>%
  slice(1:10) %>%
  arrange(time, treatment, replicate, fish) %>%
  mutate(fish = seq(1, 10, 1))

#middle slice
t1data.s <- t1data %>%
  filter(time == 96 & treatment != 'Control') %>%
  group_by(time, tank) %>%
  arrange(total) %>%
  slice(11:20) %>%
  mutate(fish = seq(1, 10, 1))
t1data.s <- t1data %>%
  filter(time != 96 | treatment == 'Control') %>%
  bind_rows(t1data.s) %>%
  arrange(time, treatment, replicate, fish)
  
#select slice
t1data.s <- t1data %>%
  filter(time == 96 & treatment != 'Control') %>%
  group_by(time, tank) %>%
  arrange(total) %>%
  slice(c(1:3, 6, 8, 12, 15, 19, 22, 27)) %>%
  mutate(fish = seq(1, 10, 1))
t1data.s <- t1data %>%
  filter(time != 96 | treatment == 'Control') %>%
  bind_rows(t1data.s) %>%
  arrange(time, treatment, replicate, fish)


# Load trial 2 data and reformat----------------------------------
t2data <- read.csv('/Users/adambrooker/Dropbox/1-IoA/cleanerfish/Projects/SAIC Lumpfish/Delousing Trials/T2 Cryptic/t2crypticdata.csv')

t2data$time <- dplyr::recode(t2data$time, '0' = 0, '1' = 48, '2' = 96, '3' = 168)
t2data$treatment <- dplyr::recode(t2data$treatment, 'Con' = 'Control', 'LC' = 'Lumpfish cryptic', 'LP' = 'Lumpfish pigmented', 'WC' = 'Wrasse cryptic')

# Create total lice at each location columns

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
t2data$treatment <- factor(t2data$treatment, levels(t2data$treatment)[c(1, 3, 2, 4)]) # change treatment order

# Load trial 4 data and reformat---------------------------------
t4data <- read.csv('/Users/adambrooker/Dropbox/1-IoA/cleanerfish/Projects/SAIC Lumpfish/Delousing Trials/T4 Winter/DelousingTrial4-winter.csv')

t4data$time <- dplyr::recode(t4data$time, '1' = 0, '2' = 48, '3' = 96, '4' = 168)
t4data$treatment <- dplyr::recode(t4data$treatment, 'Con' = 'Control', 'L' = 'Lumpfish', 'W' = 'Wrasse')
t4data <- select(t4data, -weight_g, length_mm)

t4data <- t4data %>% mutate(total.m = t4data %>% rowwise() %>% select(contains('.m')) %>% rowSums()) # new total male col
t4data <- t4data %>% mutate(total.f = t4data %>% rowwise() %>% select(contains('.f')) %>% rowSums()) # new total female col
t4data$total <- t4data$total.m + t4data$total.f

t4data <- arrange(t4data, time, treatment, replicate) # arrange data



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
  scale_y_continuous(limits = c(0, 170), breaks = seq(0, 170, 10), name = 'No. of lice (%)', expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 24, 48, 72, 96), labels = c('0', '24', '48', '72', '96'), name = 'Time (h)', expand = c(0, 0)) +
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
  scale_x_continuous(limits = c(0, 172), breaks = c(0, 48, 96, 172), labels = c('0', '48', '96', '168'), name = 'Time (h)', expand = c(0, 0)) +
  #ggtitle('Total lice') +
  scale_shape_manual(name = 'treatment', values = c(0, 1, 16, 2)) +
  scale_size_manual(name = 'treatment', values = c(3.5, 3, 3.5, 3)) +
  scale_linetype_manual(name = 'gender', values = c('dashed', 'dotted'), labels = c('\u2640', '\u2642')) +
  scale_colour_manual(name = 'treatment', values = c('black', 'black', 'black', 'black')) +
  theme_classic() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14))

#trial 4 plot
t4means <- t4data %>%
  group_by(time, tank, treatment, replicate) %>%
  dplyr::summarise(mean.m = mean(total.m), mean.f = mean(total.f), mean.t = mean(total.m + total.f))

s.means <- filter(t4means, time == 0) %>% # extract df of mean lice at start of trial
  ungroup() %>%
  select(-'time')
colnames(s.means) <- c('tank', 'treatment', 'replicate', 'start.m', 'start.f', 'start.t')

t4means <- t4means %>% # Add new columns of starting mean values to df
  left_join(s.means, by = c('tank', 'treatment', 'replicate'))
rm(s.means)

t4means <- t4means %>% # calculate columns of % change in lice since start
  dplyr::mutate(diff.m = (mean.m/start.m)*100, diff.f = (mean.f/start.f)*100, diff.t = (mean.t/start.t)*100)

t4dec.p <- t4means %>%
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
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 140, 10), name = 'No. of lice (%)', expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 172), breaks = c(0, 48, 96, 168), labels = c('0', '48', '96', '168'), name = 'Time (h)', expand = c(0, 0)) +
  #ggtitle('Total lice') +
  scale_shape_manual(name = 'treatment', values = c(0, 1, 16, 2)) +
  scale_size_manual(name = 'treatment', values = c(3.5, 3, 3.5, 3)) +
  scale_linetype_manual(name = 'gender', values = c('dashed', 'dotted'), labels = c('\u2640', '\u2642')) +
  scale_colour_manual(name = 'treatment', values = c('black', 'black', 'black', 'black')) +
  theme_classic() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14))

plot_grid(t1dec.p, t2dec.p, t4dec.p, labels = c('(a)', '(b)', '(c)'), hjust = c(-3.5, -3.5, -3.5), vjust = c(2, 2, 2))

  
# Wrasse plot for Otter Ferry, just females//////////////////////////////////////////////////////////////

# calculate percent means for t3 data already loaded
t3means <- t3data %>%
  group_by(time, tank, group, replicate) %>%
  dplyr::summarise(mean.m = mean(total.m), mean.f = mean(total.f), mean.t = mean(total.m + total.f))

s.means <- filter(t3means, time == 0) %>%
  ungroup() %>%
  select(-'time')
colnames(s.means) <- c('tank', 'group', 'replicate', 'start.m', 'start.f', 'start.t')

t3means <- t3means %>%
  left_join(s.means, by = c('tank', 'group', 'replicate'))
rm(s.means)

t3means <- t3means %>%
  dplyr::mutate(diff.m = (mean.m/start.m)*100, diff.f = (mean.f/start.f)*100, diff.t = (mean.t/start.t)*100)  

t3means <- t3means %>% rename(treatment = group)
t3means$treatment <- 'Individual wrasse'

t1means$trial <- 'trial1-summer'
t2means$trial <- 'trial2-cryptic'
t3means$trial <- 'trial3-individual'

# combine wrasse means from all trials
wrassemeans <- bind_rows(t1means, t2means, t3means) %>%
  select(trial, time, tank, treatment, replicate, mean.m, mean.f, mean.t) %>%
  filter(treatment == 'Wrasse' | treatment == 'Wrasse cryptic' | treatment == 'Individual wrasse' | treatment == 'Control')

wrassesummary <- wrassemeans %>%
  group_by(trial, time, treatment) %>% 
  summarise(males.m = mean(mean.m), males.sd = sd(mean.m), males.se = sd(mean.m)/sqrt(n()), 
            females.m = mean(mean.f), females.sd = sd(mean.f), females.se = sd(mean.f)/sqrt(n()), 
            total.m = mean(mean.t), total.sd = sd(mean.t), total.se = sd(mean.t)/sqrt(n()))

write.csv(wrassesummary, 'wrassesummary.csv')

wrassemeans$treatment <- dplyr::recode(wrassemeans$treatment, 'Wrasse' = 'Summer trial', 'Wrasse cryptic' = 'Cryptic trial', 'Individual wrasse' = 'Individual trial')

wplot <- wrassemeans %>%
  group_by(time, treatment) %>%
  #  dplyr::summarise(mean_m = mean(diff.m), sd_m = sd(diff.m), mean_f = mean(diff.f), sd_f = sd(diff.f), total_mean = mean(diff.t), total_sd = sd(diff.t)) %>% # standard deviation
  dplyr::summarise(mean_m = mean(diff.m), sd_m = sd(diff.m)/sqrt(3), mean_f = mean(diff.f), sd_f = sd(diff.f)/sqrt(3)) %>% # standard error
  filter(treatment == 'Summer trial') %>%
  gather(key = v, value = value, mean_m:sd_f) %>%
  separate(col = v, into = c('stat', 'gender')) %>%
  arrange(time) %>%
  spread(stat, value) %>%
  filter(gender == 'f') %>%
  ggplot(aes(x = time, colour = treatment)) +
  #geom_line(aes(y = mean, linetype = gender), size = 0.5) +
  geom_line(aes(y = mean), size = 0.5) +
  geom_point(aes(y = mean, shape = treatment, size = treatment)) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 3, size = 0.5) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 10), name = 'No. of lice (%)', expand = c(0, 0)) +
  #scale_x_continuous(limits = c(0, 150), breaks = c(0, 24, 48, 72, 96, 120, 144), labels = c('0', '24', '48', '72', '96' ,'120', '144'), name = 'Time (h)', expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 24, 48, 72, 96), labels = c('0', '24', '48', '72', '96'), name = 'Time (h)', expand = c(0, 0)) +
  #ggtitle('Total lice') +
  scale_shape_manual(name = 'treatment', values = c(0, 1, 17)) +
  scale_size_manual(name = 'treatment', values = c(3, 3, 3)) +
  #scale_linetype_manual(name = 'gender', values = c('dashed', 'dotted'), labels = c('\u2640', '\u2642')) +
  scale_colour_manual(name = 'treatment', values = c('black', 'black', 'black')) +
  theme_classic() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  theme(legend.position = 'none') +
#  labs(title = 'Ballan wrasse delousing rates (adult female lice) in tanks',
#       caption = 'Summer trial conducted in August 2019, n = 3 tanks, 30 salmon and 3 wrasse per tank (10% stocking ratio), \nmean salmon weight = 1200g, mean wrasse weight = 25g, mean 13.23 female lice per fish.
#Cryptic trial conducted in October 2019, n = 3 tanks, 30 salmon and 3 wrasse per tank (10% stocking ratio) \nmean salmon weight = 452g, mean wrasse weight = 25g, mean 4.16 female lice per fish. 
#Invidiual trial conducted in February 2020, n = 10 tanks, 10 salmon and 1 wrasse per tank (10% stocking ratio) \nmean salmon weight = 588g, mean wrasse weight 25g, mean 3.49 female lice per fish, bars = SD of tank means.')
labs(title = 'Ballan wrasse delousing rates (adult female lice) in tanks',
     caption = 'Trial conducted in August 2019, n = 3 tanks, 30 salmon and 3 wrasse per tank (10% stocking ratio), \nmean salmon weight = 1200g, mean wrasse weight = 25g, mean 13.23 female lice per fish, bars = SD of tank means.')


#///////////////////////////////////////////////////////////////////////////////////


# Figure 2. Delousing rates------------------------------------------

# Trial 1 summer
means <- t1data.s %>%
  group_by(time, tank, treatment, replicate) %>%
  dplyr::summarise(start.m = mean(total.m), start.f = mean(total.f), start.t = mean(total.m + total.f)) %>%
  filter(time == '0')

t1dec <- left_join(t1data.s, means[,c(2, 5:7)], by = 'tank') # Join mean T0 totals to raw dataset to compare delousing rate
t1dec <- t1dec %>% mutate(dec.m = (total.m/start.m)*100, dec.f = (total.f/start.f)*100, dec.t = (total/start.t)*100) # calculate delousing rate columns based on tank mean at T0

t1dec$dprop.t <- cut(t1dec$dec.t, breaks = c(-1, seq(10, 100, 10), 800), 
                      labels = (c('0-10%', '10-20%', '20-30%', '30-40%', '40-50%', '50-60%', '60-70%', '70-80%', '80-90%', '90-100%', '100%+')))

t1dec$time <- as.factor(t1dec$time)
#choose_palette()
#delousepal <- rev(sequential_hcl(11, h = 245, c = c(81, 14), l = c(34, 93), 1.1)) # blue palette
delousepal <- rev(sequential_hcl(11, h = 245, c = c(0, 0), l = c(17, 93), 1.1)) # grey palette

dplot.t1 <- t1dec %>%
  filter(treatment != 'Control') %>%
  ggplot(aes(x = time, fill = dprop.t)) +
  geom_bar(position = 'fill', stat = 'count') +
  scale_x_discrete(name = 'Time (h)', expand = c(0, 0)) +
  scale_y_continuous(name = 'Proportion of salmon', labels = scales::percent, expand = c(0, 0)) +
  facet_wrap(~treatment) +
  scale_fill_manual(values = delousepal, name = 'Lice per fish') +
  theme_classic() +
  theme(strip.background = element_rect(color = 'white', fill = "white"),
        text = element_text(size = 14))

# Trial 2 cryptic
means <- t2data %>%
  group_by(time, tank, treatment, replicate) %>%
  dplyr::summarise(start.m = mean(total.m), start.f = mean(total.f), start.t = mean(total.m + total.f)) %>%
  filter(time == '0')

t2data <- left_join(t2data, means[,c(2, 5:7)], by = 'tank') # Join mean T0 totals to raw dataset to compare delousing rate
t2data <- t2data %>% mutate(dec.m = (total.m/start.m)*100, dec.f = (total.f/start.f)*100, dec.t = (total/start.t)*100) # calculate delousing rate columns based on tank mean at T0

t2data$dprop.t <- cut(t2data$dec.t, breaks = c(-1, seq(10, 100, 10), 800), 
                      labels = (c('0-10%', '10-20%', '20-30%', '30-40%', '40-50%', '50-60%', '60-70%', '70-80%', '80-90%', '90-100%', '100%+')))

t2data$time <- as.factor(t2data$time)
#choose_palette()
#delousepal <- rev(sequential_hcl(11, h = 245, c = c(81, 14), l = c(34, 93), 1.1)) # blue palette
delousepal <- rev(sequential_hcl(11, h = 245, c = c(0, 0), l = c(17, 93), 1.1)) # grey palette

dplot.t2 <- t2data %>%
  filter(treatment != 'Control') %>%
  ggplot(aes(x = time, fill = dprop.t)) +
  geom_bar(position = 'fill', stat = 'count') +
  scale_x_discrete(name = 'Time (h)', expand = c(0, 0)) +
  scale_y_continuous(name = 'Proportion of salmon', labels = scales::percent, expand = c(0, 0)) +
  facet_wrap(~treatment) +
  scale_fill_manual(values = delousepal, name = 'Lice per fish') +
  theme_classic() +
  theme(strip.background = element_rect(color = 'white', fill = "white"),
        text = element_text(size = 14), 
        legend.position = 'none')

# Trial 4 winter
means <- t4data %>%
  group_by(time, tank, treatment, replicate) %>%
  dplyr::summarise(start.m = mean(total.m), start.f = mean(total.f), start.t = mean(total.m + total.f)) %>%
  filter(time == '0')

t4dec <- left_join(t4data, means[,c(2, 5:7)], by = 'tank') # Join mean T0 totals to raw dataset to compare delousing rate
t4dec <- t4dec %>% mutate(dec.m = (total.m/start.m)*100, dec.f = (total.f/start.f)*100, dec.t = (total/start.t)*100) # calculate delousing rate columns based on tank mean at T0

t4dec$dprop.t <- cut(t4dec$dec.t, breaks = c(-1, seq(10, 100, 10), 800), 
                     labels = (c('0-10%', '10-20%', '20-30%', '30-40%', '40-50%', '50-60%', '60-70%', '70-80%', '80-90%', '90-100%', '100%+')))

t4dec$time <- as.factor(t4dec$time)
#choose_palette()
#delousepal <- rev(sequential_hcl(11, h = 245, c = c(81, 14), l = c(34, 93), 1.1)) # blue palette
delousepal <- rev(sequential_hcl(11, h = 245, c = c(0, 0), l = c(17, 93), 1.1)) # grey palette

dplot.t4 <- t4dec %>%
  filter(treatment != 'Control') %>%
  ggplot(aes(x = time, fill = dprop.t)) +
  geom_bar(position = 'fill', stat = 'count') +
  scale_x_discrete(name = 'Time (h)', expand = c(0, 0)) +
  scale_y_continuous(name = 'Proportion of salmon', labels = scales::percent, expand = c(0, 0)) +
  facet_wrap(~treatment) +
  scale_fill_manual(values = delousepal, name = 'Lice per fish') +
  theme_classic() +
  theme(strip.background = element_rect(color = 'white', fill = "white"),
        text = element_text(size = 14),
        legend.position = 'none')

dplot.legend <- get_legend(dplot.t1)
dplot.t1 <- dplot.t1 + theme(legend.position = 'none')

plot_grid(plot_grid(dplot.t1, dplot.legend, '', ncol = 3, nrow = 1, rel_widths = c(0.7, 0.15, 0.15), labels = c('(a)')),
          plot_grid(dplot.t2, dplot.t4, ncol = 2, nrow = 1, rel_widths = c(0.58, 0.42), labels = c('(b)', '(c)')),
          ncol = 1,
          nrow = 2)
          #labels = c('(a)', '(b)', '(c)'), 
          #hjust = c(-26.6, -25.3, -26.8), 
          #vjust = c(1.8, 1.8, 1.8))

# Figure 3. Fish maps of start and end of trial----------------------------------------

# Trial 2 cryptic

# melt data into long format
t2melt <- t2data %>%
  select(time:fish, dorsal_head.mt:tail.ft) %>%
  mutate(head.mt = dorsal_head.mt + mid_head.mt + ventral_head.mt, head.ft = dorsal_head.ft + mid_head.ft + ventral_head.ft) %>%
  select(-dorsal_head.mt, -mid_head.mt, -ventral_head.mt, -dorsal_head.ft, -mid_head.ft, -ventral_head.ft) %>%
  gather(key = location, value = lice, -time:-fish) %>%
  separate(col = location, into = c('location', 'gender'), sep = -3) %>%
  spread(gender, lice) %>%
  rename(male = .mt, female = .ft)

t2melt$location <- as.factor(t2melt$location)
t2melt$time <- as.factor(t2melt$time)
t2melt$location <- factor(t2melt$location, levels(t2melt$location)[c(7, 1, 2, 3, 4, 5, 6, 9, 10, 11, 8)])
t2melt$total <- t2melt$male + t2melt$female

t2melt.sub <- filter(t2melt, time == 0 | time == 96)

t2locsum <- t2melt.sub %>%
  group_by(time, treatment, location) %>%
  dplyr::summarise(mean.m = mean(male), sd.m = sd(male), mean.f = mean(female), sd.f = sd(female), mean.t = mean(total), sd.t = sd(total)) 

# subset by treatment, calculate anovas between 0h and 96h for each location and reassemble results as df
aov.results <- data.frame()
for(f in 1:length(unique(t2melt.sub$treatment))){
  
  datf <- filter(t2melt.sub, t2melt.sub$treatment == unique(t2melt.sub$treatment)[[f]])
  
  modobj <- datf %>% group_by(location) %>% do(model = aov(female~time, data = .)) # apply aov to each location with time as factor and create df of aov models
  modobj$summary <- lapply(modobj$model, summary) # new column of model summaries
  modobj$p <- unlist(lapply(modobj$summary, function(x) x[[1]]$'Pr(>F)'[1])) # extract p-values from summaries and add to new column
  modobj$p <- ifelse(is.nan(modobj$p), 1, modobj$p) # change NaNs to 1
  modobj$sig <- ifelse(modobj$p < 0.001, '***', ifelse(modobj$p < 0.01, '**', ifelse(modobj$p < 0.05, '*', ''))) # calculate significance and add to new column
  modobj <- select(modobj, -model, -summary)
  modobj$time <- factor(96, levels = c(0, 48, 96, 168))
  modobj$treatment <- unique(t2melt.sub$treatment)[[f]]
  
  aov.results <- bind_rows(aov.results, modobj)
}

t2locsum <- left_join(t2locsum, aov.results, by = c('time', 'treatment', 'location'))
rm(datf, modobj, aov.results)
t2locsum$p <- ifelse(is.na(t2locsum$p), '', t2locsum$p)
t2locsum$sig <- ifelse(is.na(t2locsum$sig), '', t2locsum$sig)

# lice maps

# lumpfish cryptic
licelocs <- data.frame(location = unique(t2locsum$location), 
                       lice.m = t2locsum$mean.f[t2locsum$time == '0' & t2locsum$treatment == 'Lumpfish cryptic'],
                       lice.sd = t2locsum$sd.f[t2locsum$time == '0' & t2locsum$treatment == 'Lumpfish cryptic'])
map.lice(licelocs, 3.2, T, F, 'Mean lice')
lcf0 <- salplot

licelocs <- data.frame(location = unique(t2locsum$location), 
                       lice.m = t2locsum$mean.f[t2locsum$time == '96' & t2locsum$treatment == 'Lumpfish cryptic'],
                       lice.sd = t2locsum$sd.f[t2locsum$time == '96' & t2locsum$treatment == 'Lumpfish cryptic'],
                       lice.sig = t2locsum$sig[t2locsum$time == '96' & t2locsum$treatment == 'Lumpfish cryptic'])

map.lice(licelocs, 3.2, T, T, 'Mean lice')
lcf96 <- salplot

lctitle <- ggdraw() +
  draw_label('Lumpfish cryptic', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

# Lumpfish pigmented
licelocs <- data.frame(location = unique(t2locsum$location), 
                       lice.m = t2locsum$mean.f[t2locsum$time == '0' & t2locsum$treatment == 'Lumpfish pigmented'],
                       lice.sd = t2locsum$sd.f[t2locsum$time == '0' & t2locsum$treatment == 'Lumpfish pigmented'])
map.lice(licelocs, 3.2, T, F, 'Mean lice')
lpf0 <- salplot

licelocs <- data.frame(location = unique(t2locsum$location), 
                       lice.m = t2locsum$mean.f[t2locsum$time == '96' & t2locsum$treatment == 'Lumpfish pigmented'],
                       lice.sd = t2locsum$sd.f[t2locsum$time == '96' & t2locsum$treatment == 'Lumpfish pigmented'],
                       lice.sig = t2locsum$sig[t2locsum$time == '96' & t2locsum$treatment == 'Lumpfish pigmented'])

map.lice(licelocs, 3.2, T, T, 'Mean lice')
lpf96 <- salplot

lptitle <- ggdraw() +
  draw_label('Lumpfish pigmented', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)


# 168h plots
t2melt.sub <- filter(t2melt, time == 0 | time == 168)

t2locsum <- t2melt.sub %>%
  group_by(time, treatment, location) %>%
  dplyr::summarise(mean.m = mean(male), sd.m = sd(male), mean.f = mean(female), sd.f = sd(female), mean.t = mean(total), sd.t = sd(total)) 

# subset by treatment, calculate anovas between 0h and 168h for each location and reassemble results as df
aov.results <- data.frame()
for(f in 1:length(unique(t2melt.sub$treatment))){
  
  datf <- filter(t2melt.sub, t2melt.sub$treatment == unique(t2melt.sub$treatment)[[f]])
  
  modobj <- datf %>% group_by(location) %>% do(model = aov(female~time, data = .)) # apply aov to each location with time as factor and create df of aov models
  modobj$summary <- lapply(modobj$model, summary) # new column of model summaries
  modobj$p <- unlist(lapply(modobj$summary, function(x) x[[1]]$'Pr(>F)'[1])) # extract p-values from summaries and add to new column
  modobj$p <- ifelse(is.nan(modobj$p), 1, modobj$p) # change NaNs to 1
  modobj$sig <- ifelse(modobj$p < 0.001, '***', ifelse(modobj$p < 0.01, '**', ifelse(modobj$p < 0.05, '*', ''))) # calculate significance and add to new column
  modobj <- select(modobj, -model, -summary)
  modobj$time <- factor(168, levels = c(0, 48, 96, 168))
  modobj$treatment <- unique(t2melt.sub$treatment)[[f]]
  
  aov.results <- bind_rows(aov.results, modobj)
}

t2locsum <- left_join(t2locsum, aov.results, by = c('time', 'treatment', 'location'))
rm(datf, modobj, aov.results)
t2locsum$p <- ifelse(is.na(t2locsum$p), '', t2locsum$p)
t2locsum$sig <- ifelse(is.na(t2locsum$sig), '', t2locsum$sig)

#lice maps
# lumpfish cryptic
licelocs <- data.frame(location = unique(t2locsum$location), 
                       lice.m = t2locsum$mean.f[t2locsum$time == '168' & t2locsum$treatment == 'Lumpfish cryptic'],
                       lice.sd = t2locsum$sd.f[t2locsum$time == '168' & t2locsum$treatment == 'Lumpfish cryptic'],
                       lice.sig = t2locsum$sig[t2locsum$time == '168' & t2locsum$treatment == 'Lumpfish cryptic'])

map.lice(licelocs, 3.2, T, T, 'Mean lice')
lcf168 <- salplot

# Lumpfish pigmented
licelocs <- data.frame(location = unique(t2locsum$location), 
                       lice.m = t2locsum$mean.f[t2locsum$time == '168' & t2locsum$treatment == 'Lumpfish pigmented'],
                       lice.sd = t2locsum$sd.f[t2locsum$time == '168' & t2locsum$treatment == 'Lumpfish pigmented'],
                       lice.sig = t2locsum$sig[t2locsum$time == '168' & t2locsum$treatment == 'Lumpfish pigmented'])

map.lice(licelocs, 3.2, T, T, 'Mean lice')
lpf168 <- salplot


T0title <- ggdraw() +
  draw_label('0h', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)
T96title <- ggdraw() +
  draw_label('96h', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)
T168title <- ggdraw() +
  draw_label('168h', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)


# male plots
t2melt.sub <- filter(t2melt, time == 0 | time == 96)

t2locsum <- t2melt.sub %>%
  group_by(time, treatment, location) %>%
  dplyr::summarise(mean.m = mean(male), sd.m = sd(male), mean.f = mean(female), sd.f = sd(female), mean.t = mean(total), sd.t = sd(total)) 

# subset by treatment, calculate anovas between 0h and 96h for each location and reassemble results as df
aov.results <- data.frame()
for(f in 1:length(unique(t2melt.sub$treatment))){
  
  datf <- filter(t2melt.sub, t2melt.sub$treatment == unique(t2melt.sub$treatment)[[f]])
  
  modobj <- datf %>% group_by(location) %>% do(model = aov(male~time, data = .)) # apply aov to each location with time as factor and create df of aov models
  modobj$summary <- lapply(modobj$model, summary) # new column of model summaries
  modobj$p <- unlist(lapply(modobj$summary, function(x) x[[1]]$'Pr(>F)'[1])) # extract p-values from summaries and add to new column
  modobj$p <- ifelse(is.nan(modobj$p), 1, modobj$p) # change NaNs to 1
  modobj$sig <- ifelse(modobj$p < 0.001, '***', ifelse(modobj$p < 0.01, '**', ifelse(modobj$p < 0.05, '*', ''))) # calculate significance and add to new column
  modobj <- select(modobj, -model, -summary)
  modobj$time <- factor(96, levels = c(0, 48, 96, 168))
  modobj$treatment <- unique(t2melt.sub$treatment)[[f]]
  
  aov.results <- bind_rows(aov.results, modobj)
}

t2locsum <- left_join(t2locsum, aov.results, by = c('time', 'treatment', 'location'))
rm(datf, modobj, aov.results)
t2locsum$p <- ifelse(is.na(t2locsum$p), '', t2locsum$p)
t2locsum$sig <- ifelse(is.na(t2locsum$sig), '', t2locsum$sig)

# lice maps

# lumpfish cryptic
licelocs <- data.frame(location = unique(t2locsum$location), 
                       lice.m = t2locsum$mean.m[t2locsum$time == '0' & t2locsum$treatment == 'Lumpfish cryptic'],
                       lice.sd = t2locsum$sd.m[t2locsum$time == '0' & t2locsum$treatment == 'Lumpfish cryptic'])
map.lice(licelocs, 3.2, T, F, 'Mean lice')
lcm0 <- salplot

licelocs <- data.frame(location = unique(t2locsum$location), 
                       lice.m = t2locsum$mean.m[t2locsum$time == '96' & t2locsum$treatment == 'Lumpfish cryptic'],
                       lice.sd = t2locsum$sd.m[t2locsum$time == '96' & t2locsum$treatment == 'Lumpfish cryptic'],
                       lice.sig = t2locsum$sig[t2locsum$time == '96' & t2locsum$treatment == 'Lumpfish cryptic'])

map.lice(licelocs, 3.2, T, T, 'Mean lice')
lcm96 <- salplot

# Lumpfish pigmented
licelocs <- data.frame(location = unique(t2locsum$location), 
                       lice.m = t2locsum$mean.m[t2locsum$time == '0' & t2locsum$treatment == 'Lumpfish pigmented'],
                       lice.sd = t2locsum$sd.m[t2locsum$time == '0' & t2locsum$treatment == 'Lumpfish pigmented'])
map.lice(licelocs, 3.2, T, F, 'Mean lice')
lpm0 <- salplot

licelocs <- data.frame(location = unique(t2locsum$location), 
                       lice.m = t2locsum$mean.m[t2locsum$time == '96' & t2locsum$treatment == 'Lumpfish pigmented'],
                       lice.sd = t2locsum$sd.m[t2locsum$time == '96' & t2locsum$treatment == 'Lumpfish pigmented'],
                       lice.sig = t2locsum$sig[t2locsum$time == '96' & t2locsum$treatment == 'Lumpfish pigmented'])

map.lice(licelocs, 3.2, T, T, 'Mean lice')
lpm96 <- salplot

#168h male plots
t2melt.sub <- filter(t2melt, time == 0 | time == 168)

t2locsum <- t2melt.sub %>%
  group_by(time, treatment, location) %>%
  dplyr::summarise(mean.m = mean(male), sd.m = sd(male), mean.m = mean(female), sd.m = sd(female), mean.t = mean(total), sd.t = sd(total)) 

# subset by treatment, calculate anovas between 0h and 168h for each location and reassemble results as df
aov.results <- data.frame()
for(f in 1:length(unique(t2melt.sub$treatment))){
  
  datf <- filter(t2melt.sub, t2melt.sub$treatment == unique(t2melt.sub$treatment)[[f]])
  
  modobj <- datf %>% group_by(location) %>% do(model = aov(male~time, data = .)) # apply aov to each location with time as factor and create df of aov models
  modobj$summary <- lapply(modobj$model, summary) # new column of model summaries
  modobj$p <- unlist(lapply(modobj$summary, function(x) x[[1]]$'Pr(>F)'[1])) # extract p-values from summaries and add to new column
  modobj$p <- ifelse(is.nan(modobj$p), 1, modobj$p) # change NaNs to 1
  modobj$sig <- ifelse(modobj$p < 0.001, '***', ifelse(modobj$p < 0.01, '**', ifelse(modobj$p < 0.05, '*', ''))) # calculate significance and add to new column
  modobj <- select(modobj, -model, -summary)
  modobj$time <- factor(168, levels = c(0, 48, 96, 168))
  modobj$treatment <- unique(t2melt.sub$treatment)[[f]]
  
  aov.results <- bind_rows(aov.results, modobj)
}

t2locsum <- left_join(t2locsum, aov.results, by = c('time', 'treatment', 'location'))
rm(datf, modobj, aov.results)
t2locsum$p <- ifelse(is.na(t2locsum$p), '', t2locsum$p)
t2locsum$sig <- ifelse(is.na(t2locsum$sig), '', t2locsum$sig)

#lice maps
# lumpfish cryptic
licelocs <- data.frame(location = unique(t2locsum$location), 
                       lice.m = t2locsum$mean.m[t2locsum$time == '168' & t2locsum$treatment == 'Lumpfish cryptic'],
                       lice.sd = t2locsum$sd.m[t2locsum$time == '168' & t2locsum$treatment == 'Lumpfish cryptic'],
                       lice.sig = t2locsum$sig[t2locsum$time == '168' & t2locsum$treatment == 'Lumpfish cryptic'])

map.lice(licelocs, 3.2, T, T, 'Mean lice')
lcm168 <- salplot

# Lumpfish pigmented
licelocs <- data.frame(location = unique(t2locsum$location), 
                       lice.m = t2locsum$mean.m[t2locsum$time == '168' & t2locsum$treatment == 'Lumpfish pigmented'],
                       lice.sd = t2locsum$sd.m[t2locsum$time == '168' & t2locsum$treatment == 'Lumpfish pigmented'],
                       lice.sig = t2locsum$sig[t2locsum$time == '168' & t2locsum$treatment == 'Lumpfish pigmented'])

map.lice(licelocs, 3.2, T, T, 'Mean lice')
lpm168 <- salplot




# Plot all trial 2 totals
plot_grid(
  plot_grid(T0title, T96title, T168title, lpf0, lpf96, lpf168, lpm0, lpm96, lpm168, lcf0, lcf96, lcf168,lcm0, lcm96, lcm168,  ncol = 3, nrow = 5, 
            rel_heights = c(0.04, 0.24, 0.24, 0.24, 0.24), 
            labels = c('', '', '',  'Pigmented females', '', '', 'Pigmented males', '', '', 'Cryptic females', '', '', 'Cryptic males', '', ''),
            vjust = 3.5, hjust = -0.1, label_size = 11),
  liceleg, ncol = 1, nrow = 2, rel_heights = c(0.92, 0.08))


# Statistics for the paper---------------------------------------------------------------------

# one-way anova to test for differences within replicate treatment tanks
anova <- t1data.s %>%
  #filter(time == '96') %>%
  filter(treatment == 'Wrasse') %>%
  aov(data = ., formula = total.m~replicate)
summary(anova)

# one-way anova to compare lice numbers to control at each time point
anova <- t1data.s %>%
  filter(time == '96') %>%
  filter(treatment == 'Control' | treatment == 'Large lumpfish') %>%
  aov(data = ., formula = total.m~treatment)
summary(anova)

# two-way anova to compare lice numbers to control over time
anova <- t1data.s %>%
  filter(treatment == 'Control' | treatment == 'Wrasse') %>%
  aov(data = ., formula = total~treatment*time)
summary(anova)

# two-way nested anova
# https://www.researchgate.net/post/How_do_I_compare_2_different_groups_control_vs_treatment_over_time_And_how_do_I_see_at_what_moment_in_time_they_become_sign_different
# nested factors: treatment < replicate < time
t1data.s$time <- as.factor(t1data.s$time)
t1data.s$replicate <- as.factor(t1data.s$replicate)

anova <- t1data.s %>%
  aov(data = ., formula = total.f~treatment+treatment:replicate+replicate:time)
  #aov(data = ., formula = total.f~treatment:replicate:time)
summary(anova)

TukeyHSD(anova)

# Repeated measures linear mixed-effects model with nested replicate tanks
# https://rcompanion.org/handbook/I_09.html
library(nlme)

model <- t1data.s %>%
  group_by(time, tank, treatment, replicate) %>%
  dplyr::summarise(mean.m = mean(total.m), mean.f = mean(total.f), mean.t = mean(total)) %>%
  ungroup() %>%
  mutate(time = as.numeric(time)) %>%
  lme(mean.f ~ treatment + time + treatment*time, 
            random = ~1|tank,
            correlation = corAR1(form = ~ time | tank,
                                 value = 0.6273),
            data = .,
            method = "REML")

# determine autocorrelation structure
ACF(model) # enter lag 1 value into corAR1 value and recalculate lme

library(car)

Anova(model)

# test random effects in the model
model.fixed <- t1data.s %>%
  group_by(time, tank, treatment, replicate) %>%
  dplyr::summarise(mean.m = mean(total.m), mean.f = mean(total.f), mean.t = mean(total)) %>%
  ungroup() %>%
  mutate(time = as.numeric(time)) %>%
  gls(mean.f ~ treatment + time + treatment*time,
                  data = .,
                  method="REML")

anova(model,
      model.fixed)

# post-hoc analysis
library(multcomp)
library(multcompView)
library(lsmeans)

marginal <- lsmeans(model, 
                   ~ treatment:time)

cld(marginal,
    alpha   = 0.2, 
    Letters = letters,     ### Use lower-case letters for .group
    adjust  = "tukey")     ###  Tukey-adjusted comparisons
