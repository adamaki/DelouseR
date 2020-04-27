# Cleaner fish tank delousing
# Adam Brooker 29th August 2019

source('/Users/adambrooker/R Projects/DelouseR/Delousing-initiate.R')


# Load and reformat T1 summer temperature trial ---------------------------------------
workingdir <- ifelse(Sys.info()['user'] == 'Laptop', 'G:/Projects/Lumpfish delousing/Data/T1 Summer', 
                     '/Users/adambrooker/Dropbox/1-IoA/cleanerfish/Projects/SAIC Lumpfish/Delousing Trials/T1 Summer') # change to location of data
setwd(workingdir)

t1data <- read.csv('t1summerdata.csv')

t1data$time <- dplyr::recode(t1data$time, '1' = 0, '2' = 24, '3' = 48, '4' = 72, '5' = 96)
t1data$treatment <- dplyr::recode(t1data$treatment, 'con' = 'Control', 'LL' = 'Large lumpfish', 'SL' = 'Small lumpfish', 'W' = 'Wrasse')

t1data <- t1data %>% mutate(total.m = t1data %>% rowwise() %>% select(contains('.m')) %>% rowSums()) # new total male col
t1data <- t1data %>% mutate(total.f = t1data %>% rowwise() %>% select(contains('.f')) %>% rowSums()) # new total female col
t1data$total <- t1data$total.m + t1data$total.f

t1data <- arrange(t1data, time, treatment, replicate) # arrange data


# melt data into long format--------------------------------------------------------------------------------------------------
males <- t1data %>% 
          mutate(head.m = dorsal_head.m + mid_head.m + ventral_head.m) %>%
          select(-dorsal_head.m, -mid_head.m, -ventral_head.m) %>%
          melt(id = c('time', 'tank', 'treatment', 'replicate','fish'), value.name = 'male') %>%
          filter(grepl('.m', variable, fixed = T)) %>%
          filter(variable != 'total.m' & variable != 'bucket.m') %>%
          dplyr::rename(location = variable)

females <- t1data %>%
            mutate(head.f = dorsal_head.f + mid_head.f + ventral_head.f) %>%
            select(-dorsal_head.f, -mid_head.f, -ventral_head.f) %>%
            melt(id = c('time', 'tank', 'treatment', 'replicate','fish'), value.name = 'female') %>%
            filter(grepl('.f', variable, fixed = T)) %>%
            filter(variable != 'total.f' & variable != 'bucket.f')

t1melt <- bind_cols(males, female = females$female)
t1melt$location <- str_sub(t1melt$location, end = -3) # remove .m from locations
t1melt$location <- as.factor(t1melt$location)
t1melt$time <- as.factor(t1melt$time)
t1melt$location <- factor(t1melt$location, levels(t1melt$location)[c(7, 1, 2, 3, 4, 5, 6, 9, 10, 11, 8)])
t1melt$total <- t1melt$male + t1melt$female
rm(males, females)

# summarise data and barplot No. of lice by time and location----------------------------------------------------------------------
t1locsum <- t1melt %>%
  group_by(time, treatment, replicate, location) %>%
  dplyr::summarise(mean.m = mean(male), sd.m = sd(male), mean.f = mean(female), sd.f = sd(female), mean.t = mean(total), sd.t = sd(total)) 

#choose_palette()
fishpal <- c(sequential_hcl(3, h = -349, c = c(83, 50), l = c(34, 81), 0.7)[2], 
             sequential_hcl(3, h = -99, c = c(83, 50), l = c(34, 81), 0.7), 
             sequential_hcl(3, h = -232, c = c(83, 50), l = c(34, 81), 0.7), 
             sequential_hcl(3, h = 61, c = c(83, 50), l = c(34, 81), 0.7), 
             sequential_hcl(3, h = -53, c = c(83, 50), l = c(34, 81), 0.7)[2])
             
             
# all lice plot
ggplot(t1locsum, aes(x = time, y = mean.t, fill = location)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_x_discrete(breaks = c(0, 24, 48, 72, 96), labels = c('0', '24', '48', '72', '96'), name = 'Time (h)') +
  scale_y_continuous(name = 'mean No. of lice') +
  scale_fill_manual(values = fishpal) +
  theme_classic() +
  facet_wrap(~treatment)

# female plot
ggplot(t1locsum, aes(x = time, y = mean.f, fill = location)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_x_discrete(breaks = c(0, 24, 48, 72, 96), labels = c('0', '24', '48', '72', '96'), name = 'Time (h)') +
  scale_y_continuous(name = 'mean No. of female lice') +
  scale_fill_manual(values = fishpal) +
  theme_classic() +
  facet_wrap(~treatment)

# male plot
ggplot(t1locsum, aes(x = time, y = mean.m, fill = location)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_x_discrete(breaks = c(0, 24, 48, 72, 96), labels = c('0', '24', '48', '72', '96'), name = 'Time (h)') +
  scale_y_continuous(name = 'mean No. of male lice') +
  scale_fill_manual(values = fishpal) +
  theme_classic() +
  facet_wrap(~treatment)


# summarise data and do lineplots--------------------------------------------------------------------------------
t1summ <- t1data %>%
  group_by(time, tank, treatment, replicate) %>%
  dplyr::summarise(m_m = mean(total.m), sd_m = sd(total.m), m_f = mean(total.f), sd_f = sd(total.f), tot_m = mean(total.m + total.f), tot_sd = sd(total.m + total.f))

# summarise by time and group with rep as error and draw plot of total lice
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

# plot all lice by tank over time
ggplot(data = t1summ, aes(x = time, y = tot_m, group = tank, colour = treatment)) +
  geom_line(size = 1) +
#  geom_errorbar(aes(x = time, ymin = tot_m-tot_sd, ymax = tot_m+tot_sd), width = 3, position = 'dodge') +
  geom_errorbar(aes(x = time, ymin = tot_m-(tot_sd/sqrt(10)), ymax = tot_m+(tot_sd/sqrt(10))), width = 3, position = 'dodge') +
  scale_y_continuous(limits = c(0, 30)) +
  ggtitle('All lice') +
  theme_classic()

# plot all lice by tank over time
t1summ %>% filter(treatment == 'Wrasse') %>%
ggplot(aes(x = time, y = tot_m, group = tank, colour = tank)) +
  geom_line(size = 1) +
  geom_errorbar(aes(x = time, ymin = tot_m-tot_sd, ymax = tot_m+tot_sd), width = 3, position = 'dodge') +
  scale_y_continuous(limits = c(0, 30)) +
  ggtitle('All lice') +
  theme_classic()

# Delousing rates----------------------------------------------------------------------------------------------

# create summary dataset of T0 mean lice counts
means <- t1data %>%
  group_by(time, tank, treatment, replicate) %>%
  dplyr::summarise(start.m = mean(total.m), start.f = mean(total.f), start.t = mean(total.m + total.f)) %>%
  filter(time == '0')

t1data <- left_join(t1data, means[,c(2, 5:7)], by = 'tank') # Join mean T0 totals to raw dataset to compare delousing rate

t1data <- t1data %>% mutate(dec.m = (total.m/start.m)*100, dec.f = (total.f/start.f)*100, dec.t = (total/start.t)*100) # calculate delousing rate columns based on tank mean at T0

# change 0% values to 0.1% (avoids fish outside 0-10 category)
#t1dfilt <- rbind(t1data %>% filter(time == '0' | treatment == 'Control'), t1data %>% filter(time != '0' & treatment != 'Control' & dec.t < 0.5))
t1filt <- t1data %>%
  mutate(dec.m = ifelse(dec.m == 0, 0.1, dec.m)) %>%
  mutate(dec.f = ifelse(dec.f == 0, 0.1, dec.f)) %>%
  mutate(dec.t = ifelse(dec.t == 0, 0.1, dec.t))
t1filt <- arrange(t1filt, time, treatment, replicate, fish)

# melt data into long format
males <- t1filt %>% 
  mutate(head.m = dorsal_head.m + mid_head.m + ventral_head.m) %>%
  select(-dorsal_head.m, -mid_head.m, -ventral_head.m, -total.m, -start.m, -dec.m, -bucket.m) %>%
  reshape2::melt(id = c('time', 'tank', 'treatment', 'replicate','fish'), value.name = 'male') %>%
  filter(grepl('.m', variable, fixed = T)) %>%
  #filter(variable != 'total.m' & variable != 'bucket.m') %>%
  dplyr::rename(location = variable)

females <- t1filt %>%
  mutate(head.f = dorsal_head.f + mid_head.f + ventral_head.f) %>%
  select(-dorsal_head.f, -mid_head.f, -ventral_head.f, -total.f, -start.f, -dec.f, -bucket.f) %>%
  reshape2::melt(id = c('time', 'tank', 'treatment', 'replicate','fish'), value.name = 'female') %>%
  filter(grepl('.f', variable, fixed = T))
  #filter(variable != 'total.f' & variable != 'bucket.f')

t1melt <- bind_cols(males, female = females$female)
t1melt$location <- str_sub(t1melt$location, end = -3) # remove .m from locations
t1melt$location <- as.factor(t1melt$location)
t1melt$time <- as.factor(t1melt$time)
t1melt$location <- factor(t1melt$location, levels(t1melt$location)[c(7, 1, 2, 3, 4, 5, 6, 9, 10, 11, 8)])
t1melt$total <- t1melt$male + t1melt$female
rm(males, females)

t1locsum <- t1melt %>%
  group_by(time, treatment, replicate, location) %>%
  dplyr::summarise(mean.m = mean(male), sd.m = sd(male), mean.f = mean(female), sd.f = sd(female), mean.t = mean(total), sd.t = sd(total)) 

# plot treatments by daily delousing rate histogram---------------------
t1data$dprop.m <- cut(t1data$dec.m, breaks = c(-1, seq(10, 90, 10), 800), labels = (c('0-10', '11-20', '21-30', '31-40', '41-50', '51-60', '61-70', '71-80', '81-90', '91-100')))
t1data$dprop.f <- cut(t1data$dec.f, breaks = c(-1, seq(10, 90, 10), 800), labels = (c('0-10', '11-20', '21-30', '31-40', '41-50', '51-60', '61-70', '71-80', '81-90', '91-100')))
t1data$dprop.t <- cut(t1data$dec.t, breaks = c(-1, seq(10, 90, 10), 800), labels = (c('0-10', '11-20', '21-30', '31-40', '41-50', '51-60', '61-70', '71-80', '81-90', '91-100')))
#t5data$dprop.t <- cut(t5data$dec.t, breaks = c(-1, seq(10, 90, 10), 800), labels = (c('91-100%', '81-90%', '71-80%', '61-70%', '51-60%', '41-50%', '31-40%', '21-30%', '11-20%', '0-10%')))

t5data$time <- as.factor(t5data$time)
delousepal <- rev(sequential_hcl(10, h = 245, c = c(81, 14), l = c(34, 93), 1.1))

dplot.f <- t5data %>%
  filter(treatment != 'Control' & time != 0) %>%
  ggplot(aes(x = time, fill = dprop.f)) +
  geom_bar(position = 'fill', stat = 'count') +
  scale_x_discrete(name = 'Time (h)', expand = c(0, 0)) +
  scale_y_continuous(name = 'Proportion', expand = c(0, 0)) +
  facet_wrap(~treatment) +
  scale_fill_manual(values = delousepal, name = '% female lice \n remaining') +
  theme_classic() +
  ggtitle('Delousing rates - female lice')

dplot.m <- t1data %>%
  filter(treatment != 'Control' & time != 0) %>%
  ggplot(aes(x = time, fill = dprop.m)) +
  geom_bar(position = 'fill', stat = 'count') +
  scale_x_discrete(name = 'Time (h)', expand = c(0, 0)) +
  scale_y_continuous(name = 'Proportion', expand = c(0, 0)) +
  facet_wrap(~treatment) +
  scale_fill_manual(values = delousepal, name = '% male lice \n remaining') +
  theme_classic() +
  ggtitle('Delousing rates - male lice')

dplot.t <- t5data %>%
  filter(treatment != 'Control' & time != 0) %>%
  ggplot(aes(x = time, fill = dprop.t)) +
  geom_bar(position = 'fill', stat = 'count') +
  scale_x_discrete(name = 'Time (h)', expand = c(0, 0)) +
  scale_y_continuous(name = 'Proportion', expand = c(0, 0)) +
  facet_wrap(~treatment) +
  scale_fill_manual(values = delousepal, name = '% all lice \n remaining') +
  theme_classic() +
  ggtitle('Delousing rates - all lice')

# Map lice levels onto fish map --------------------------------------------------------------------------------------

#----------------------------------------------------------------
# magick testing

salout <- image_read('G:/Projects/Lumpfish delousing/Data/SalmonOutline.bmp')
#salout <- image_convert(salout, 'svg')

#choose_palette()
licepal <- sequential_hcl(10)
licepal <- sequential_hcl(10, -360, c(83, 25), c(30, 96), 1.1)
licepal <- colorspace::sequential_hcl(n = 10, h = c(0, -100), c. = c(80, 40), l = c(40, 75), power = c(1, 1), fixup = TRUE, gamma = NULL, alpha = 1)
licepal <- rev(heat.colors(10, alpha = 0.5))



# Lice location plots

t1locsum <- t1melt %>%
  group_by(time, treatment, location) %>%
  dplyr::summarise(mean.m = mean(male), sd.m = sd(male), mean.f = mean(female), sd.f = sd(female), mean.t = mean(total), sd.t = sd(total)) 

# total lice plots

# Large lumpfish
licelocs <- data.frame(location = unique(t1locsum$location), 
                       lice.m = t1locsum$mean.t[t1locsum$time == '0' & t1locsum$treatment == 'Large lumpfish'],
                       lice.sd = t1locsum$sd.t[t1locsum$time == '0' & t1locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
ll0 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.t[t5locsum$time == '24' & t5locsum$treatment == 'Large lumpfish'],
                       lice.sd = t5locsum$sd.t[t5locsum$time == '24' & t5locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
ll24 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.t[t5locsum$time == '48' & t5locsum$treatment == 'Large lumpfish'],
                       lice.sd = t5locsum$sd.t[t5locsum$time == '48' & t5locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
ll48 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.t[t5locsum$time == '72' & t5locsum$treatment == 'Large lumpfish'],
                       lice.sd = t5locsum$sd.t[t5locsum$time == '72' & t5locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
ll72 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.t[t5locsum$time == '96' & t5locsum$treatment == 'Large lumpfish'],
                       lice.sd = t5locsum$sd.t[t5locsum$time == '96' & t5locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
ll96 <- salplot

lltitle <- ggdraw() +
  draw_label('Large Lumpfish', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(lltitle, ll0, ll24, ll48, ll72, ll96, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
          labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)


# Small lumpfish
licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.t[t5locsum$time == '0' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.t[t5locsum$time == '0' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
sl0 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.t[t5locsum$time == '24' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.t[t5locsum$time == '24' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
sl24 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.t[t5locsum$time == '48' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.t[t5locsum$time == '48' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
sl48 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.t[t5locsum$time == '72' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.t[t5locsum$time == '72' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
sl72 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.t[t5locsum$time == '96' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.t[t5locsum$time == '96' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
sl96 <- salplot

sltitle <- ggdraw() +
  draw_label('Small Lumpfish', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(sltitle, sl0, sl24, sl48, sl72, sl96, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
          labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse
licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.t[t5locsum$time == '0' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.t[t5locsum$time == '0' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
w0 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.t[t5locsum$time == '24' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.t[t5locsum$time == '24' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
w24 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.t[t5locsum$time == '48' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.t[t5locsum$time == '48' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
w48 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.t[t5locsum$time == '72' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.t[t5locsum$time == '72' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
w72 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.t[t5locsum$time == '96' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.t[t5locsum$time == '96' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 8.83, T, T, 'Total No. lice')
w96 <- salplot

wtitle <- ggdraw() +
  draw_label('Wrasse', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(wtitle, w0, w24, w48, w72, w96, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
          labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

# All treatments plot
slplot <- plot_grid(sltitle, sl0, sl24, sl48, sl72, sl96, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
                    labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

llplot <- plot_grid(lltitle, ll0, ll24, ll48, ll72, ll96, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
                    labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

wplot <- plot_grid(wtitle, w0, w24, w48, w72, w96, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
                   labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

plot_grid(slplot, llplot, wplot, nrow = 1, ncol = 3)

# Female lice plots

# Large lumpfish
licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '0' & t5locsum$treatment == 'Large lumpfish'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '0' & t5locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
ll0 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '24' & t5locsum$treatment == 'Large lumpfish'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '24' & t5locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
ll24 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '48' & t5locsum$treatment == 'Large lumpfish'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '48' & t5locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
ll48 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '72' & t5locsum$treatment == 'Large lumpfish'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '72' & t5locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
ll72 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '96' & t5locsum$treatment == 'Large lumpfish'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '96' & t5locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
ll96 <- salplot

lltitle <- ggdraw() +
  draw_label('Large Lumpfish', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(lltitle, ll0, ll24, ll48, ll72, ll96, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
          labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)


# Small lumpfish
licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '0' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '0' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
sl0 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '24' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '24' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
sl24 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '48' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '48' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
sl48 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '72' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '72' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
sl72 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '96' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '96' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
sl96 <- salplot

sltitle <- ggdraw() +
  draw_label('Small Lumpfish', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(sltitle, sl0, sl24, sl48, sl72, sl96, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
          labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse
licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '0' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '0' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
w0 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '24' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '24' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
w24 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '48' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '48' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
w48 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '72' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '72' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
w72 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.f[t5locsum$time == '96' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.f[t5locsum$time == '96' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 6.4, T, T, 'No. female lice')
w96 <- salplot

wtitle <- ggdraw() +
  draw_label('Wrasse', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(wtitle, w0, w24, w48, w72, w96, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
          labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

# All treatments plot
slplot <- plot_grid(sltitle, sl0, sl24, sl48, sl72, sl96, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
                    labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

llplot <- plot_grid(lltitle, ll0, ll24, ll48, ll72, ll96, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
                    labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

wplot <- plot_grid(wtitle, w0, w24, w48, w72, w96, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
                   labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

plot_grid(slplot, llplot, wplot, nrow = 1, ncol = 3)

# Male lice plots

# Large lumpfish
licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '0' & t5locsum$treatment == 'Large lumpfish'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '0' & t5locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
ll0 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '24' & t5locsum$treatment == 'Large lumpfish'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '24' & t5locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
ll24 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '48' & t5locsum$treatment == 'Large lumpfish'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '48' & t5locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
ll48 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '72' & t5locsum$treatment == 'Large lumpfish'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '72' & t5locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
ll72 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '96' & t5locsum$treatment == 'Large lumpfish'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '96' & t5locsum$treatment == 'Large lumpfish'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
ll96 <- salplot

lltitle <- ggdraw() +
  draw_label('Large Lumpfish', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(lltitle, ll0, ll24, ll48, ll72, ll96, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
          labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)


# Small lumpfish
licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '0' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '0' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
sl0 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '24' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '24' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
sl24 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '48' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '48' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
sl48 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '72' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '72' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
sl72 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '96' & t5locsum$treatment == 'Small lumpfish'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '96' & t5locsum$treatment == 'Small lumpfish'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
sl96 <- salplot

sltitle <- ggdraw() +
  draw_label('Small Lumpfish', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(sltitle, sl0, sl24, sl48, sl72, sl96, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
          labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse
licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '0' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '0' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
w0 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '24' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '24' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
w24 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '48' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '48' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
w48 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '72' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '72' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
w72 <- salplot

licelocs <- data.frame(location = unique(t5locsum$location), 
                       lice.m = t5locsum$mean.m[t5locsum$time == '96' & t5locsum$treatment == 'Wrasse'],
                       lice.sd = t5locsum$sd.m[t5locsum$time == '96' & t5locsum$treatment == 'Wrasse'])
map.lice(licelocs, 2.6, T, T, 'No. male lice')
w96 <- salplot

wtitle <- ggdraw() +
  draw_label('Wrasse', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(wtitle, w0, w24, w48, w72, w96, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
          labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

# All treatments plot
slplot <- plot_grid(sltitle, sl0, sl24, sl48, sl72, sl96, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
          labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

llplot <- plot_grid(lltitle, ll0, ll24, ll48, ll72, ll96, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
          labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

wplot <- plot_grid(wtitle, w0, w24, w48, w72, w96, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.17, 5), 0.1), 
          labels = c('', ' 0h', '24h', '48h', '72h', '96h'), label_size = 12, vjust = 4, hjust = -2)

plot_grid(slplot, llplot, wplot, nrow = 1, ncol = 3)


