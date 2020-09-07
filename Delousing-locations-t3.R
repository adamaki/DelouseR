# Cleaner fish tank delousing
# Adam Brooker 29th August 2019

source('G:/Projects/Lumpfish delousing/Delousing-initiate.R')
source('/Users/adambrooker/R Projects/DelouseR/Delousing-initiate.R')


# Load and reformat T3 cryptic lice trial ---------------------------------------
setwd('/Users/adambrooker/Dropbox/1-IoA/cleanerfish/Projects/SAIC Lumpfish/Delousing Trials/T3 Individual Wrasse')


t3data <- read.csv('DelousingTrial3-IndWrasse.csv')

t3data$time <- dplyr::recode(t3data$time, '1' = 0, '2' = 48, '3' = 96, '4' = 144)
#t3data$treatment <- dplyr::recode(t3data$treatment, 'Con' = 'Control', 'LC' = 'Lumpfish cryptic', 'LP' = 'Lumpfish pigmented', 'WC' = 'Wrasse cryptic')


t3data <- t3data %>% mutate(total.m = t3data %>% rowwise() %>% select(contains('.m')) %>% rowSums()) # new total male col
t3data <- t3data %>% mutate(total.f = t3data %>% rowwise() %>% select(contains('.f')) %>% rowSums()) # new total female col

t3data$total <- t3data$total.m + t3data$total.f
t3data$tank <- factor(t3data$tank, levels = c('C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C12')) # convert ID to factor


# melt data into long format--------------------------------------------------------------------------------------------------
males <- t3data %>% 
  mutate(head.m = dorsal_head.m + mid_head.m + ventral_head.m) %>%
  select(-dorsal_head.m, -mid_head.m, -ventral_head.m) %>%
  select(-group) %>%
  select(-dorsal_head.f:-total) %>%
  melt(id = c('time', 'tank', 'replicate','fish'), value.name = 'male') %>%
  #filter(grepl('.mt', variable, fixed = T)) %>%
  dplyr::rename(location = variable)

females <- t3data %>% 
  mutate(head.f = dorsal_head.f + mid_head.f + ventral_head.f) %>%
  select(-dorsal_head.f, -mid_head.f, -ventral_head.f) %>%
  select(-group) %>%
  select(-dorsal_head.m:-tail.m) %>%
  select(-weight_g:-total) %>%
  melt(id = c('time', 'tank', 'replicate','fish'), value.name = 'female') %>%
  #filter(grepl('.mt', variable, fixed = T)) %>%
  dplyr::rename(location = variable)

t3melt <- bind_cols(males, female = females$female)
t3melt$location <- str_sub(t3melt$location, end = -3) # remove .m from locations
t3melt$location <- as.factor(t3melt$location)
t3melt$time <- as.factor(t3melt$time)
t3melt$location <- factor(t3melt$location, levels(t3melt$location)[c(7, 1, 2, 3, 4, 5, 6, 9, 10, 11, 8)])
t3melt$total <- t3melt$male + t3melt$female
rm(males, females)



# summarise data and barplot No. of lice by time and location----------------------------------------------------------------------
t3locsum <- t3melt %>%
  group_by(time, replicate, location) %>%
  dplyr::summarise(mean.m = mean(male), sd.m = sd(male), mean.f = mean(female), sd.f = sd(female), mean.t = mean(total), sd.t = sd(total)) 

#choose_palette()
fishpal <- c(sequential_hcl(3, h = -349, c = c(83, 50), l = c(34, 81), 0.7)[2], 
             sequential_hcl(3, h = -99, c = c(83, 50), l = c(34, 81), 0.7), 
             sequential_hcl(3, h = -232, c = c(83, 50), l = c(34, 81), 0.7), 
             sequential_hcl(3, h = 61, c = c(83, 50), l = c(34, 81), 0.7), 
             sequential_hcl(3, h = -53, c = c(83, 50), l = c(34, 81), 0.7)[2])


# all lice plot
ggplot(t3locsum, aes(x = time, y = mean.t, fill = location)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_x_discrete(breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)') +
  scale_y_continuous(name = 'mean No. of lice') +
  scale_fill_manual(values = fishpal) +
  theme_classic() +
  facet_wrap(~replicate)

# female plot
ggplot(t3locsum, aes(x = time, y = mean.f, fill = location)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_x_discrete(breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)') +
  scale_y_continuous(name = 'mean No. of female lice') +
  scale_fill_manual(values = fishpal) +
  theme_classic() +
  facet_wrap(~replicate)

# male plot
ggplot(t3locsum, aes(x = time, y = mean.m, fill = location)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_x_discrete(breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)') +
  scale_y_continuous(name = 'mean No. of male lice') +
  scale_fill_manual(values = fishpal) +
  theme_classic() +
  facet_wrap(~replicate)


# summarise data and do lineplots--------------------------------------------------------------------------------
t3summ <- t3data %>%
  #select(-dorsal_head.mc:-length_mm)
  group_by(time, tank, replicate) %>%
  dplyr::summarise(m_m = mean(total.m), sd_m = sd(total.m), m_f = mean(total.f), sd_f = sd(total.f), tot_m = mean(total.m + total.f), tot_sd = sd(total.m + total.f))

t3summ$tank <- factor(t3summ$tank, levels = c('C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C12')) # convert ID to factor
t3summ$replicate <- as.factor(t3summ$replicate)

# summarise by time and group with rep as error and draw plot of total lice
totliceplot <- t3summ %>%
  #group_by(time, replicate) %>%
  #dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = tot_m, colour = tank)) +
  geom_line(size = 1) +
  geom_errorbar(aes(x = time, ymin = tot_m-tot_sd, ymax = tot_m+tot_sd), width = 3, position = 'dodge', size = 1) +
  scale_y_continuous(limits = c(0, 12), name = 'mean lice per fish', expand = c(0, 0)) +
  scale_x_continuous(breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)', expand = c(0, 0)) +
  ggtitle('Total lice') +
  theme_classic() #+
  #theme(legend.title = element_blank())

# summarise by time and group with rep as error and draw plot of male lice
totmaleplot <- t3summ %>%
  #group_by(time, treatment) %>%
  #dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = m_m, colour = tank)) +
  geom_line(size = 1) +
  geom_errorbar(aes(x = time, ymin = m_m-sd_m, ymax = m_m+sd_m), width = 3, position = 'dodge', size = 1) +
  scale_y_continuous(limits = c(0, 8), name = 'mean lice per fish', expand = c(0, 0)) +
  scale_x_continuous(breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)', expand = c(0, 0)) +
  ggtitle('Male lice') +
  theme_classic() #+
  #theme(legend.title = element_blank())


# summarise by time and group with rep as error and draw plot of female lice
totfemaleplot <- t3summ %>%
  #group_by(time, treatment) %>%
  #dplyr::summarise(mean_m = mean(m_m), sd_m = sd(m_m), mean_f = mean(m_f), sd_f = sd(m_f), total_mean = mean(tot_m), total_sd = sd(tot_m)) %>%
  ggplot(aes(x = time, y = m_f, colour = tank)) +
  geom_line(size = 1) +
  geom_errorbar(aes(x = time, ymin = m_f-sd_f, ymax = m_f+sd_f), width = 3, position = 'dodge', size = 1) +
  scale_y_continuous(limits = c(0, 8), name = 'mean lice per fish', expand = c(0, 0)) +
  scale_x_continuous(breaks = c(0, 48, 96, 144), labels = c('0', '48', '96', '144'), name = 'Time (h)', expand = c(0, 0)) +
  ggtitle('Female lice') +
  theme_classic() #+
  #theme(legend.title = element_blank())




# Delousing rates----------------------------------------------------------------------------------------------

# create summary dataset of T0 mean lice counts
means <- t3data %>%
  group_by(time, tank, group) %>%
  dplyr::summarise(start.m = mean(total.m), start.f = mean(total.f), start.t = mean(total.m + total.f)) %>%
  filter(time == '0')

t3data <- left_join(t3data, means[,c(2, 4:5)], by = 'tank') # Join mean T0 totals to raw dataset to compare delousing rate

t3data <- t3data %>% mutate(dec.m = (total.m/start.m)*100, dec.f = (total.f/start.f)*100, dec.t = (total/start.t)*100) # calculate delousing rate columns based on tank mean at T0

# dataset prep for fish map plots
# filter treatments after T0 when delousing rate is > 0.5 of tank mean at T0
#t3dfilt <- rbind(t3data %>% filter(time == '0'), t3data %>% filter(time != '0' & dec.t < 0.5))
#t3dfilt <- arrange(t3dfilt, time, replicate, fish)

# melt data into long format
males <- t3data %>% 
  mutate(head.m = dorsal_head.m + mid_head.m + ventral_head.m) %>%
  select(-dorsal_head.m, -mid_head.m, -ventral_head.m, -total.m, -start.m, -dec.m) %>%
  melt(id = c('time', 'tank', 'replicate','fish'), value.name = 'male') %>%
  filter(grepl('.m', variable, fixed = T)) %>%
  #filter(variable != 'total.m' & variable != 'bucket.m') %>%
  dplyr::rename(location = variable)

females <- t3data %>%
  mutate(head.f = dorsal_head.f + mid_head.f + ventral_head.f) %>%
  select(-dorsal_head.f, -mid_head.f, -ventral_head.f, -total.f, -start.f, -dec.f) %>%
  melt(id = c('time', 'tank', 'replicate','fish'), value.name = 'female') %>%
  filter(grepl('.f', variable, fixed = T))
#filter(variable != 'total.f' & variable != 'bucket.f')

t3dmelt <- bind_cols(males, female = females$female)
t3dmelt$location <- str_sub(t3dmelt$location, end = -3) # remove .m from locations
t3dmelt$location <- as.factor(t3dmelt$location)
t3dmelt$time <- as.factor(t3dmelt$time)
t3dmelt$location <- factor(t3dmelt$location, levels(t3dmelt$location)[c(7, 1, 2, 3, 4, 5, 6, 9, 10, 11, 8)])
t3dmelt$total <- t3dmelt$male + t3dmelt$female
rm(males, females)

t3dlocsum <- t3dmelt %>%
  group_by(time, replicate, location) %>%
  dplyr::summarise(mean.m = mean(male), sd.m = sd(male), mean.f = mean(female), sd.f = sd(female), mean.t = mean(total), sd.t = sd(total)) 

# plot treatments by daily delousing rate histogram---------------------
t3data$dprop.m <- cut(t3data$dec.m, breaks = c(-1, seq(10, 90, 10), 800), labels = (c('0-10', '11-20', '21-30', '31-40', '41-50', '51-60', '61-70', '71-80', '81-90', '91-100')))
t3data$dprop.f <- cut(t3data$dec.f, breaks = c(-1, seq(10, 90, 10), 800), labels = (c('0-10', '11-20', '21-30', '31-40', '41-50', '51-60', '61-70', '71-80', '81-90', '91-100')))
t3data$dprop.t <- cut(t3data$dec.t, breaks = c(-1, seq(10, 90, 10), 800), labels = (c('0-10', '11-20', '21-30', '31-40', '41-50', '51-60', '61-70', '71-80', '81-90', '91-100')))
#t5data$dprop.t <- cut(t5data$dec.t, breaks = c(-1, seq(10, 90, 10), 800), labels = (c('91-100%', '81-90%', '71-80%', '61-70%', '51-60%', '41-50%', '31-40%', '21-30%', '11-20%', '0-10%')))

t3data$time <- as.factor(t3data$time)
delousepal <- rev(sequential_hcl(10, h = 245, c = c(81, 14), l = c(34, 93), 1.1))

dplot.f <- t3data %>%
  #filter(time != 0) %>%
  ggplot(aes(x = time, fill = dprop.f)) +
  geom_bar(position = 'fill', stat = 'count') +
  scale_x_discrete(name = 'Time (h)', expand = c(0, 0)) +
  scale_y_continuous(name = 'Proportion', expand = c(0, 0)) +
  facet_wrap(~tank, nrow = 2, ncol = 5) +
  scale_fill_manual(values = delousepal, name = '% female lice \n remaining') +
  theme_classic() +
  ggtitle('Delousing rates - female lice')

dplot.m <- t3data %>%
  #filter(time != 0) %>%
  ggplot(aes(x = time, fill = dprop.m)) +
  geom_bar(position = 'fill', stat = 'count') +
  scale_x_discrete(name = 'Time (h)', expand = c(0, 0)) +
  scale_y_continuous(name = 'Proportion', expand = c(0, 0)) +
  facet_wrap(~tank, nrow = 2, ncol = 5) +
  scale_fill_manual(values = delousepal, name = '% male lice \n remaining') +
  theme_classic() +
  ggtitle('Delousing rates - male lice')

dplot.t <- t3data %>%
  #filter(time != 0) %>%
  ggplot(aes(x = time, fill = dprop.t)) +
  geom_bar(position = 'fill', stat = 'count') +
  scale_x_discrete(name = 'Time (h)', expand = c(0, 0)) +
  scale_y_continuous(name = 'Proportion', expand = c(0, 0)) +
  facet_wrap(~tank, nrow = 2, ncol = 5) +
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

t3locsum <- t3dmelt %>%
  group_by(time, replicate, location) %>%
  dplyr::summarise(mean.m = mean(male), sd.m = sd(male), mean.f = mean(female), sd.f = sd(female), mean.t = mean(total), sd.t = sd(total)) 

# total lice plots

# wrasse 1
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '0' & t3locsum$replicate == '1'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '0' & t3locsum$replicate == '1'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r10 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '48' & t3locsum$replicate == '1'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '48' & t3locsum$replicate == '1'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r148 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '96' & t3locsum$replicate == '1'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '96' & t3locsum$replicate == '1'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r196 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '144' & t3locsum$replicate == '1'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '144' & t3locsum$replicate == '1'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r1144 <- salplot


r1title <- ggdraw() +
  draw_label('Wrasse 1', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r1title, r10, r148, r196, r1144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


# wrasse 2
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '0' & t3locsum$replicate == '2'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '0' & t3locsum$replicate == '2'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r20 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '48' & t3locsum$replicate == '2'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '48' & t3locsum$replicate == '2'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r248 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '96' & t3locsum$replicate == '2'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '96' & t3locsum$replicate == '2'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r296 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '144' & t3locsum$replicate == '2'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '144' & t3locsum$replicate == '2'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r2144 <- salplot


r2title <- ggdraw() +
  draw_label('Wrasse 2', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r2title, r20, r248, r296, r2144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


# wrasse 3
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '0' & t3locsum$replicate == '3'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '0' & t3locsum$replicate == '3'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r30 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '48' & t3locsum$replicate == '3'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '48' & t3locsum$replicate == '3'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r348 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '96' & t3locsum$replicate == '3'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '96' & t3locsum$replicate == '3'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r396 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '144' & t3locsum$replicate == '3'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '144' & t3locsum$replicate == '3'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r3144 <- salplot


r3title <- ggdraw() +
  draw_label('Wrasse 3', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r3title, r30, r348, r396, r3144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


# wrasse 4
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '0' & t3locsum$replicate == '4'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '0' & t3locsum$replicate == '4'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r40 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '48' & t3locsum$replicate == '4'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '48' & t3locsum$replicate == '4'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r448 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '96' & t3locsum$replicate == '4'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '96' & t3locsum$replicate == '4'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r496 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '144' & t3locsum$replicate == '4'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '144' & t3locsum$replicate == '4'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r4144 <- salplot


r4title <- ggdraw() +
  draw_label('Wrasse 4', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r4title, r40, r448, r496, r4144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


# wrasse 5
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '0' & t3locsum$replicate == '5'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '0' & t3locsum$replicate == '5'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r50 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '48' & t3locsum$replicate == '5'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '48' & t3locsum$replicate == '5'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r548 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '96' & t3locsum$replicate == '5'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '96' & t3locsum$replicate == '5'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r596 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '144' & t3locsum$replicate == '5'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '144' & t3locsum$replicate == '5'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r5144 <- salplot


r5title <- ggdraw() +
  draw_label('Wrasse 5', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r5title, r50, r548, r596, r5144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 6
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '0' & t3locsum$replicate == '6'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '0' & t3locsum$replicate == '6'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r60 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '48' & t3locsum$replicate == '6'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '48' & t3locsum$replicate == '6'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r648 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '96' & t3locsum$replicate == '6'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '96' & t3locsum$replicate == '6'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r696 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '144' & t3locsum$replicate == '6'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '144' & t3locsum$replicate == '6'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r6144 <- salplot


r6title <- ggdraw() +
  draw_label('Wrasse 6', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r6title, r60, r648, r696, r6144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 7
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '0' & t3locsum$replicate == '7'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '0' & t3locsum$replicate == '7'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r70 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '48' & t3locsum$replicate == '7'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '48' & t3locsum$replicate == '7'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r748 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '96' & t3locsum$replicate == '7'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '96' & t3locsum$replicate == '7'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r796 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '144' & t3locsum$replicate == '7'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '144' & t3locsum$replicate == '7'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r7144 <- salplot


r7title <- ggdraw() +
  draw_label('Wrasse 7', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r7title, r70, r748, r796, r7144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 8
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '0' & t3locsum$replicate == '8'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '0' & t3locsum$replicate == '8'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r80 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '48' & t3locsum$replicate == '8'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '48' & t3locsum$replicate == '8'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r848 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '96' & t3locsum$replicate == '8'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '96' & t3locsum$replicate == '8'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r896 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '144' & t3locsum$replicate == '8'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '144' & t3locsum$replicate == '8'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r8144 <- salplot


r8title <- ggdraw() +
  draw_label('Wrasse 8', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r8title, r80, r848, r896, r8144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 9
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '0' & t3locsum$replicate == '9'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '0' & t3locsum$replicate == '9'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r90 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '48' & t3locsum$replicate == '9'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '48' & t3locsum$replicate == '9'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r948 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '96' & t3locsum$replicate == '9'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '96' & t3locsum$replicate == '9'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r996 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '144' & t3locsum$replicate == '9'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '144' & t3locsum$replicate == '9'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r9144 <- salplot


r9title <- ggdraw() +
  draw_label('Wrasse 9', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r9title, r90, r948, r996, r9144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 10
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '0' & t3locsum$replicate == '10'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '0' & t3locsum$replicate == '10'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r100 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '48' & t3locsum$replicate == '10'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '48' & t3locsum$replicate == '10'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r1048 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '96' & t3locsum$replicate == '10'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '96' & t3locsum$replicate == '10'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r1096 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.t[t3locsum$time == '144' & t3locsum$replicate == '10'],
                       lice.sd = t3locsum$sd.t[t3locsum$time == '144' & t3locsum$replicate == '10'])
map.lice(licelocs, 3.5, T, T, 'Total No. lice')
r10144 <- salplot


r10title <- ggdraw() +
  draw_label('Wrasse 10', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r10title, r100, r1048, r1096, r10144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)



# All treatments plot 1-5
r1plot <- plot_grid(r1title, r10, r148, r196, r1144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r2plot <- plot_grid(r2title, r20, r248, r296, r2144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r3plot <- plot_grid(r3title, r30, r348, r396, r3144, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r4plot <- plot_grid(r4title, r40, r448, r496, r4144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r5plot <- plot_grid(r5title, r50, r548, r596, r5144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


plot_grid(r1plot, r2plot, r3plot, r4plot, r5plot, nrow = 1, ncol = 5)

# All treatments plot 6-10
r6plot <- plot_grid(r6title, r60, r648, r696, r6144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r7plot <- plot_grid(r7title, r70, r748, r796, r7144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r8plot <- plot_grid(r8title, r80, r848, r896, r8144, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r9plot <- plot_grid(r9title, r90, r948, r996, r9144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r10plot <- plot_grid(r10title, r100, r1048, r1096, r10144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


plot_grid(r6plot, r7plot, r8plot, r9plot, r10plot, nrow = 1, ncol = 5)



# Female lice plots

# wrasse 1
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '0' & t3locsum$replicate == '1'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '0' & t3locsum$replicate == '1'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r10 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '48' & t3locsum$replicate == '1'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '48' & t3locsum$replicate == '1'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r148 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '96' & t3locsum$replicate == '1'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '96' & t3locsum$replicate == '1'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r196 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '144' & t3locsum$replicate == '1'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '144' & t3locsum$replicate == '1'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r1144 <- salplot


r1title <- ggdraw() +
  draw_label('Wrasse 1', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r1title, r10, r148, r196, r1144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


# wrasse 2
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '0' & t3locsum$replicate == '2'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '0' & t3locsum$replicate == '2'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r20 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '48' & t3locsum$replicate == '2'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '48' & t3locsum$replicate == '2'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r248 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '96' & t3locsum$replicate == '2'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '96' & t3locsum$replicate == '2'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r296 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '144' & t3locsum$replicate == '2'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '144' & t3locsum$replicate == '2'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r2144 <- salplot


r2title <- ggdraw() +
  draw_label('Wrasse 2', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r2title, r20, r248, r296, r2144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


# wrasse 3
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '0' & t3locsum$replicate == '3'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '0' & t3locsum$replicate == '3'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r30 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '48' & t3locsum$replicate == '3'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '48' & t3locsum$replicate == '3'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r348 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '96' & t3locsum$replicate == '3'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '96' & t3locsum$replicate == '3'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r396 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '144' & t3locsum$replicate == '3'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '144' & t3locsum$replicate == '3'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r3144 <- salplot


r3title <- ggdraw() +
  draw_label('Wrasse 3', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r3title, r30, r348, r396, r3144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


# wrasse 4
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '0' & t3locsum$replicate == '4'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '0' & t3locsum$replicate == '4'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r40 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '48' & t3locsum$replicate == '4'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '48' & t3locsum$replicate == '4'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r448 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '96' & t3locsum$replicate == '4'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '96' & t3locsum$replicate == '4'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r496 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '144' & t3locsum$replicate == '4'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '144' & t3locsum$replicate == '4'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r4144 <- salplot


r4title <- ggdraw() +
  draw_label('Wrasse 4', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r4title, r40, r448, r496, r4144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


# wrasse 5
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '0' & t3locsum$replicate == '5'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '0' & t3locsum$replicate == '5'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r50 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '48' & t3locsum$replicate == '5'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '48' & t3locsum$replicate == '5'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r548 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '96' & t3locsum$replicate == '5'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '96' & t3locsum$replicate == '5'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r596 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '144' & t3locsum$replicate == '5'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '144' & t3locsum$replicate == '5'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r5144 <- salplot


r5title <- ggdraw() +
  draw_label('Wrasse 5', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r5title, r50, r548, r596, r5144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 6
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '0' & t3locsum$replicate == '6'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '0' & t3locsum$replicate == '6'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r60 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '48' & t3locsum$replicate == '6'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '48' & t3locsum$replicate == '6'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r648 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '96' & t3locsum$replicate == '6'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '96' & t3locsum$replicate == '6'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r696 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '144' & t3locsum$replicate == '6'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '144' & t3locsum$replicate == '6'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r6144 <- salplot


r6title <- ggdraw() +
  draw_label('Wrasse 6', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r6title, r60, r648, r696, r6144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 7
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '0' & t3locsum$replicate == '7'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '0' & t3locsum$replicate == '7'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r70 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '48' & t3locsum$replicate == '7'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '48' & t3locsum$replicate == '7'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r748 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '96' & t3locsum$replicate == '7'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '96' & t3locsum$replicate == '7'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r796 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '144' & t3locsum$replicate == '7'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '144' & t3locsum$replicate == '7'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r7144 <- salplot


r7title <- ggdraw() +
  draw_label('Wrasse 7', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r7title, r70, r748, r796, r7144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 8
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '0' & t3locsum$replicate == '8'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '0' & t3locsum$replicate == '8'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r80 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '48' & t3locsum$replicate == '8'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '48' & t3locsum$replicate == '8'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r848 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '96' & t3locsum$replicate == '8'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '96' & t3locsum$replicate == '8'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r896 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '144' & t3locsum$replicate == '8'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '144' & t3locsum$replicate == '8'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r8144 <- salplot


r8title <- ggdraw() +
  draw_label('Wrasse 8', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r8title, r80, r848, r896, r8144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 9
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '0' & t3locsum$replicate == '9'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '0' & t3locsum$replicate == '9'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r90 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '48' & t3locsum$replicate == '9'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '48' & t3locsum$replicate == '9'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r948 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '96' & t3locsum$replicate == '9'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '96' & t3locsum$replicate == '9'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r996 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '144' & t3locsum$replicate == '9'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '144' & t3locsum$replicate == '9'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r9144 <- salplot


r9title <- ggdraw() +
  draw_label('Wrasse 9', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r9title, r90, r948, r996, r9144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 10
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '0' & t3locsum$replicate == '10'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '0' & t3locsum$replicate == '10'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r100 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '48' & t3locsum$replicate == '10'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '48' & t3locsum$replicate == '10'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r1048 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '96' & t3locsum$replicate == '10'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '96' & t3locsum$replicate == '10'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r1096 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.f[t3locsum$time == '144' & t3locsum$replicate == '10'],
                       lice.sd = t3locsum$sd.f[t3locsum$time == '144' & t3locsum$replicate == '10'])
map.lice(licelocs, 2.8, T, T, 'No. female lice')
r10144 <- salplot


r10title <- ggdraw() +
  draw_label('Wrasse 10', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r10title, r100, r1048, r1096, r10144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)



# All treatments plot 1-5
r1plot <- plot_grid(r1title, r10, r148, r196, r1144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r2plot <- plot_grid(r2title, r20, r248, r296, r2144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r3plot <- plot_grid(r3title, r30, r348, r396, r3144, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r4plot <- plot_grid(r4title, r40, r448, r496, r4144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r5plot <- plot_grid(r5title, r50, r548, r596, r5144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


plot_grid(r1plot, r2plot, r3plot, r4plot, r5plot, nrow = 1, ncol = 5)

# All treatments plot 6-10
r6plot <- plot_grid(r6title, r60, r648, r696, r6144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r7plot <- plot_grid(r7title, r70, r748, r796, r7144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r8plot <- plot_grid(r8title, r80, r848, r896, r8144, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r9plot <- plot_grid(r9title, r90, r948, r996, r9144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r10plot <- plot_grid(r10title, r100, r1048, r1096, r10144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                     labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


plot_grid(r6plot, r7plot, r8plot, r9plot, r10plot, nrow = 1, ncol = 5)



# Male lice plots


# wrasse 1
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '0' & t3locsum$replicate == '1'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '0' & t3locsum$replicate == '1'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r10 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '48' & t3locsum$replicate == '1'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '48' & t3locsum$replicate == '1'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r148 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '96' & t3locsum$replicate == '1'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '96' & t3locsum$replicate == '1'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r196 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '144' & t3locsum$replicate == '1'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '144' & t3locsum$replicate == '1'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r1144 <- salplot


r1title <- ggdraw() +
  draw_label('Wrasse 1', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r1title, r10, r148, r196, r1144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


# wrasse 2
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '0' & t3locsum$replicate == '2'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '0' & t3locsum$replicate == '2'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r20 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '48' & t3locsum$replicate == '2'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '48' & t3locsum$replicate == '2'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r248 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '96' & t3locsum$replicate == '2'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '96' & t3locsum$replicate == '2'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r296 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '144' & t3locsum$replicate == '2'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '144' & t3locsum$replicate == '2'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r2144 <- salplot


r2title <- ggdraw() +
  draw_label('Wrasse 2', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r2title, r20, r248, r296, r2144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


# wrasse 3
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '0' & t3locsum$replicate == '3'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '0' & t3locsum$replicate == '3'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r30 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '48' & t3locsum$replicate == '3'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '48' & t3locsum$replicate == '3'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r348 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '96' & t3locsum$replicate == '3'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '96' & t3locsum$replicate == '3'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r396 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '144' & t3locsum$replicate == '3'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '144' & t3locsum$replicate == '3'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r3144 <- salplot


r3title <- ggdraw() +
  draw_label('Wrasse 3', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r3title, r30, r348, r396, r3144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


# wrasse 4
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '0' & t3locsum$replicate == '4'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '0' & t3locsum$replicate == '4'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r40 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '48' & t3locsum$replicate == '4'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '48' & t3locsum$replicate == '4'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r448 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '96' & t3locsum$replicate == '4'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '96' & t3locsum$replicate == '4'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r496 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '144' & t3locsum$replicate == '4'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '144' & t3locsum$replicate == '4'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r4144 <- salplot


r4title <- ggdraw() +
  draw_label('Wrasse 4', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r4title, r40, r448, r496, r4144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


# wrasse 5
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '0' & t3locsum$replicate == '5'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '0' & t3locsum$replicate == '5'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r50 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '48' & t3locsum$replicate == '5'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '48' & t3locsum$replicate == '5'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r548 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '96' & t3locsum$replicate == '5'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '96' & t3locsum$replicate == '5'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r596 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '144' & t3locsum$replicate == '5'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '144' & t3locsum$replicate == '5'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r5144 <- salplot


r5title <- ggdraw() +
  draw_label('Wrasse 5', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r5title, r50, r548, r596, r5144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 6
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '0' & t3locsum$replicate == '6'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '0' & t3locsum$replicate == '6'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r60 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '48' & t3locsum$replicate == '6'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '48' & t3locsum$replicate == '6'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r648 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '96' & t3locsum$replicate == '6'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '96' & t3locsum$replicate == '6'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r696 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '144' & t3locsum$replicate == '6'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '144' & t3locsum$replicate == '6'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r6144 <- salplot


r6title <- ggdraw() +
  draw_label('Wrasse 6', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r6title, r60, r648, r696, r6144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 7
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '0' & t3locsum$replicate == '7'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '0' & t3locsum$replicate == '7'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r70 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '48' & t3locsum$replicate == '7'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '48' & t3locsum$replicate == '7'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r748 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '96' & t3locsum$replicate == '7'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '96' & t3locsum$replicate == '7'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r796 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '144' & t3locsum$replicate == '7'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '144' & t3locsum$replicate == '7'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r7144 <- salplot


r7title <- ggdraw() +
  draw_label('Wrasse 7', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r7title, r70, r748, r796, r7144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 8
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '0' & t3locsum$replicate == '8'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '0' & t3locsum$replicate == '8'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r80 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '48' & t3locsum$replicate == '8'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '48' & t3locsum$replicate == '8'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r848 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '96' & t3locsum$replicate == '8'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '96' & t3locsum$replicate == '8'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r896 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '144' & t3locsum$replicate == '8'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '144' & t3locsum$replicate == '8'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r8144 <- salplot


r8title <- ggdraw() +
  draw_label('Wrasse 8', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r8title, r80, r848, r896, r8144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 9
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '0' & t3locsum$replicate == '9'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '0' & t3locsum$replicate == '9'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r90 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '48' & t3locsum$replicate == '9'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '48' & t3locsum$replicate == '9'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r948 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '96' & t3locsum$replicate == '9'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '96' & t3locsum$replicate == '9'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r996 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '144' & t3locsum$replicate == '9'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '144' & t3locsum$replicate == '9'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r9144 <- salplot


r9title <- ggdraw() +
  draw_label('Wrasse 9', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r9title, r90, r948, r996, r9144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

# wrasse 10
licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '0' & t3locsum$replicate == '10'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '0' & t3locsum$replicate == '10'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r100 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '48' & t3locsum$replicate == '10'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '48' & t3locsum$replicate == '10'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r1048 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '96' & t3locsum$replicate == '10'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '96' & t3locsum$replicate == '10'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r1096 <- salplot

licelocs <- data.frame(location = unique(t3locsum$location), 
                       lice.m = t3locsum$mean.m[t3locsum$time == '144' & t3locsum$replicate == '10'],
                       lice.sd = t3locsum$sd.m[t3locsum$time == '144' & t3locsum$replicate == '10'])
map.lice(licelocs, 2.0, T, T, 'No. male lice')
r10144 <- salplot


r10title <- ggdraw() +
  draw_label('Wrasse 10', fontface = 'bold', x = 0.5, hjust = 0.5, size = 12)

plot_grid(r10title, r100, r1048, r1096, r10144, liceleg, nrow = 6, ncol = 1, rel_heights = c(0.05, rep(0.17, 4), 0.1), 
          labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)



# All treatments plot 1-5
r1plot <- plot_grid(r1title, r10, r148, r196, r1144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r2plot <- plot_grid(r2title, r20, r248, r296, r2144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r3plot <- plot_grid(r3title, r30, r348, r396, r3144, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r4plot <- plot_grid(r4title, r40, r448, r496, r4144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r5plot <- plot_grid(r5title, r50, r548, r596, r5144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


plot_grid(r1plot, r2plot, r3plot, r4plot, r5plot, nrow = 1, ncol = 5)

# All treatments plot 6-10
r6plot <- plot_grid(r6title, r60, r648, r696, r6144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r7plot <- plot_grid(r7title, r70, r748, r796, r7144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r8plot <- plot_grid(r8title, r80, r848, r896, r8144, liceleg, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r9plot <- plot_grid(r9title, r90, r948, r996, r9144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                    labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)

r10plot <- plot_grid(r10title, r100, r1048, r1096, r10144, nrow = 7, ncol = 1, rel_heights = c(0.05, rep(0.2125, 4), 0.1), 
                     labels = c('', ' 0h', '48h', '96h', '144h'), label_size = 12, vjust = 4, hjust = -2)


plot_grid(r6plot, r7plot, r8plot, r9plot, r10plot, nrow = 1, ncol = 5)

