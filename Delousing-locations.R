# Cleaner fish tank delousing
# Adam Brooker 29th August 2019


setwd('G:/Projects/Lumpfish delousing/Data')
t5data <- read.csv('DelousingTrial5.csv')

t5data$time <- dplyr::recode(t5data$time, '1' = 0, '2' = 24, '3' = 48, '4' = 72, '5' = 96)
t5data$treatment <- dplyr::recode(t5data$treatment, 'con' = 'control', 'LL' = 'Large lumpfish', 'SL' = 'Small lumpfish', 'W' = 'Wrasse')

t5data <- t5data %>% mutate(total.m = t5data %>% rowwise() %>% select(contains('.m')) %>% rowSums()) # new total male col
t5data <- t5data %>% mutate(total.f = t5data %>% rowwise() %>% select(contains('.f')) %>% rowSums()) # new total female col

t5data <- arrange(t5data, time, treatment, replicate) # arrange data

# melt data into long format
males <- t5data %>% 
          melt(id = c('time', 'tank', 'treatment', 'replicate','fish'), value.name = 'male') %>%
          filter(grepl('.m', variable, fixed = T)) %>%
          filter(variable != 'total.m' & variable != 'bucket.m') %>%
          dplyr::rename(location = variable)

females <- t5data %>%
            melt(id = c('time', 'tank', 'treatment', 'replicate','fish'), value.name = 'female') %>%
            filter(grepl('.f', variable, fixed = T)) %>%
            filter(variable != 'total.f' & variable != 'bucket.f')

t5melt <- bind_cols(males, female = females$female)
t5melt$location <- str_sub(t5melt$location, end = -3)
t5melt$location <- as.factor(t5melt$location)
t5melt$location <- factor(t5melt$location, levels(t5melt$location)[c(2, 8, 11, 1, 3, 4, 5, 6, 7, 10, 12, 13, 9)])
t5melt$total <- t5melt$male + t5melt$female
rm(males, females)


t5locsum <- t5melt %>%
  group_by(time, treatment, replicate, location) %>%
  dplyr::summarise(mean.m = mean(male), sd.m = sd(male), mean.f = mean(female), sd.f = sd(female), mean.t = mean(total), sd.t = sd(total)) 

ggplot(t5locsum, aes(x = time, y = mean.t, fill = location)) +
  geom_bar(stat = 'identity', position = 'stack') +
  facet_wrap(~treatment)


t5melt %>%
  group_by(time, treatment, replicate, location) %>%
  dplyr::summarise(mean.m = mean(male), sd.m = sd(male), mean.f = mean(female), sd.f = sd(female)) %>%
  ggplot(aes(x = time, y = mean.f, fill = location)) +
  geom_bar(stat = 'identity', position = 'fill') +
  facet_wrap(~treatment)



