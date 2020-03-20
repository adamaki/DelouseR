# Novel object test behaviour analysis
# Copyright Adam Brooker 
# 29th February 2020



library(tidyverse)

source('/Users/adambrooker/R Projects/DelouseR/Delousing-initiate.R')
#source('G:/Projects/Lumpfish delousing/Delousing-initiate.R')

t3data <- read.csv('/Users/adambrooker/Dropbox/cleanerfish/Current/Delousing - individual wrasse/DelousingTrial3-IndWrasse.csv') # Load lice data
#t3data <- read.csv('G:/Projects/Lumpfish delousing/Data/T3 Individual Wrasse/DelousingTrial3-IndWrasse.csv') # Load lice data

t3data <- t3data %>% mutate(total.m = t3data %>% rowwise() %>% select(contains('.m')) %>% rowSums()) # new total male col
t3data <- t3data %>% mutate(total.f = t3data %>% rowwise() %>% select(contains('.f')) %>% rowSums()) # new total female col
t3data$total <- t3data$total.m + t3data$total.f # new total lice col
t3data$time <- dplyr::recode(t3data$time, '1' = 0, '2' = 48, '3' = 96, '4' = 144)


#workingdir <- 'G:/Data/Cleaner fish delousing/Novel Object videos/Individual wrasse' # change to location of data
workingdir <- '/Users/adambrooker/Dropbox/cleanerfish/Current/Delousing - individual wrasse' # change to location of data

setwd(workingdir)

files <- list.files(path = workingdir, pattern = 'C.*.csv', all.files = FALSE, recursive = FALSE)


# Read behaviour files into one dataframe
tdat <- data.frame(frame = numeric(0), fish.px = numeric(0), fish.py = numeric(0), distno.p = numeric(0), fish.rx = numeric(0), fish.ry = numeric(0), distno.r = numeric(0), ID = character(0))

for(i in 1:length(files)){
  
 df <- read.csv(files[i]) 
 df <- select(df, -errors)
 df$frame <- seq(1, nrow(df), 1)
 #df$ID <- str_sub(files[i], 1, 2)
 df$ID <- sub('\\-.*', '', files[i])
 
 tdat <- rbind(tdat, df)
 rm(df)
}

tdat$ID <- factor(tdat$ID, levels = c('C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C12')) # convert ID to factor
delouse_key <- c(C2 = 'Good', C3 = 'Poor', C4 = 'Good', C5 = 'Excellent', C6 = 'Poor', C7 = 'Poor', C8 = 'Good', C9 = 'None', C10 = 'Poor', C12 = 'None') # create delousing ability levels
tdat$ability <- recode(tdat$ID, !!!delouse_key) # add delousing ability column
tdat$ability <- factor(tdat$ability, levels = c('Excellent', 'Good', 'Poor', 'None')) # code delousing ability
tdat$ab_code <- as.numeric(recode_factor(tdat$ability, 'None' = 1, 'Poor' = 2, 'Good' = 3, 'Excellent' = 4)) # code delousing ability as value

tdat <- tdat[,c(8, 9, 1, 2, 3, 4, 5, 6, 7)]

# calculate velocity
tdat$sec <- c(NA, rep(1, 299))
tdat$cmsec <- round(c(0, (sqrt(diff(tdat$fish.rx)^2+diff(tdat$fish.ry)^2)))/tdat$sec, digits = 3) # calculate swimming speed in cm/s
tdat$sec <- NULL

# calculate heading and direction
heading.func(thresh = 1.5)
tdat$head <- headvec
tdat$dir <- ifelse(tdat$head > 315 | tdat$head < 44.999, 'N', 
                   ifelse(tdat$head > 45 & tdat$head < 134.999, 'E', 
                          ifelse(tdat$head > 135 & tdat$head < 224.999, 'S', 
                                 ifelse(tdat$head > 225 & tdat$head < 314.999, 'W', NA)))) # Code fish heading column
rm(headvec)

tdat$move <- ifelse(is.na(tdat$cmsec), NA, ifelse(is.na(tdat$head), 'S', 'M')) # column for fish moving static or moving

# calculate fish eye looking at novel object
nocoords <- c(71, 50)
eye <- character()

for (i in 1:nrow(tdat)){
  
  if(tdat$fish.rx[i] >= nocoords[1] & tdat$fish.ry[i] < nocoords[2]){
    
    eye <- c(eye, ifelse(tdat$dir[i] == 'N' | tdat$dir[i] == 'W', 'L', 'R'))
    
  } else {
    
    if(tdat$fish.rx[i] >= nocoords[1] & tdat$fish.ry[i] >= nocoords[2]){
      
      eye <- c(eye, ifelse(tdat$dir[i] == 'N' | tdat$dir[i] == 'E', 'L', 'R'))
    
    } else {
        
      if(tdat$fish.rx[i] < nocoords[1] & tdat$fish.ry[i] >= nocoords[2]){
        
        eye <- c(eye, ifelse(tdat$dir[i] == 'S' | tdat$dir[i] == 'E', 'L', 'R'))
        
      } else {
        
        if(tdat$fish.rx[i] < nocoords[1] & tdat$fish.ry[i] < nocoords[2]){
          
          eye <- c(eye, ifelse(tdat$dir[i] == 'S' | tdat$dir[i] == 'W', 'L', 'R'))
          
        }
        
      }
      
    }
    
  }
  
}

tdat$eye <- eye
rm(eye)

tdat <- tdat %>% 
  dplyr::group_by(ID) %>% 
  tidyr::fill(eye, dir, .direction = 'down') %>%
  tidyr::fill(eye, dir, .direction = 'up') # fill down direction and eye when fish not moving

tdat$eye <- ifelse(tdat$ID == 'C3', 'L', tdat$eye) # manually code left eye for fish C3 (static during NO test)

tdat$dir <- ifelse(is.na(tdat$cmsec), NA, tdat$dir) # change 1st row of each fish back to NA
tdat$eye <- ifelse(is.na(tdat$cmsec), NA, tdat$eye) # change 1st row of each fish back to NA

tdat$move_eye <- ifelse(is.na(tdat$eye), NA, paste0(tdat$move, '_', tdat$eye)) # new column for moving and static left and right eye

# Calculate percent lice reduction from starting number of lice at each time point

reduc <- t3data %>%
  dplyr::group_by(time, tank) %>%
  dplyr::summarise(start.m = sum(total.m), start.f = sum(total.f), start.t = sum(total.m + total.f)) %>%
  filter(time == '0') %>%
  ungroup() %>%
  select(-time)

reduc <- t3data %>%
  group_by(time, tank) %>%
  filter(time == '48') %>%
  dplyr::summarise(m.48 = sum(total.m), f.48 = sum(total.f), t.48 = sum(total.m + total.f)) %>%
  ungroup() %>%
  select(-time) %>%
  left_join(., reduc, by = 'tank')

reduc <- t3data %>%
  group_by(time, tank) %>%
  filter(time == '96') %>%
  dplyr::summarise(m.96 = sum(total.m), f.96 = sum(total.f), t.96 = sum(total.m + total.f)) %>%
  ungroup() %>%
  select(-time) %>%
  left_join(., reduc, by = 'tank')

reduc <- t3data %>%
  group_by(time, tank) %>%
  filter(time == '144') %>%
  dplyr::summarise(m.144 = sum(total.m), f.144 = sum(total.f), t.144 = sum(total.m + total.f)) %>%
  ungroup() %>%
  select(-time) %>%
  left_join(., reduc, by = 'tank')

# calculate percent reduction from lice at start
reduc <- reduc %>%
  mutate(m.48 = round((m.48/start.m)*100), f.48 = round((f.48/start.f)*100), t.48 = round((t.48/start.t)*100),
         m.96 = round((m.96/start.m)*100), f.96 = round((f.96/start.f)*100), t.96 = round((t.96/start.t)*100),
         m.144 = round((m.144/start.m)*100), f.144 = round((f.144/start.f)*100), t.144 = round((t.144/start.t)*100))

reduc <- dplyr::rename(reduc, ID = tank)
reduc$ID <- factor(reduc$ID, levels = c('C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C12')) # convert ID to factor
reduc <- select(reduc, 1:10)

tdat <- tdat %>%
  left_join(., reduc[,c(1, 8, 9, 10, 5, 6, 7, 2, 3, 4)], by = 'ID') # add lice reduction columns to behaviour dataframe

# Calculate summary dataset for PCA and correlation analysis----------------------------------------

bsum <- reduc[,c(1, 8, 9, 10, 5, 6, 7, 2, 3, 4)]
bsum <- arrange(bsum, ID)

# summarise novel object distance and velocity and add to summary dataframe
bsum <- tdat %>%
  group_by(ID) %>%
  dplyr::summarise(NO.dist.mean = round(mean(distno.r, na.rm = T), 2),
            NO.dist.min = round(min(distno.r, na.rm = T), 2),
            vel.mean = round(mean(cmsec, na.rm = T), 2),
            vel.max = round(max(cmsec, na.rm = T), 2)) %>%
  select(ID, NO.dist.mean, NO.dist.min, vel.mean, vel.max) %>%
  left_join(bsum, ., by = 'ID')
  
# summarise movement variables and add to summary dataframe
bsum <- tdat %>%
  group_by(ID) %>%
  dplyr::count(move, name = 't.move') %>%
  filter(move == 'M') %>%
  select (ID, t.move) %>%
  left_join(bsum, ., by = 'ID') %>%
  mutate(t.move = tidyr::replace_na(t.move, 0))
  
bsum <- tdat %>%
  group_by(ID) %>%
  dplyr::count(move, name = 't.static') %>%
  filter(move == 'S') %>%
  select (ID, t.static) %>%
  left_join(bsum, ., by = 'ID') %>%
  mutate(t.static = tidyr::replace_na(t.static, 0))

# count movement bouts and add summary data to bsum

bout.df <- data.frame(ID = factor(), bout.n.s = numeric(), bout.n.m = numeric(), bout.m.s = numeric(), bout.m.m = numeric())

for(i in 1:length(unique(tdat$ID))){
  
  mvec <- tdat$move[tdat$ID == unique(tdat$ID)[[i]]]
  mvec <- mvec[!is.na(mvec)]
  mvec <- data.frame('move' = mvec, 'bout' = rep(NA, length(mvec)))
  
  mvec[1,'bout'] <- 1
  
  # add bout length column
  for(j in 2:nrow(mvec)){
    
    if(mvec[j,'move'] == mvec[j-1,'move'])
    {
      mvec[j,'bout'] <- mvec[j-1,'bout'] + 1
    } else {
        mvec[j,'bout'] <- 1
      }
  }
  
  # calculate number of static bouts
  if(mvec[1,'move'] ==  'S'){
    n.s <- floor((sum(diff(mvec$bout) != 1))/2) + 1
  } else{
    n.s <- floor((sum(diff(mvec$bout) != 1))/2)
  }
  
  # calculate number of moving bouts
  if(mvec[1,'move'] ==  'M'){
    n.m <- floor((sum(diff(mvec$bout) != 1))/2) + 1
  } else{
    n.m <- floor((sum(diff(mvec$bout) != 1))/2)
  }
  
  m.s <- round(nrow(mvec[mvec$move == 'S',])/n.s, 2) # calculate mean length of static bount
  m.m <- round(nrow(mvec[mvec$move == 'M',])/n.m, 2) # calculate mean length of moving bout
  
  bout.df <- add_row(bout.df, ID = unique(tdat$ID)[i], bout.n.s = n.s, bout.n.m = n.m, bout.m.s = m.s, bout.m.m = m.m)
  bout.df$bout.m.s[is.nan(bout.df$bout.m.s)] <- 0
  bout.df$bout.m.m[is.nan(bout.df$bout.m.m)] <- 0
  
}

bsum <- bsum %>% left_join(bout.df, by = 'ID')
rm(bout.df)


# summarise eye variables and add to summary dataframe
bsum <- tdat %>%
  group_by(ID) %>%
  dplyr::count(eye, name = 'eye.l') %>%
  filter(eye == 'L') %>%
  select (ID, eye.l) %>%
  left_join(bsum, ., by = 'ID') %>%
  mutate(eye.l = tidyr::replace_na(eye.l, 0))

bsum <- tdat %>%
  group_by(ID) %>%
  dplyr::count(eye, name = 'eye.r') %>%
  filter(eye == 'R') %>%
  select (ID, eye.r) %>%
  left_join(bsum, ., by = 'ID') %>%
  mutate(eye.r = tidyr::replace_na(eye.r, 0))


# count eye bouts and add summary to bsum dataframe

bout.df <- data.frame(ID = factor(), bout.n.l = numeric(), bout.n.r = numeric(), bout.m.l = numeric(), bout.m.r = numeric())

for(i in 1:length(unique(tdat$ID))){
  
  mvec <- tdat$eye[tdat$ID == unique(tdat$ID)[[i]]]
  mvec <- mvec[!is.na(mvec)]
  mvec <- data.frame('eye' = mvec, 'bout' = rep(NA, length(mvec)))
  
  mvec[1,'bout'] <- 1
  
  # add bout length column
  for(j in 2:nrow(mvec)){
    
    if(mvec[j,'eye'] == mvec[j-1,'eye'])
    {
      mvec[j,'bout'] <- mvec[j-1,'bout'] + 1
    } else {
      mvec[j,'bout'] <- 1
    }
  }
  
  # calculate number of left eye bouts
  if(mvec[1,'eye'] ==  'L'){
    n.l <- floor((sum(diff(mvec$bout) != 1))/2) + 1
  } else{
    n.l <- floor((sum(diff(mvec$bout) != 1))/2)
  }
  
  # calculate number of right eye bouts
  if(mvec[1,'eye'] ==  'R'){
    n.r <- floor((sum(diff(mvec$bout) != 1))/2) + 1
  } else{
    n.r <- floor((sum(diff(mvec$bout) != 1))/2)
  }
  
  m.l <- round(nrow(mvec[mvec$eye == 'L',])/n.l, 2) # calculate mean length of static bount
  m.r <- round(nrow(mvec[mvec$eye == 'R',])/n.r, 2) # calculate mean length of moving bout
  
  bout.df <- add_row(bout.df, ID = unique(tdat$ID)[i], bout.n.l = n.l, bout.n.r = n.r, bout.m.l = m.l, bout.m.r = m.r)
  bout.df$bout.m.l[is.nan(bout.df$bout.m.l)] <- 0
  bout.df$bout.m.r[is.nan(bout.df$bout.m.r)] <- 0
  
}

bsum <- bsum %>% left_join(bout.df, by = 'ID')
rm(bout.df)





# Plots=============================================================================

# Plot fish tracks with tank and novel object outlines
ggplot(data = tdat, aes(x = fish.rx, y = fish.ry)) +
  geom_path() +
  scale_y_reverse(name = 'cm') +
  scale_x_continuous(name = 'cm') +
  facet_wrap(~ID) +
  annotate("path", x = 50+50*cos(seq(0,2*pi,length.out=100)), y = 50+50*sin(seq(0,2*pi,length.out=100)), colour = 'blue') +
  annotate('rect', xmin = 68, xmax = 73.5, ymin = 47.5, ymax = 53)


# plot velocity for each fish
ggplot(data = tdat, aes(x = frame, y = cmsec, colour = ability)) +
  geom_line() +
  facet_wrap(~ID)

# plot distance from object for each fish
ggplot(data = tdat, aes(x = frame, y = distno.r, colour = ability)) +
  geom_line() +
  facet_wrap(~ID)

# plot heading for each fish
ggplot(data = tdat, aes(x = frame, y = head, colour = ability)) +
  geom_line() +
  facet_wrap(~ID)

# histogram of distance from object for each fish
ggplot(data = tdat, aes(x = distno.r, fill = ability)) +
  geom_histogram() +
  facet_wrap(~ID)

# summary of mean velocity and distance from novel object for each fish
tdat %>%
  group_by(ID, ability) %>%
  dplyr::summarise(mean_vel = mean(cmsec, na.rm = T), mean_dist = mean(distno.r)) %>%
  ggplot(aes(x = ID)) +
  geom_bar(aes(y = mean_vel, fill = ability), stat = 'identity')

tdat %>%
  group_by(ID, ability) %>%
  dplyr::summarise(mean_vel = mean(cmsec, na.rm = T), mean_dist = mean(distno.r)) %>%
  ggplot(aes(x = ID)) +
  geom_bar(aes(y = mean_dist, fill = ability), stat = 'identity')

# boxplot of distance from novel object for each fish
ggplot(data = tdat, aes(x = ID, y = distno.r, colour = ability)) +
  geom_boxplot()

# scatterplot of distance from novel object vs. swimming speed
tdat %>%
  filter(cmsec > 2) %>%
  #filter(ID == 'C9') %>%
  ggplot(aes(x = distno.r, y = cmsec, colour = ability)) +
  geom_point() +
  stat_smooth(method = lm) +
  facet_wrap(~ID)

# barplot of viewing moving and static time
tdat %>%
  filter(!is.na(cmsec)) %>%
  #ggplot(aes(x = fct_reorder(ID, ab_code))) +
  ggplot(aes(x = fct_reorder(ID, t.144, .desc = T))) +
  geom_bar(aes(fill = move)) +
  geom_point(aes(y = t.144)) +
  scale_y_continuous(name = 'secs', limits = c(0, 300), breaks = c(0, 60, 120, 180, 240, 300), expand = c(0, 0)) +
  scale_x_discrete(name = 'fish ID') +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#AA5257", "#7D87B9"), labels = c('Moving', 'Static'))


# barplot of viewing novel object with left or right eye
tdat %>%
  filter(!is.na(cmsec)) %>%
  ggplot(aes(x = fct_reorder(ID, t.144, .desc = T))) +
  geom_bar(aes(fill = eye)) +
  scale_y_continuous(name = 'secs', limits = c(0, 300), breaks = c(0, 60, 120, 180, 240, 300), expand = c(0, 0)) +
  scale_x_discrete(name = 'fish ID') +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#AA5257", "#8A1923"), labels = c('Left eye', 'Right eye'))


# barplot of viewing novel object with left or right eye while static or moving
tdat %>%
  filter(!is.na(cmsec)) %>%
  ggplot(aes(x = fct_reorder(ID, t.144, .desc = T))) +
  geom_bar(aes(fill = move_eye)) +
  scale_y_continuous(name = 'secs', limits = c(0, 300), breaks = c(0, 60, 120, 180, 240, 300), expand = c(0, 0)) +
  scale_x_discrete(name = 'fish ID') +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#AA5257", "#8A1923", "#023FA5", "#7D87B9"), labels = c('Left eye, moving', 'Right eye, moving', 'Left eye, static', 'Right eye, static'))

# barplot of viewing novel object with left or right eye while static
tdat %>%
  filter(!is.na(cmsec)) %>%
  filter(move == 'S') %>%
  group_by(ID, t.144) %>%
  dplyr::count(move_eye) %>%
  ggplot(aes(x = fct_reorder(ID, t.144, .desc = T), y = n)) +
  geom_bar(aes(fill = move_eye), position = 'fill', stat = 'identity') +
  scale_y_continuous(name = 'Time', expand = c(0, 0), labels = scales::percent_format()) +
  scale_x_discrete(name = 'fish ID') +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#023FA5", "#7D87B9"), labels = c('Left eye, static', 'Right eye, static'))

# barplot of viewing novel object with left or right eye while moving
tdat %>%
  filter(!is.na(cmsec)) %>%
  filter(move == 'M') %>%
  #group_by(ID, ab_code) %>%
  group_by(ID, t.144) %>%
  dplyr::count(move_eye) %>%
  #ggplot(aes(x = fct_reorder(ID, ab_code), y = n)) +
  ggplot(aes(x = fct_reorder(ID, t.144, .desc = T), y = n)) +
  geom_bar(aes(fill = move_eye), position = 'fill', stat = 'identity') +
  scale_y_continuous(name = 'Time', expand = c(0, 0), labels = scales::percent_format()) +
  scale_x_discrete(name = 'fish ID') +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#AA5257", "#8A1923"), labels = c('Left eye, moving', 'Right eye, moving'))






