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

# load tracking data
#workingdir <- 'G:/Data/Cleaner fish delousing/Novel Object videos/Individual wrasse' # change to location of data
#wrasse videos
workingdir <- '/Users/adambrooker/Dropbox/cleanerfish/Current/Delousing - individual wrasse' # change to location of data
workingdir <- '/Users/adambrooker/OneDrive - University of Stirling/Lumpfish NO test' # change to location of data
#lumpfish videos
workingdir <- '/Users/adambrooker/OneDrive - University of Stirling/Lumpfish NO test'

setwd(workingdir)

files <- list.files(path = workingdir, pattern = 'C.*.csv', all.files = FALSE, recursive = FALSE)


# Read behaviour files into one dataframe
tdat <- data.frame(frame = numeric(0), fish.px = numeric(0), fish.py = numeric(0), distno.p = numeric(0), fish.rx = numeric(0), fish.ry = numeric(0), distno.r = numeric(0), ID = character(0))

for(i in 1:length(files)){
  
 df <- read.csv(files[i]) 
 df <- select(df, -errors)
 df$frame <- seq(1, nrow(df), 1)
 #df$ID <- str_sub(files[i], 1, 2)
 #df$ID <- sub('\\-.*', '', files[i])
 df$ID <- sub('\\..*$', '', files[i])
 
 tdat <- rbind(tdat, df)
 rm(df)
}

tdat <- filter(tdat, ID != 'C8') # remove lumpfish 8, which died during delousing trial

tdat$ID <- factor(tdat$ID, levels = c('C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C12')) # convert wrasse ID to factor
tdat$ID <- factor(tdat$ID, levels = c('C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C9', 'C10', 'C11', 'C12')) # convert lumpfish ID to factor
delouse_key <- c(C2 = 'Good', C3 = 'Poor', C4 = 'Good', C5 = 'Excellent', C6 = 'Poor', C7 = 'Poor', C8 = 'Good', C9 = 'None', C10 = 'Poor', C12 = 'None') # create wrasse delousing ability levels
delouse_key <- c(C1 = 'Poor', C2 = 'Poor', C3 = 'Poor', C4 = 'Moderate', C5 = 'Poor', C6 = 'Poor', C7 = 'Good', C9 = 'Good', C10 = 'Poor', C11 = 'Poor', C12 = 'Poor') # create lumpfish delousing ability levels
tdat$ability <- recode(tdat$ID, !!!delouse_key) # add delousing ability column
tdat$ability <- factor(tdat$ability, levels = c('Excellent', 'Good', 'Moderate', 'Poor')) # code delousing ability
tdat$ab_code <- as.numeric(recode_factor(tdat$ability, 'Poor' = 1, 'Moderate' = 2, 'Good' = 3, 'Excellent' = 4)) # code delousing ability as value

tdat <- tdat[,c(8, 9, 1, 2, 3, 4, 5, 6, 7)]

# separate acclimation and novel object experiment phases
tdatac <- filter(tdat, is.na(distno.p) == T) %>% select(-distno.p, -distno.r)
tdat <- filter(tdat, is.na(distno.p) == F)

# Calculate instantaneous variables for novel object phase----------------

# calculate velocity
tdat$sec <- c(NA, rep(1, 299))
tdat$cmsec <- round(c(0, (sqrt(diff(tdat$fish.rx)^2+diff(tdat$fish.ry)^2)))/tdat$sec, digits = 3) # calculate swimming speed in cm/s
tdat$sec <- NULL

# calculate heading and direction
heading.func(tdat, thresh = 1.5)
tdat$head <- headvec
tdat$dir <- ifelse(tdat$head > 315 | tdat$head < 44.999, 'N', 
                   ifelse(tdat$head > 45 & tdat$head < 134.999, 'E', 
                          ifelse(tdat$head > 135 & tdat$head < 224.999, 'S', 
                                 ifelse(tdat$head > 225 & tdat$head < 314.999, 'W', NA)))) # Code fish heading column
rm(headvec)
tdat$dir <- ifelse(is.na(tdat$cmsec), NA, tdat$dir) # change 1st row of each fish back to NA

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

tdat$eye <- ifelse(tdat$ID == 'C3', 'L', tdat$eye) # manually code left eye for wrasse C3 (static during NO test)

tdat$eye <- ifelse(is.na(tdat$cmsec), NA, tdat$eye) # change 1st row of each fish back to NA

tdat$move_eye <- ifelse(is.na(tdat$eye), NA, paste0(tdat$move, '_', tdat$eye)) # new column for moving and static left and right eye

tdat$phase <- 'NO'

# Calculate instantaneous variables for acclimation phase---------------------

# calculate velocity
tdatac$sec <- c(NA, rep(1, 299))
tdatac$cmsec <- round(c(0, (sqrt(diff(tdatac$fish.rx)^2+diff(tdatac$fish.ry)^2)))/tdatac$sec, digits = 3) # calculate swimming speed in cm/s
tdatac$sec <- NULL

# calculate heading and direction
heading.func(tdatac, thresh = 1.5)
tdatac$head <- headvec
tdatac$dir <- ifelse(tdatac$head > 315 | tdatac$head < 44.999, 'N', 
                   ifelse(tdatac$head > 45 & tdatac$head < 134.999, 'E', 
                          ifelse(tdatac$head > 135 & tdatac$head < 224.999, 'S', 
                                 ifelse(tdatac$head > 225 & tdatac$head < 314.999, 'W', NA)))) # Code fish heading column
rm(headvec)
tdatac$dir <- ifelse(is.na(tdatac$cmsec), NA, tdatac$dir) # change 1st row of each fish back to NA

tdatac$move <- ifelse(is.na(tdatac$cmsec), NA, ifelse(is.na(tdatac$head), 'S', 'M')) # column for fish moving static or moving

tdatac$phase <- 'Acc'


# Calculate percent lice reduction from starting number of lice at each time point-----------------

licedata <- t3data # pass wrassse lice data to df
licedata <- t5data # pass lumpfish lice data to df

reduc <- licedata %>%
  dplyr::group_by(time, tank) %>%
  dplyr::summarise(start.m = sum(total.m), start.f = sum(total.f), start.t = sum(total.m + total.f)) %>%
  filter(time == '0') %>%
  ungroup() %>%
  select(-time)

reduc <- licedata %>%
  group_by(time, tank) %>%
  filter(time == '48') %>%
  dplyr::summarise(m.48 = sum(total.m), f.48 = sum(total.f), t.48 = sum(total.m + total.f)) %>%
  ungroup() %>%
  select(-time) %>%
  left_join(., reduc, by = 'tank')

reduc <- licedata %>%
  group_by(time, tank) %>%
  filter(time == '96') %>%
  dplyr::summarise(m.96 = sum(total.m), f.96 = sum(total.f), t.96 = sum(total.m + total.f)) %>%
  ungroup() %>%
  select(-time) %>%
  left_join(., reduc, by = 'tank')

reduc <- licedata %>%
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
reduc$ID <- factor(reduc$ID, levels = c('C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C12')) # convert ID to factor wrasse trial
reduc$ID <- factor(reduc$ID, levels = c('C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C9', 'C10', 'C11', 'C12')) # convert ID to factor lumpfish trial

reduc <- select(reduc, 1:10)

tdat <- tdat %>%
  left_join(., reduc[,c(1, 8, 9, 10, 5, 6, 7, 2, 3, 4)], by = 'ID') # add lice reduction columns to behaviour dataframe

# Calculate summary dataset for PCA and correlation analysis----------------------------------------
# bsum = behaviour summary df

bsum <- reduc[,c(1, 8, 9, 10, 5, 6, 7, 2, 3, 4)]
bsum <- arrange(bsum, ID)

bsum$ability <- recode(bsum$ID, !!!delouse_key) # add delousing ability column
bsum$ability <- factor(bsum$ability, levels = c('Excellent', 'Good', 'Moderate', 'Poor')) # code delousing ability

# summarise novel object distance and velocity and add to summary dataframe
bsum <- tdat %>%
  group_by(ID) %>%
  dplyr::summarise(NO.dist.mean = round(mean(distno.r, na.rm = T), 2),
            NO.dist.min = round(min(distno.r, na.rm = T), 2),
            vel.mean = round(mean(cmsec, na.rm = T), 2),
            vel.max = round(max(cmsec, na.rm = T), 2),
            tot.dist = round((sum(cmsec, na.rm = T))/100, 2)) %>%
  select(ID, NO.dist.mean, NO.dist.min, vel.mean, vel.max, tot.dist) %>%
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

bout.df <- data.frame(ID = factor(), bout.n.s = numeric(), bout.n.m = numeric(), bout.m.s = numeric(), bout.m.m = numeric(), move.1 = numeric())

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
  
  # calculate time of first movement
  
  if(length(unique(mvec$move)) > 1){
    if(mvec[1,'move'] ==  'M'){
      m.1 <- 1
    } else{
      m.1 <- which(diff(mvec$bout) != 1)[[1]]
    }
  } else {
    m.1 <- 0 
  }  
  
  bout.df <- add_row(bout.df, ID = unique(tdat$ID)[i], bout.n.s = n.s, bout.n.m = n.m, bout.m.s = m.s, bout.m.m = m.m, move.1 = m.1)
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

# Calculate summary dataset for comparison of acclimation and novel object phases----------------------------------------
# csum = comparison summary df

csum <- reduc[,c(1, 8, 9, 10, 5, 6, 7, 2, 3, 4)]
csum <- arrange(csum, ID)

csum$ability <- recode(csum$ID, !!!delouse_key) # add delousing ability column
csum$ability <- factor(csum$ability, levels = c('Excellent', 'Good', 'Moderate', 'Poor')) # code delousing ability

# summarise acclimation phase distance and velocity and add to summary dataframe
csum <- tdatac %>%
  group_by(ID) %>%
  dplyr::summarise(ac_vel.mean = round(mean(cmsec, na.rm = T), 2),
                   ac_vel.sd = round(sd(cmsec, na.rm = T), 2),
                   ac_vel.max = round(max(cmsec, na.rm = T), 2),
                   ac_tot.dist = round((sum(cmsec, na.rm = T))/100, 2)) %>%
  select(ID, ac_vel.mean, ac_vel.sd, ac_vel.max, ac_tot.dist) %>%
  left_join(csum, ., by = 'ID')

# summarise novel object phase distance and velocity and add to summary dataframe
csum <- tdat %>%
  group_by(ID) %>%
  dplyr::summarise(no_vel.mean = round(mean(cmsec, na.rm = T), 2),
                   no_vel.sd = round(sd(cmsec, na.rm = T), 2),
                   no_vel.max = round(max(cmsec, na.rm = T), 2),
                   no_tot.dist = round((sum(cmsec, na.rm = T))/100, 2)) %>%
  select(ID, no_vel.mean, no_vel.sd, no_vel.max, no_tot.dist) %>%
  left_join(csum, ., by = 'ID')

# summarise acclimation phase movement variables and add to summary dataframe
csum <- tdatac %>%
  group_by(ID) %>%
  dplyr::count(move, name = 't.move.ac') %>%
  filter(move == 'M') %>%
  select (ID, t.move.ac) %>%
  left_join(csum, ., by = 'ID') %>%
  mutate(t.move.ac = tidyr::replace_na(t.move.ac, 0))

csum <- tdatac %>%
  group_by(ID) %>%
  dplyr::count(move, name = 't.static.ac') %>%
  filter(move == 'S') %>%
  select (ID, t.static.ac) %>%
  left_join(csum, ., by = 'ID') %>%
  mutate(t.static.ac = tidyr::replace_na(t.static.ac, 0))

# summarise novel object phase movement variables and add to summary dataframe
csum <- tdat %>%
  group_by(ID) %>%
  dplyr::count(move, name = 't.move.no') %>%
  filter(move == 'M') %>%
  select (ID, t.move.no) %>%
  left_join(csum, ., by = 'ID') %>%
  mutate(t.move.no = tidyr::replace_na(t.move.no, 0))

csum <- tdat %>%
  group_by(ID) %>%
  dplyr::count(move, name = 't.static.no') %>%
  filter(move == 'S') %>%
  select (ID, t.static.no) %>%
  left_join(csum, ., by = 'ID') %>%
  mutate(t.static.no = tidyr::replace_na(t.static.no, 0))

# Calculate bouts for acclimation phaase
bout.df <- data.frame(ID = factor(), bout.n.s.ac = numeric(), bout.n.m.ac = numeric(), bout.m.s.ac = numeric(), bout.m.m.ac = numeric())

for(i in 1:length(unique(tdatac$ID))){
  
  mvec <- tdatac$move[tdatac$ID == unique(tdatac$ID)[[i]]]
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
  
  bout.df <- add_row(bout.df, ID = unique(tdatac$ID)[i], bout.n.s.ac = n.s, bout.n.m.ac = n.m, bout.m.s.ac = m.s, bout.m.m.ac = m.m)
  bout.df$bout.m.s.ac[is.nan(bout.df$bout.m.s.ac)] <- 0
  bout.df$bout.m.m.ac[is.nan(bout.df$bout.m.m.ac)] <- 0
  
}

csum <- csum %>% left_join(bout.df, by = 'ID')
rm(bout.df)


# Calculate bouts for novel object phaase
bout.df <- data.frame(ID = factor(), bout.n.s.no = numeric(), bout.n.m.no = numeric(), bout.m.s.no = numeric(), bout.m.m.no = numeric())

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
  
  bout.df <- add_row(bout.df, ID = unique(tdat$ID)[i], bout.n.s.no = n.s, bout.n.m.no = n.m, bout.m.s.no = m.s, bout.m.m.no = m.m)
  bout.df$bout.m.s.no[is.nan(bout.df$bout.m.s.no)] <- 0
  bout.df$bout.m.m.no[is.nan(bout.df$bout.m.m.no)] <- 0
  
}

csum <- csum %>% left_join(bout.df, by = 'ID')
rm(bout.df)
 
# reorder columns in csum
csum <- csum %>% select(ID:ac_vel.mean, no_vel.mean, ac_vel.sd, no_vel.sd, ac_vel.max, no_vel.max, ac_tot.dist, no_tot.dist, t.move.ac, t.move.no, t.static.ac, t.static.no, bout.n.s.ac, 
                        bout.n.s.no, bout.n.m.ac, bout.n.m.no, bout.m.s.ac, bout.m.s.no, bout.m.m.ac, bout.m.m.no)


# Correlogram analysis-----------------------------------
library(corrgram)

cor.df <- bsum %>% 
  select(m.144, f.144, t.144, NO.dist.min, NO.dist.mean, vel.mean, vel.max, tot.dist, t.move, t.static, bout.n.s, bout.n.m, bout.m.s, bout.m.m, move.1, eye.l, eye.r, bout.n.l, bout.n.r, bout.m.l, bout.m.r)

labs <- c('M lice', 'F lice', 'Tot lice', 'Min dist to NO', 'Mean dist to NO', 'Mean velocity', 'Max velocity', 'Total dist', 'Time moving', 
          'Time static', 'No. static bouts', 'No. moving bouts', 'Mean length static bouts', 'Mean length moving bouts', '1st movement', 
          'left eye NO', 'right eye NO', 'No. left eye bouts', 'No. right eye bouts', 'Mean length left eye bouts', 'Mean length right eye bouts')

corrgram(cor.df, order = F, lower.panel = panel.shade, upper.panel = NULL, abs = F,
         outer.labels = list(bottom = list(labels = labs, cex = 1.5, srt = 90), left = list(labels = labs, cex = 1.5, srt = 0))
        )
corrgram(bsum[,c(1:16)], order = T, lower.panel = panel.ellipse, upper.panel = panel.pts)
corrgram(bsum[,c(1:10, 17:26)], order = T, lower.panel = panel.ellipse, upper.panel = panel.pts)

library(corrplot)

cor.df <-
  #select(m.144, f.144, t.144, NO.dist.min, NO.dist.mean, vel.mean, vel.max, tot.dist, t.move, t.static, bout.n.s, bout.n.m, bout.m.s, bout.m.m, move.1, eye.l, eye.r, bout.n.l, bout.n.r, bout.m.l, bout.m.r) %>%
  cor(y = select(bsum, m.48, f.48, t.48, m.96, f.96, t.96, m.144, f.144, t.144), 
      x = select(bsum, NO.dist.min, NO.dist.mean, vel.mean, vel.max, tot.dist, t.move, t.static, bout.n.s, bout.n.m, bout.m.s, bout.m.m, move.1, eye.l, eye.r, bout.n.l, bout.n.r, bout.m.l, bout.m.r),
      use = 'everything',
      method = 'pearson')
colnames(cor.df) <- c('48h M', '48h F', '48h total', '96h M', '96h F', '96h total', '144h M', '144h F', '144h total')
rownames(cor.df) <- c('NO min. dist.', 'NO mean dist.', 'Mean velocity', 'Max velocity', 'Total dist.', 'Time moving',
                      'time static', 'No. static bouts','No. moving bouts', 'Mean length static bouts', 'Mean length moving bouts',
                      '1st movement', 'NO left eye', 'NO right eye', 'No. left eye bouts', 'No right eye bouts', 'Mean length left eye bouts',
                      'Mean length right eye bouts')

corrplot(cor.df, 
         method = 'color',
         addCoef.col = 'black',
         tl.col = 'black'
        )


# principal component analysis
library(factoextra)

pca.df <- bsum %>% filter(ID != 'C3') # remove C3 as didn't move in behaviour test wrasse data
pca.df <- bsum # lumpfish data

pca.df <- pca.df %>% remove_rownames %>% column_to_rownames(var = 'ID') %>% select(11:28) # select behaviour data for pca
pca.df <- pca.df%>% remove_rownames %>% column_to_rownames(var = 'ID') %>% select(NO.dist.min, vel.mean, t.move, bout.n.m, bout.m.m, eye.l, bout.n.l, bout.m.l, move.1)
pca.df <- pca.df %>% remove_rownames %>% column_to_rownames(var = 'ID') %>% select(NO.dist.min, NO.dist.mean, vel.max, bout.m.s, bout.m.r)


delouse.groups <- pull(round(bsum[,10], -1))[-2] # colour by all lice delousing
delouse.groups <- cut(bsum$t.144, breaks = c(-1, 30, 100), labels = c('<30%', '>30%'))   [-2] # colour by all lice delousing wrasse data
delouse.groups <- cut(bsum$f.144, breaks = c(-1, 30, 100), labels = c('<30%', '>30%')) # colour by female lice delousing lumpfish data

res.pca <- prcomp(pca.df, scale = T) # PCA calculation
fviz_eig(res.pca) # eigen value plot
fviz_pca_ind(res.pca, axes = c(1, 3), geom = c('point', 'text'), palette = c('red', 'blue'),  
             habillage = delouse.groups,  pointshape = 19, repel = T, 
             invisible = 'quali', pointsize = 3.5, title = 'PC1 vs. PC3') +
            guides(colour = guide_legend(title = 'Delousing ability \n (% lice left after 6d)')) # individual PCA plot
fviz_pca_biplot(res.pca, axes = c(1, 2), geom = c('point', 'text'), palette = 'RdBu',  habillage = delouse.groups,  pointshape = 19, repel = T) # individual PCA plot
fviz_pca_ind(res.pca, axes = c(2, 3), geom = c('point', 'text'), palette = 'RdBu',  habillage = delouse.groups,  pointshape = 19, repel = T) # individual PCA plot
fviz_pca_var(res.pca, axes = c(1,3), col.var = 'contrib', 
             gradient.cols = c('#00AFBB', '#E7B800', '#FC4E07'), repel = T,
             title = 'PC1 vs. PC3') # variables plot
fviz_pca_biplot(res.pca, repel = T, col.var = '#2E9FDF', col.ind = '#696969') # bipot of individuals and variables

eig.val <- get_eigenvalue(res.pca) # extract eigenvalues from pca object
res.var <- get_pca_var(res.pca) # extract variable coordinates from pca object
res.ind <- get_pca_ind(res.pca) # extract individual coordinates from pca object

# PCA sandpit------------------------

# https://www.r-bloggers.com/using-r-two-plots-of-principal-component-analysis/

pca.df <- bsum %>% filter(ID != 'C3')
pca.df <- pca.df %>% remove_rownames %>% column_to_rownames(var = 'ID') %>% select(11:28) 
res.pca <- prcomp(pca.df, scale = T) 

pca.melt <- melt(res.pca$rotation)

# loading plot of principal components
ggplot(data = pca.melt) +
  geom_bar(aes(x = Var1, y = value, fill = Var1), stat = 'identity') +
  facet_wrap(~Var2) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))

# https://www.data-imaginist.com/2019/a-flurry-of-facets/

library(ggforce)

pca.df <- bsum %>% filter(ID != 'C3')
pca.df <- pca.df %>% remove_rownames %>% column_to_rownames(var = 'ID') %>% select(11:28) 
pca.df <- pca.df %>% remove_rownames %>% column_to_rownames(var = 'ID') %>% select(11, 12, 14, 20, 28) # select best correlations lumpfish data
res.pca <- prcomp(pca.df, scale = T) 
pc.df <- as.data.frame(res.pca$x)

delouse.groups <- pull(round(bsum[,10], -1))[-2] # colour by all lice delousing wrasse data
delouse.groups <- pull(round(bsum[,10], -1)) # colour by all lice delousing lumpfish data

# pca facet plot wrasse data
ggplot(pc.df, aes(x = .panel_x, y = .panel_y, colour = rep(delouse.groups, 81))) +
  geom_point(alpha = 1, shape = 16, size = 2) +
  facet_matrix(vars(everything())) +
  #scale_color_discrete(h = c('red', 'blue')) +
  theme_linedraw() +
  guides(colour = guide_legend(title = 'Delousing ability \n (% lice left after 6d)'))
  
# pca facet plot lumpfish data
ggplot(pc.df, aes(x = .panel_x, y = .panel_y, colour = rep(delouse.groups, 25))) +
  geom_point(alpha = 1, shape = 16, size = 2) +
  facet_matrix(vars(everything())) +
  #scale_color_discrete(h = c('red', 'blue')) +
  theme_linedraw() +
  guides(colour = guide_legend(title = 'Delousing ability \n (% lice left after 6d)'))

# regression facet plots
reg.df <- bsum %>% filter(ID != 'C3') # remove static wrasse 3
reg.df <- bsum # lumpfish data
reg.facet <- ggplot(reg.df, aes(x = .panel_x, y = .panel_y, colour = rep(delouse.groups, 121))) +
  geom_point(alpha = 1, shape = 16, size = 2) +
  facet_matrix(vars(t.144, NO.dist.mean, vel.mean, vel.max, t.move, bout.n.m, bout.m.m, move.1, eye.l, bout.n.l, bout.m.l)) +
  geom_smooth(se = F)



library(devtools)
source_gist("524eade46135f6348140") # new function to plot r2 and regression equation on plot

facet.labs <- c('NO min. dist.', 'NO mean dist.', 'Mean velocity', 'Max velocity', 'Total dist.', 'Time moving',
                      'time static', 'No. static bouts','No. moving bouts', 'Mean length static bouts', 'Mean length moving bouts',
                      '1st movement', 'NO left eye', 'NO right eye', 'No. left eye bouts', 'No right eye bouts', 'Mean length left eye bouts',
                      'Mean length right eye bouts')
names(facet.labs) <- c('NO.dist.mean', 'NO.dist.min', 'vel.mean', 'vel.max', 'tot.dist', 't.move', 't.static', 'bout.n.s', 'bout.n.m', 
                       'bout.m.s', 'bout.m.m', 'move.1', 'eye.l', 'eye.r', 'bout.n.l', 'bout.n.r', 'bout.m.l', 'bout.m.r')

bsum %>%
  #filter(ID != 'C3') %>% # only for wrasse data
  melt(id.vars = c('ID', 't.144'), measure.vars = c('NO.dist.mean', 'NO.dist.min', 'vel.mean', 'vel.max', 'tot.dist', 't.move', 't.static', 'bout.n.s', 'bout.n.m', 
                                                    'bout.m.s', 'bout.m.m', 'move.1', 'eye.l', 'eye.r', 'bout.n.l', 'bout.n.r', 'bout.m.l', 'bout.m.r')) %>%
  #ggplot(aes(x = t.144, y = value, colour = rep(delouse.groups, 18))) +
  ggplot(aes(x = t.144, y = value)) +
  geom_point() +
  geom_smooth(se = F, method = 'glm') +
  stat_smooth_func2(geom = 'text', method = 'lm', hjust = 0, parse = T) +
  scale_x_continuous(name = '144h total lice') +
  facet_wrap(~variable, scales = 'free', labeller = labeller(variable = facet.labs)) +
  theme_classic()

# lumpfish all variables
bsum %>%
  melt(id.vars = c('ID', 'f.144'), measure.vars = c('NO.dist.mean', 'NO.dist.min', 'vel.mean', 'vel.max', 'tot.dist', 't.move', 't.static', 'bout.n.s', 'bout.n.m', 
                                                    'bout.m.s', 'bout.m.m', 'move.1', 'eye.l', 'eye.r', 'bout.n.l', 'bout.n.r', 'bout.m.l', 'bout.m.r')) %>%
  #ggplot(aes(x = t.144, y = value, colour = rep(delouse.groups, 18))) +
  ggplot(aes(x = f.144, y = value)) +
  geom_point() +
  geom_smooth(se = F, method = 'glm') +
  stat_smooth_func2(geom = 'text', method = 'lm', hjust = 0, parse = T) +
  scale_x_continuous(name = '144h female lice') +
  facet_wrap(~variable, scales = 'free', labeller = labeller(variable = facet.labs)) +
  theme_classic()


# Plots=============================================================================

# Plot fish tracks with tank and novel object outlines
ggplot(data = tdat, aes(x = fish.rx, y = fish.ry, colour = frame)) +
  geom_path(size = 0.4) +
  scale_y_reverse(name = 'cm') +
  scale_x_continuous(name = 'cm') +
  scale_color_viridis(option = "D") +
  facet_wrap(~ID) +
  annotate("path", x = 50+50*cos(seq(0,2*pi,length.out=100)), y = 50+50*sin(seq(0,2*pi,length.out=100)), colour = 'blue') + # draw tank circle
  annotate('rect', xmin = 68, xmax = 73.5, ymin = 47.5, ymax = 53) + # draw novel object
  theme_classic()


# plot velocity for each fish
ggplot(data = tdat, aes(x = frame, y = cmsec, colour = ability)) +
  geom_line() +
  facet_wrap(~ID) +
  scale_x_continuous(name = 'secs') +
  scale_y_continuous(name = 'Velocity (cm/s)') +
  guides(colour = guide_legend(title = 'Delousing ability')) +
  theme_classic()

# plot distance from object for each fish
ggplot(data = tdat, aes(x = frame, y = distno.r, colour = ability)) +
  geom_line() +
  facet_wrap(~ID) +
  scale_x_continuous(name = 'secs') +
  scale_y_continuous(name = 'Distance from novel object (cm)') +
  guides(colour = guide_legend(title = 'Delousing ability')) +
  theme_classic()

# plot heading for each fish
ggplot(data = tdat, aes(x = frame, y = head, colour = ability)) +
  geom_line() +
  facet_wrap(~ID)

# histogram of distance from object for each fish
ggplot(data = tdat, aes(x = distno.r, fill = ability)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~ID) +
  scale_x_continuous(name = 'Distance from novel object (cm)') +
  scale_y_continuous(name = 'Count') +
  guides(fill = guide_legend(title = 'Delousing ability')) +
  theme_classic()

# barplot of total distance travelled for each fish
ggplot(data = bsum, aes(x = fct_reorder(ID, t.144, .desc = T))) +
  geom_bar(aes(y = tot.dist, fill = ability), stat = 'identity') +
  scale_x_discrete(name = 'Tank ID') +
  scale_y_continuous(name = 'Distance travelled (m)', expand = c(0, 0)) +
  guides(fill = guide_legend(title = 'Delousing ability')) +
  theme_classic()

# summary of mean velocity and distance from novel object for each fish
tdat %>%
  group_by(ID, ability, t.144) %>%
  dplyr::summarise(mean_vel = mean(cmsec, na.rm = T), 
                   sd_vel = sd(cmsec, na.rm = T), 
                   mean_dist = mean(distno.r), 
                   min_dist = min(distno.r)) %>%
  ggplot(aes(x = fct_reorder(ID, t.144, .desc = T))) +
  geom_bar(aes(y = mean_vel, fill = ability), stat = 'identity') +
  geom_errorbar(aes(ymin = mean_vel-sd_vel, ymax = mean_vel+sd_vel, width = 0.2)) +
  scale_x_discrete(name = 'Tank ID') +
  scale_y_continuous(name = 'Velocity (cm/s)', expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 35)) +
  guides(fill = guide_legend(title = 'Delousing ability')) +
  theme_classic()

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
  facet_wrap(~ID) +
  scale_x_continuous(name = 'Distance from novel object (cm)') +
  scale_y_continuous(name = 'Velocity (cm/s)', expand = c(0, 0)) +
  guides(colour = guide_legend(title = 'Delousing ability')) +
  theme_classic()

# barplot of viewing moving and static time
tdat %>%
  filter(!is.na(cmsec)) %>%
  #ggplot(aes(x = fct_reorder(ID, ab_code))) +
  ggplot(aes(x = fct_reorder(ID, t.144, .desc = T))) +
  geom_bar(aes(fill = move)) +
  #geom_point(aes(y = t.144)) +
  scale_y_continuous(name = 'secs', limits = c(0, 300), 
                     breaks = c(0, 60, 120, 180, 240, 300), 
                     expand = c(0, 0)) +
  scale_x_discrete(name = 'fish ID') +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#AA5257", "#7D87B9"), labels = c('Moving', 'Static'))


# barplot of viewing novel object with left or right eye
tdat %>%
  filter(!is.na(cmsec)) %>%
  ggplot(aes(x = fct_reorder(ID, t.144, .desc = T))) +
  geom_bar(aes(fill = eye)) +
  scale_y_continuous(name = 'secs', limits = c(0, 300), 
                     breaks = c(0, 60, 120, 180, 240, 300), 
                     expand = c(0, 0)) +
  scale_x_discrete(name = 'fish ID') +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#AA5257", "#7D87B9"), labels = c('Left eye', 'Right eye'))


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

# barplot of number of static and moving bouts
bsum %>%
  melt(id.vars = c('ID', 'ability', 't.144'), measure.vars = c('bout.n.s', 'bout.n.m')) %>%
  ggplot() +
  geom_bar(aes(x = fct_reorder(ID, t.144, .desc = T), y = value, fill = variable), stat = 'identity', position = position_dodge()) +
  theme_classic() +
  scale_x_discrete(name = 'Tank ID') +
  scale_y_continuous(name = 'No. of bouts', expand = c(0, 0)) +
  scale_fill_discrete(name = '', labels = c('Static bouts', 'Moving bouts'))

# barplot of mean duration of static and moving bouts
bsum %>%
  melt(id.vars = c('ID', 'ability', 't.144'), measure.vars = c('bout.m.s', 'bout.m.m')) %>%
  ggplot() +
  geom_bar(aes(x = fct_reorder(ID, t.144, .desc = T), y = value, fill = variable), stat = 'identity', position = position_dodge()) +
  theme_classic() +
  scale_x_discrete(name = 'Tank ID') +
  scale_y_continuous(name = 'Bout duration (sec)', expand = c(0, 0)) +
  scale_fill_discrete(name = '', labels = c('Static bouts', 'Moving bouts'))

# barplot of number of left eye and right eye bouts
bsum %>%
  melt(id.vars = c('ID', 'ability', 't.144'), measure.vars = c('bout.n.l', 'bout.n.r')) %>%
  ggplot() +
  geom_bar(aes(x = fct_reorder(ID, t.144, .desc = T), y = value, fill = variable), stat = 'identity', position = position_dodge()) +
  theme_classic() +
  scale_x_discrete(name = 'Tank ID') +
  scale_y_continuous(name = 'No. of bouts', expand = c(0, 0)) +
  scale_fill_discrete(name = '', labels = c('Left eye bouts', 'Right eye bouts'))

# barplot of mean duration of left eye and right eye bouts
bsum %>%
  melt(id.vars = c('ID', 'ability', 't.144'), measure.vars = c('bout.m.l', 'bout.m.r')) %>%
  ggplot() +
  geom_bar(aes(x = fct_reorder(ID, t.144, .desc = T), y = value, fill = variable), stat = 'identity', position = position_dodge()) +
  theme_classic() +
  scale_x_discrete(name = 'Tank ID') +
  scale_y_continuous(name = 'Bout duration', expand = c(0, 0)) +
  scale_fill_discrete(name = '', labels = c('Left eye bouts', 'Right eye bouts'))

# barplot of time of first movement after novel object added
bsum %>%
  ggplot() +
  geom_bar(aes(x = fct_reorder(ID, t.144, .desc = T), y = move.1, fill = ability), stat = 'identity') +
  theme_classic() +
  scale_x_discrete(name = 'Tank ID') +
  scale_y_continuous(name = 'Time of first movement (sec)', expand = c(0, 0)) +
  scale_fill_discrete(name = 'Delousing ability')

# Comparison plots of acclimation and novel object phases----------------------

# Plot fish tracks with tank and novel object outlines
tdat %>% bind_rows(tdatac) %>%
ggplot(aes(x = fish.rx, y = fish.ry, colour = phase)) +
  geom_path(size = 0.4) +
  scale_y_reverse(name = 'cm') +
  scale_x_continuous(name = 'cm') +
  scale_color_discrete(labels = c('Acclimation', 'Test')) +
  facet_wrap(~ID) +
  annotate("path", x = 50+50*cos(seq(0,2*pi,length.out=100)), y = 50+50*sin(seq(0,2*pi,length.out=100)), colour = 'blue') + # draw tank circle
  annotate('rect', xmin = 68, xmax = 73.5, ymin = 47.5, ymax = 53) + # draw novel object
  theme_classic()

# Change in mean velocity
csum %>%
  select(ID, ac_vel.mean, ac_vel.sd, no_vel.mean, no_vel.sd) %>%
  pivot_longer(cols = c(ac_vel.mean:no_vel.sd), names_to = c('phase', '.value'), names_sep = '\\_') %>%
  #data.table::melt(id.vars = 'ID', measure.vars = c('vel.mean.ac', 'vel.mean.no'), variable.name = 'phase', value.name = 'mean.velocity') %>%
  ggplot(aes(x = ID, y = vel.mean, fill = phase, ymin = vel.mean-vel.sd, ymax = vel.mean+vel.sd)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = 'Tank ID') +
  scale_y_continuous(name = 'Velocity (m/s)', limits = c(-5, 35), expand = c(0, 0)) +
  scale_fill_discrete(name = 'Phase', labels = c('Acclimation', 'Test')) +
  coord_cartesian(y = c(0, 35)) +
  theme_classic()

# Change in max velocity
csum %>%
  select(ID, ac_vel.max, no_vel.max) %>%
  pivot_longer(cols = c(ac_vel.max:no_vel.max), names_to = 'phase', values_to = 'vel.max') %>%
  ggplot(aes(x = ID, y = vel.max, fill = phase)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  scale_x_discrete(name = 'Tank ID') +
  scale_y_continuous(name = 'Velocity (m/s)', limits = c(0, 40), expand = c(0, 0)) +
  scale_fill_discrete(name = 'Phase', labels = c('Acclimation', 'Test')) +
  theme_classic()

# Change in total distance
csum %>%
  select(ID, ac_tot.dist, no_tot.dist) %>%
  pivot_longer(cols = c(ac_tot.dist:no_tot.dist), names_to = 'phase', values_to = 'tot.dist') %>%
  ggplot(aes(x = ID, y = tot.dist, fill = phase)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  scale_x_discrete(name = 'Tank ID') +
  scale_y_continuous(name = 'Distance travelled (cm)', limits = c(0, 100), expand = c(0, 0)) +
  scale_fill_discrete(name = 'Phase', labels = c('Acclimation', 'Test')) +
  theme_classic()


# stats--------------------------------------------------

# minimum distance to novel object
tdat %>%
  group_by(ID) %>%
  dplyr::summarise(min_dist = min(distno.r))




