# Novel object test behaviour analysis
# Copyright Adam Brooker 
# 29th February 2020



library(tidyverse)



workingdir <- 'G:/Data/Cleaner fish delousing/Novel Object videos/Individual wrasse' # change to location of data
setwd(workingdir)

files <- list.files(path = workingdir, pattern = '.csv', all.files = FALSE, recursive = FALSE)


# Read behaviour files into one dataframe
tdat <- data.frame(frame = numeric(0), fish.px = numeric(0), fish.py = numeric(0), distno.p = numeric(0), fish.rx = numeric(0), fish.ry = numeric(0), distno.r = numeric(0), ID = character(0))

for(i in 1:length(files)){
  
 df <- read.csv(files[i]) 
 df <- select(df, -errors)
 df$frame <- seq(1, nrow(df), 1)
 #df$ID <- str_sub(files[i], 1, 2)
 df$ID <- sub('\\-.*', '', files[i])
 
 tdat <- rbind(tdat, df)
 
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
  group_by(ID) %>% 
  fill(eye, dir, .direction = 'down') %>%
  fill(eye, dir, .direction = 'up') # fill down direction and eye when fish not moving

tdat$eye <- ifelse(tdat$ID == 'C3', 'L', tdat$eye) # manually code left eye for fish C3 (static during NO test)

tdat$dir <- ifelse(is.na(tdat$cmsec), NA, tdat$dir) # change 1st row of each fish back to NA
tdat$eye <- ifelse(is.na(tdat$cmsec), NA, tdat$eye) # change 1st row of each fish back to NA

tdat$move_eye <- ifelse(is.na(tdat$eye), NA, paste0(tdat$move, '_', tdat$eye)) # new column for moving and static left and right eye


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

tdat %>%
  filter(!is.na(cmsec)) %>%
  ggplot(aes(x = fct_reorder(ID, ab_code))) +
  geom_bar(aes(fill = move_eye)) +
  scale_y_continuous(name = 'secs', limits = c(0, 300), breaks = c(0, 60, 120, 180, 240, 300), expand = c(0, 0)) +
  scale_x_discrete(name = 'fish ID') +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#AA5257", "#8A1923", "#023FA5", "#7D87B9"), labels = c('Left eye, moving', 'Right eye, moving', 'Left eye, static', 'Right eye, static'))








