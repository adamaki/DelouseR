# Novel object test behaviour analysis
# Copyright Adam Brooker 
# 29th February 2020



library(tidyverse)



workingdir <- 'G:/Data/Cleaner fish delousing/Novel Object videos/Individual wrasse' # change to location of data
setwd(workingdir)

files <- list.files(path = workingdir, pattern = '.csv', all.files = FALSE, recursive = FALSE)

tdat <- data.frame(frame = numeric(0), fish.px = numeric(0), fish.py = numeric(0), distno.p = numeric(0), fish.rx = numeric(0), fish.ry = numeric(0), distno.r = numeric(0), ID = character(0))

for(i in 1:length(files)){
  
 df <- read.csv(files[i]) 
 df <- select(df, -errors)
 df$frame <- seq(1, nrow(df), 1)
 #df$ID <- str_sub(files[i], 1, 2)
 df$ID <- sub('\\-.*', '', files[i])
 
 tdat <- rbind(tdat, df)
 
}

tdat$ID <- factor(tdat$ID, levels = c('C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C12'))


ggplot(data = tdat, aes(x = fish.rx, y = fish.ry)) +
  geom_path() +
  scale_y_reverse(name = 'cm') +
  scale_x_continuous(name = 'cm') +
  facet_wrap(~ID) +
  annotate("path", x = 50+50*cos(seq(0,2*pi,length.out=100)), y = 50+50*sin(seq(0,2*pi,length.out=100)), colour = 'blue') +
  annotate('rect', xmin = 68, xmax = 73.5, ymin = 47.5, ymax = 53)





