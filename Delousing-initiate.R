# Cleaner fish tank delousing
# Adam Brooker 29th August 2019

library(dplyr)
library(ggplot2)
library(plyr)
library(reshape2)
library(stringr)
library(colorspace)
#library(grImport)
#library(grImport2)
library(devtools)
library(cowplot)
#library(Cairo)
library(magick)
library(colorRamps)

# FUNCTIONS----------------------------------------------------------------------------------------------------

# function for mapping lice numbers to salmon image
map.lice <- function(locations, maxlice, mvalues = T, sdvalues = T, leg = 'No. lice'){
  
  locations$bins <- as.numeric(cut(locations$lice.m, breaks = seq(-0.1, maxlice, 0.1), labels = seq(1, (maxlice+0.1)*10, 1)))
  licepal <- rev(heat.colors((maxlice+0.1)*10, alpha = 0.5))
  
  salcol <- image_read('G:/Projects/Lumpfish delousing/Data/SalmonOutline.bmp')
  salcol <- if(locations$bins[[1]] > 0) {image_fill(salcol, licepal[[locations$bins[[1]]]], point = '+156+227', fuzz = 40)} else {salcol} #head
  salcol <- if(locations$bins[[2]] > 0) {image_fill(salcol, licepal[[locations$bins[[2]]]], point = '+299+124', fuzz = 40)} else {salcol} # front dorsal
  salcol <- if(locations$bins[[3]] > 0) {image_fill(salcol, licepal[[locations$bins[[3]]]], point = '+630+116', fuzz = 40)} else {salcol} # mid dorsal
  salcol <- if(locations$bins[[3]] > 0) {image_fill(salcol, licepal[[locations$bins[[3]]]], point = '+538+68', fuzz = 40)} else {salcol} # mid dorsal (fin)
  salcol <- if(locations$bins[[4]] > 0) {image_fill(salcol, licepal[[locations$bins[[4]]]], point = '+934+154', fuzz = 40)} else {salcol} # rear dorsal
  salcol <- if(locations$bins[[4]] > 0) {image_fill(salcol, licepal[[locations$bins[[4]]]], point = '+913+124', fuzz = 40)} else {salcol} # rear dorsal (fin)
  salcol <- if(locations$bins[[5]] > 0) {image_fill(salcol, licepal[[locations$bins[[5]]]], point = '+314+207', fuzz = 40)} else {salcol} # front flank
  salcol <- if(locations$bins[[6]] > 0) {image_fill(salcol, licepal[[locations$bins[[6]]]], point = '+570+217', fuzz = 40)} else {salcol} # mid flank
  salcol <- if(locations$bins[[7]] > 0) {image_fill(salcol, licepal[[locations$bins[[7]]]], point = '+941+194', fuzz = 40)} else {salcol} # rear flank
  salcol <- if(locations$bins[[8]] > 0) {image_fill(salcol, licepal[[locations$bins[[8]]]], point = '+343+314', fuzz = 40)} else {salcol} # front ventral
  salcol <- if(locations$bins[[9]] > 0) {image_fill(salcol, licepal[[locations$bins[[9]]]], point = '+680+308', fuzz = 40)} else {salcol} # mid ventral
  salcol <- if(locations$bins[[10]] > 0) {image_fill(salcol, licepal[[locations$bins[[10]]]], point = '+904+250', fuzz = 40)} else {salcol} # rear ventral
  salcol <- if(locations$bins[[10]] > 0) {image_fill(salcol, licepal[[locations$bins[[10]]]], point = '+887+293', fuzz = 40)} else {salcol} # rear ventral (fin)
  salcol <- if(locations$bins[[11]] > 0) {image_fill(salcol, licepal[[locations$bins[[11]]]], point = '+1147+205', fuzz = 40)} else {salcol} # tail
  
  if(mvalues == T){
    if(sdvalues == T){
      
      salcol <- image_annotate(salcol, 
                               paste0(format(round(locations$lice.m[[1]], 1),nsmall = 1), '\u00B1', format(round(locations$lice.sd[[1]], 1),nsmall = 1)), 
                               size = 20, color = 'black', degrees = 0, location = '+156+215') # head
      salcol <- image_annotate(salcol, 
                               paste0(format(round(locations$lice.m[[2]], 1),nsmall = 1), '\u00B1', format(round(locations$lice.sd[[2]], 1),nsmall = 1)),
                               size = 20, color = 'black', degrees = -13, location = '+278+121') # dorsal front
      salcol <- image_annotate(salcol, 
                               paste0(format(round(locations$lice.m[[3]], 1),nsmall = 1), '\u00B1', format(round(locations$lice.sd[[3]], 1),nsmall = 1)), 
                               size = 20, color = 'black', degrees = 6, location = '+626+105') # dorsal middle
      salcol <- image_annotate(salcol, 
                               paste0(format(round(locations$lice.m[[4]], 1),nsmall = 1), '\u00B1', format(round(locations$lice.sd[[4]], 1),nsmall = 1)),
                               size = 20, color = 'black', degrees = 7.5, location = '+906+139') # dorsal rear
      salcol <- image_annotate(salcol, 
                               paste0(format(round(locations$lice.m[[5]], 1),nsmall = 1), '\u00B1', format(round(locations$lice.sd[[5]], 1),nsmall = 1)),
                               size = 20, color = 'black', degrees = 0, location = '+278+189') # flank front
      salcol <- image_annotate(salcol, 
                               paste0(format(round(locations$lice.m[[6]], 1),nsmall = 1), '\u00B1', format(round(locations$lice.sd[[6]], 1),nsmall = 1)),
                               size = 20, color = 'black', degrees = 0, location = '+626+189') # flank middle
      salcol <- image_annotate(salcol, 
                               paste0(format(round(locations$lice.m[[7]], 1),nsmall = 1), '\u00B1', format(round(locations$lice.sd[[7]], 1),nsmall = 1)),
                               size = 20, color = 'black', degrees = 0, location = '+906+189') # flank rear
      salcol <- image_annotate(salcol, 
                               paste0(format(round(locations$lice.m[[8]], 1),nsmall = 1), '\u00B1', format(round(locations$lice.sd[[8]], 1),nsmall = 1)),
                               size = 20, color = 'black', degrees = 0, location = '+278+279') # ventral front
      salcol <- image_annotate(salcol, 
                               paste0(format(round(locations$lice.m[[9]], 1),nsmall = 1), '\u00B1', format(round(locations$lice.sd[[9]], 1),nsmall = 1)),
                               size = 20, color = 'black', degrees = -8, location = '+626+306') # ventral middle
      salcol <- image_annotate(salcol, 
                               paste0(format(round(locations$lice.m[[10]], 1),nsmall = 1), '\u00B1', format(round(locations$lice.sd[[10]], 1),nsmall = 1)),
                               size = 20, color = 'black', degrees = -9, location = '+906+236') # ventral rear
      salcol <- image_annotate(salcol, 
                               paste0(format(round(locations$lice.m[[11]], 1),nsmall = 1), '\u00B1', format(round(locations$lice.sd[[11]], 1),nsmall = 1)),
                               size = 20, color = 'black', degrees = 0, location = '+1124+189') # tail
    
      } else {
        
      salcol <- image_annotate(salcol, format(round(locations$lice.m[[1]], 1),nsmall = 1), size = 20, color = 'black', degrees = 0, location = '+156+215')
      salcol <- image_annotate(salcol, format(round(locations$lice.m[[2]], 1), nsmall = 1), size = 20, color = 'black', degrees = -14, location = '+295+118')
      salcol <- image_annotate(salcol, format(round(locations$lice.m[[3]], 1), nsmall = 1), size = 20, color = 'black', degrees = 6, location = '+626+105')
      salcol <- image_annotate(salcol, format(round(locations$lice.m[[4]], 1), nsmall = 1), size = 20, color = 'black', degrees = 7.5, location = '+906+139')
      salcol <- image_annotate(salcol, format(round(locations$lice.m[[5]], 1), nsmall = 1), size = 20, color = 'black', degrees = 0, location = '+295+189')
      salcol <- image_annotate(salcol, format(round(locations$lice.m[[6]], 1), nsmall = 1), size = 20, color = 'black', degrees = 0, location = '+626+189')
      salcol <- image_annotate(salcol, format(round(locations$lice.m[[7]], 1), nsmall = 1), size = 20, color = 'black', degrees = 0, location = '+906+189')
      salcol <- image_annotate(salcol, format(round(locations$lice.m[[8]], 1), nsmall = 1), size = 20, color = 'black', degrees = 0, location = '+295+279')
      salcol <- image_annotate(salcol, format(round(locations$lice.m[[9]], 1), nsmall = 1), size = 20, color = 'black', degrees = -2, location = '+626+306')
      salcol <- image_annotate(salcol, format(round(locations$lice.m[[10]], 1), nsmall = 1), size = 20, color = 'black', degrees = -9, location = '+906+236')
      salcol <- image_annotate(salcol, format(round(locations$lice.m[[11]], 1), nsmall = 1), size = 20, color = 'black', degrees = 0, location = '+1130+189')
      
      }
    }
  
  salplot <- ggdraw() +
    draw_image(salcol)
  
  liceplot <- ggplot(data = locations, aes(x = location, y = lice.m, fill = lice.m)) +
    geom_bar(stat = 'identity') +
    labs(fill = leg) +
    guides(fill = guide_colourbar(title.position = 'bottom', title.hjust = 0.5, frame.colour = 'black')) +
    theme(legend.position = 'bottom', legend.justification = 'centre', legend.key.width = unit(2, 'cm')) +
    scale_fill_gradientn(colors = licepal, limits = c(0, maxlice), breaks = seq(0, floor(maxlice), 1))
  
  liceleg <- get_legend(liceplot)
  
  salplot <<- salplot
  salfig <<- plot_grid(salplot, liceleg, nrow = 2, ncol = 1, rel_heights = c(0.8, 0.2))
  liceleg <<- liceleg
}