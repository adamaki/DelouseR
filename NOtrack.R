# Novel Object test - fish tracking
# Adam Brooker
# 24th February 2020


library(EBImage)
#library(imager) 
library(dplyr)


# functions (run once before using script)----------------------------------------------------------------------

# read integer from command line input
readinteger <- function(message = 'Not a number')
{ 
  num <- readline(prompt = message)
  if(!grepl("^[0-9]+$", num))
  {
    return(readinteger())
  }
  
  #return(as.integer(num))
  num <<- as.integer(num)
}



# 1. Set working directory and input variables----------------------------

workingdir <- 'G:/Data/Cleaner fish delousing/Novel Object videos/Individual wrasse/C12-S' # change to location of data
setwd(workingdir)

inputfile <- 'C12-S'
files <- list.files(path = workingdir, pattern = inputfile, all.files = FALSE, recursive = FALSE)
start <- 303 # start frame number
end <- 602 # end frame number
rotangle <- -1 # image rotation angle to translate image to cartesian grid
xrange <- c(159,497) # x-axis crop dimensions
yrange <- c(12, 349) # y-axis crop dimensions

centre <- c(171, 169) # coords for centre of tank in cropped and rotated image
outrad <- 169 # radius of tank mask in pixels
cal1 <- c(3, 166) # location of 1st calibration marker in any image
cal2 <- c(338, 166) # location of 2nd calibration marker in any image
caldist <- 100 # real distance between calibration markers in cm
nodims <- c(237, 168, 9) # dimensions of circle for novel object (x, y, r)
rmask <- c(128, 164, 34, 71) # coordinates for masking light reflection (x1:x2, y1:y2)

# image testing to refine rotating and cropping
test.img <- readImage(files[[350]])
display(rotate(test.img, rotangle)) # display rotated image to find crop coordinates
test.img <- (rotate(test.img, rotangle)[xrange[[1]]:xrange[[2]], yrange[[1]]:yrange[[2]],])
display(test.img)
test.img <- drawCircle(test.img, centre[[1]], centre[[2]], radius = outrad, col = 'yellow', fill = F)
display(test.img)
test.img <- drawCircle(test.img, nodims[[1]], nodims[[2]], radius = nodims[[3]], col = 'light blue', fill = T)
display(test.img)

rm(test.img)

# calculated variables
cfactor <- caldist/round(sqrt(abs(cal1[[1]]-cal2[[1]])^2+abs(cal1[[2]]-cal2[[2]])^2)) # calculate real distance conversion factor

# 2. load image set, modify images for analysis and create mean of image stack---------------------------------

system.time({
  
  files <- list.files(path = workingdir, pattern = inputfile, all.files = FALSE, recursive = FALSE)
  
  mod_stack <- readImage(files[start])
  mod_stack <- (rotate(mod_stack, rotangle, bg.col = 0.5)[xrange[[1]]:xrange[[2]], yrange[[1]]:yrange[[2]],])

  for(i in (start+1):end){
    image <- readImage(files[i]) # load image file
    mod_stack <- EBImage::combine(mod_stack, rotate(image, rotangle, bg.col = 0.5)[xrange[[1]]:xrange[[2]], yrange[[1]]:yrange[[2]],])
  }
 
  green_stack <- channel(mod_stack, 'green')
  #green_stack <- 1-green_stack
  green_stack <- green_stack*(1/max(green_stack)) # increase contrast to max
  green_stack <- gblur(green_stack, sigma = 1) # Gaussian smoothing filter
  green_stack_mean <- as.Image(rowMeans(green_stack, dims = 2)) # create mean image of stack
  
  Sys.sleep(2)
  
  
  # 4. Subtract mean image from stack, threshold image stack, mask outside tank and remove noise-----------------------------------------
  
  thresh_stack <- green_stack_mean # seed thresholded image stack
  
  # create mask of tank area
  tmask <- floodFill(green_stack[,,1], c(1, 1), col = 0, tolerance = 255)
  tmask <- drawCircle(tmask, centre[[1]], centre[[2]], radius = outrad, fill = T, col = 1)
  tmask <- bwlabel(tmask)
  
  # create novel object mask
  nomask <- floodFill(green_stack[,,1], c(1, 1), col = 0, tolerance = 255)
  nomask <- drawCircle(nomask, nodims[1], nodims[2], nodims[3], col = 1, fill = T)
  nomask <- bwlabel(nomask)
  
  
  for(j in 1:dim(green_stack)[[3]]){
    subimg <- green_stack_mean - green_stack[,,j]
    subimg[rmask[1]:rmask[2], rmask[3]:rmask[4]] <- 0 # mask out light reflection
    subimg <- subimg-(1-tmask) # mask outside tank
    subimg <- subimg-(nomask) # mask out novel object
    
    subimg <- subimg*(1/max(subimg))
    subimg <- subimg > 0.60
    subimg <- bwlabel(subimg)
    sf <- computeFeatures.shape(subimg, green_stack[,,j]) # calculate shape features
    mf <- computeFeatures.moment(subimg, green_stack[,,j]) # calculate moment features 
    if(is.matrix(get('mf'))) {
      mf <- as.data.frame(mf)
      mf$tmask <- ifelse(tmask[matrix(data = c(round(mf[,'m.cx']), round(mf[,'m.cy'])), nrow(mf))] == 1, 1, 0) # test whether object centre coords are in mask
      subimg <- rmObjects(subimg, which(mf[,'tmask'] == 0)) # remove objects outside mask
    }
    subimg <- rmObjects(subimg, which(sf[,'s.area'] < round((dim(thresh_stack)[[1]] * dim(thresh_stack)[[2]]) * 0.00005) | sf[,'s.area'] > round((dim(thresh_stack)[[1]] * dim(thresh_stack)[[2]]) * 0.005))) # remove objects less than 0.005% of image area and greater than 1% of image area 
    thresh_stack <- EBImage::combine(thresh_stack, subimg)
  }
  
  thresh_stack <- thresh_stack[,,-c(1)] # remove first image, which is stack_mean
  
  # fill holes in fish and dilate to join gaps
  thresh_stack <- fillHull(thresh_stack)
  thresh_stack <- dilate(thresh_stack, makeBrush(3, shape = 'disc'))
  thresh_stack <- bwlabel(thresh_stack)
  
  display(thresh_stack, method = 'raster', all = T)  
  
  Sys.sleep(2)
  
  # 6. Create list of fish coordinates and mark errors-----------------------------------------
  
  # create coords list file
  coords <- data.frame(frame = numeric(), fishpx = numeric(), fishpy = numeric(), errors = character())
  
  # find centre of each fish object & write coords to file for each frame
  for(m in 1:dim(thresh_stack)[[3]]){
    mf <- computeFeatures.moment(thresh_stack[,,m], green_stack[,,m]) # calculate moment features  
    coords <- add_row(coords, frame = m+(start-1), fishpx = ifelse(is.matrix(get('mf')), round(mf[1,1]), NA), 
                      fishpy = ifelse(is.matrix(get('mf')), round(mf[1,2]), NA), 
                      errors = ifelse(is.matrix(get('mf')) == F, 'No fish', ifelse(nrow(mf) > 1, 'Noise', 'None')))
  }
  
  coords$fishrx <- round(coords$fishpx * cfactor, 2)
  coords$fishry <- round(coords$fishpy * cfactor, 2)
  coords <- coords[,c(1, 2, 3, 5, 6, 4)]
  
  display(thresh_stack, method = 'raster', all = T)   
  
  Sys.sleep(2)
  
  # 7. save segmented images as series---------------------------------------------------
  dir.create('Tracked')
  setwd(paste0(workingdir, '/Tracked'))
  
  for(n in 1:dim(thresh_stack)[[3]]){
    overlay <- paintObjects(thresh_stack[,,n], mod_stack[,,,n], col = c('#ff00ff', '#ff00ff'), opac = c(1, 0), thick = T) # paint fish outline in purple
    overlay <- paintObjects(nomask, overlay, col = c('light blue', 'light blue'), opac = c(1, 0), thick = T) # paint model outline in light blue
    overlay <- drawCircle(overlay, centre[[1]], centre[[2]], radius = outrad, col = 'yellow', fill = F)
    writeImage(overlay, gsub('.jpg', '_tracked.png', files[n+(start-1)]))
  }
  
}) # end of system time measurement



# 8. Run through errors and fix, saving new images (view images in external viewer as R viewer doesn't update in loops)----------------------------------
for(p in 1:dim(thresh_stack)[[3]]){
  
  if(coords[p,6] == 'Noise'){
    thresh_stack <- bwlabel(thresh_stack)
    mf <- computeFeatures.moment(thresh_stack[,,p], green_stack[,,p]) # calculate moment features 
    obj <- data.frame(object = rownames(mf), fish_x = round(mf[,'m.cx']), fish_y = round(mf[,'m.cy']))
    print(obj)
    print(paste0('Frame: ', coords[p, 1]))
    readinteger('Enter oject number to keep: ') # input object number to keep. Enter '0' if all objects are not fish.
    if(num == 0){
      thresh_stack[,,p] <- rmObjects(thresh_stack[,,p], seq(1, nrow(mf), 1)) # remove all objects if fish not detected
    } else{
      thresh_stack[,,p] <- rmObjects(thresh_stack[,,p], which(seq(1, nrow(mf), 1) != num)) # remove objects not fish
    }
    overlay <- paintObjects(thresh_stack[,,p], mod_stack[,,,p], col = c('#ff00ff', '#ff00ff'), opac = c(1, 0), thick = T)
    overlay <- paintObjects(nomask, overlay, col = c('light blue', 'light blue'), opac = c(1, 0), thick = T) # paint model outline in light blue
    overlay <- drawCircle(overlay, centre[[1]], centre[[2]], radius = outrad, col = 'yellow', fill = F)
    writeImage(overlay, gsub('.jpg', '_tracked.png', files[p+(start-1)]))
  } else {
    if(coords[p, 6] == 'No fish'){
      print(paste0('Frame: ', coords[p, 1]))
      readinteger('Enter x coordinate of fish: ')
      fish <- num
      readinteger('Enter y coordinate of fish: ')
      fish <- c(fish, num)
      thresh_stack[fish[[1]], fish[[2]], p] <- 1
      thresh_stack[,,p] <- dilate(thresh_stack[,,p], makeBrush(15, shape = 'disc'))
      overlay <- paintObjects(thresh_stack[,,p], mod_stack[,,,p], col = c('#ff00ff', '#ff00ff'), opac = c(1, 0), thick = T)
      overlay <- paintObjects(nomask, overlay, col = c('light blue', 'light blue'), opac = c(1, 0), thick = T) # paint model outline in light blue
      overlay <- drawCircle(overlay, centre[[1]], centre[[2]], radius = outrad, col = 'yellow', fill = F)
      writeImage(overlay, gsub('.jpg', '_tracked.png', files[p+(start-1)]))
    }
  } 
}


# 9. Manual removal of noise and remarking of fish for any remaining errors---------------------------------------------------
mrem <- function(fnum){
  
  frame <- fnum-(start-1) # change to frame number to re-mark
  thresh_stack <- bwlabel(thresh_stack)
  while(frame > 0){
    mf <- computeFeatures.moment(thresh_stack[,,frame], green_stack[,,frame]) # calculate moment features  
    if(is.matrix(get('mf'))) {thresh_stack[,,frame] <- rmObjects(thresh_stack[,,frame], seq(1, nrow(mf), 1))} # remove all objects
    
    display(mod_stack[,,,frame])
    readinteger('Enter x coordinate of fish: ')
    fish <- num
    readinteger('Enter y coordinate of fish: ')
    fish <- c(fish, num)
    
    thresh_stack[fish[[1]], fish[[2]], frame] <- 1
    thresh_stack[,,frame] <- dilate(thresh_stack[,,frame], makeBrush(15, shape = 'disc'))
    coords[frame,2] <- fish[[1]]
    coords[frame,3] <- fish[[2]]
    coords[frame,6] <- 'None'
    
    setwd(paste0(workingdir, '/Tracked'))
    
    overlay <- paintObjects(thresh_stack[,,frame], mod_stack[,,,frame], col = c('#ff00ff', '#ff00ff'), opac = c(1, 0), thick = T)
    overlay <- paintObjects(nomask, overlay, col = c('light blue', 'light blue'), opac = c(1, 0), thick = T) # paint model outline in light blue
    overlay <- drawCircle(overlay, centre[[1]], centre[[2]], radius = outrad, col = 'yellow', fill = F)
    writeImage(overlay, gsub('.jpg', '_tracked.png', files[fnum]))
    
    display(overlay)
    coords <<- coords
    thresh_stack <<- thresh_stack
    frame <- 0
  }
  
}

# 10. Batch code multiple frames with fish in same position when it doesn't move

multif <- function(fstart, fend){
  
  fstart <- fstart-(start-1) # change to start frame number
  fend <- fend-(start-1) # change to end frame number
  thresh_stack <- bwlabel(thresh_stack)
  setwd(paste0(workingdir, '/Tracked'))
  readinteger('Enter x coordinate of fish: ')
  fish <- num
  readinteger('Enter y coordinate of fish: ')
  fish <- c(fish, num)
  
  for(f in fstart:fend){
    mf <- computeFeatures.moment(thresh_stack[,,f], green_stack[,,f]) # calculate moment features  
    if(is.matrix(get('mf'))) {thresh_stack[,,f] <- rmObjects(thresh_stack[,,f], seq(1, nrow(mf), 1))} # remove all objects
    thresh_stack[fish[[1]], fish[[2]], f] <- 1
    thresh_stack[,,f] <- dilate(thresh_stack[,,f], makeBrush(15, shape = 'disc'))
    thresh_stack <- bwlabel(thresh_stack)
    coords[f,2] <- fish[[1]]
    coords[f,3] <- fish[[2]]
    coords[f,6] <- 'None'
    overlay <- paintObjects(thresh_stack[,,f], mod_stack[,,,f], col = c('#ff00ff', '#ff00ff'), opac = c(1, 0), thick = T)
    overlay <- paintObjects(nomask, overlay, col = c('light blue', 'light blue'), opac = c(1, 0), thick = T) # paint model outline in light blue
    overlay <- drawCircle(overlay, centre[[1]], centre[[2]], radius = outrad, col = 'yellow', fill = F)
    writeImage(overlay, gsub('.jpg', '_tracked.png', files[f+(start-1)]))
  }
  
  coords <<- coords
  thresh_stack <<- thresh_stack
}


# 11. Recalculate coordinates after all errors fixed and models tracked-------------------------------
system.time({
  thresh_stack <- bwlabel(thresh_stack)

  # create coords list file
  coords <- data.frame(frame = numeric(), fish.px = numeric(), fish.py = numeric(), distno.p = numeric(), errors = character())
  nopx <- as.data.frame(which(nomask == 1, arr.ind = T))
  
  
  # find centre of each fish object & write coords to file for each frame
  for(m in 1:dim(thresh_stack)[[3]]){
    
    # get centre of fish coords
    mf <- computeFeatures.moment(thresh_stack[,,m], green_stack[,,m])
    fishpx <- c(round(mf[,'m.cx']), round(mf[,'m.cy']))
    
    # get min fish distance to left model
    nopx$dist <- round(sqrt(abs(fishpx[[1]]-nopx[,1])^2+abs(fishpx[[2]]-nopx[,2])^2))
    
    coords <- add_row(coords, frame = m+(start-1), fish.px = ifelse(is.matrix(get('mf')), round(mf[1,1]), NA), 
                      fish.py = ifelse(is.matrix(get('mf')), round(mf[1,2]), NA),
                      distno.p = min(nopx$dist),
                      #distmod.pr = min(modpx.r$dist),
                      errors = ifelse(is.matrix(get('mf')) == F, 'No fish', ifelse(nrow(mf) > 1, 'Noise', 'None')))
    
  }
  
  
  # convert pixel coordinates to mm coordinates
  coords$fish.rx <- round(coords$fish.px * cfactor, 2)
  coords$fish.ry <- round(coords$fish.py * cfactor, 2)
  coords$distno.r <- round(coords$distno.p * cfactor, 2)
  coords <- coords[,c(1, 2, 3, 4, 6, 7, 8, 5)]
  
  
  write.csv(coords, paste0(inputfile, '_', start, '-', end, '.csv'), row.names = F)
  
}) # end of system time measurement


# Load segmented files as image stack (If need to reload old dataset)-----------------------------------
files <- list.files(path = paste0(workingdir, '/Tracked'), pattern = '_tracked.png', all.files = FALSE, recursive = FALSE)
setwd(paste0(workingdir, '/Tracked'))
thresh_stack <- readImage(files, all = T)
thresh_stack <- thresh_stack[,,1,] == 1 & thresh_stack[,,2,] == 0 & thresh_stack[,,3,] == 1 # threshold pixels with fuschia colour
colorMode(thresh_stack) <- Grayscale
thresh_stack <- fillHull(thresh_stack)
thresh_stack <- bwlabel(thresh_stack)






