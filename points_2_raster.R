## function
create_raster_from_datXY = function(datXY, fieldName, sampleR){
  library(sp)
  library(raster)
  
  # assign numeric values for NA
  datXY[is.na(datXY)] = -9999
  
  # first two column and lon(x) and lat (y) coordinates
  colnames(datXY)[1:2]  = c('X','Y')
  coordinates(datXY) =  ~ X +Y
  
  r = rasterize(datXY,sampleR, field= fieldName)
  r[r == -9999] = NA
  
  return(r)
}


## create a reference raster
sampleR = raster(res = 0.1, 
                 xmn = 45, xmx = 49, 
                 ymn = 45, ymx = 49, 
                 vals = 0.3)
## create a data frame with coordinates information
# the fist two column must be X and Y coordinates
X = seq(46, 48.5, length.out = 200)
Y = seq(44, 53 , length.out = 200)
datXY = NULL
for(ix in 1: length(X)){
  for(iy in 1:length(Y)){
    t = c(X[ix], Y[iy],val = runif(1, min = 0, max = 100))
    datXY = rbind.data.frame(datXY, t)
  }
}
colnames(datXY) = c('X','Y','val')
head(datXY)

## create raster from data points
datXY.ras = create_raster_from_datXY(datXY,  fieldName = 'val', sampleR = sampleR)


## check by plot, new raster will be constraint by sampleR regardless points location
plot(0,0, col = 'white', xlim = range(X), ylim = range(Y), xlab = '', ylab = '')
image(datXY.ras, add = T)
points(datXY[,1], datXY[,2], pch = 16, cex = 0.1)

