# library(akima)

safe.save <- function(obj, file=NULL, log=FALSE) {
  if (log) { cat(paste("Saving: ", file, "\n")) }
  temp.fname = system("mktemp /tmp/temp.RData.XXXX", intern = T)
  save(obj, file = temp.fname)
  system(paste("mv ", temp.fname, file, sep = " "))
  return();
}
 
safe.load <- function(file, log=FALSE) {
  if (log) { cat(paste("Loading: ", file, "\n")) }
  obj <- get(load(file))
  return(obj)
}

binarize.matrix <- function(mat, thresh) {
  cols <- ncol(mat)
  mat <- matrix(as.numeric(mat >= thresh), ncol = cols)
}

## OTSU'S THRESHOLD
 
## Description:
##
## This returns a threshold such that it maximizes between class variance
## the pixel intensities.
##
## Usage:
##
## otsu(img)
##
## Arguments:
##
## img: a vector of values representing the pixel intentisities
##
## Value:
##
## a list with components:
##
## threshold: the threshold intensity value that maximizes between
## class variance. Target class is equal to or greater than the threshold
##
## breaks: a vector of thresholds
##
## bcv: the corresponding between class variance for breaks.
 
otsu = function (samples, breaks = "Sturges"){
  h <- hist(samples, right=FALSE, breaks=breaks, plot=F)
 
  ## This contains the different levels of the threshold.
  breaks <- h$breaks
 
  ## Overall mean intensity of the image
  u <- mean(samples);
 
  ## Cluster mean for background pixels at threshold breaks[i]
  uB <- vector()
  uB[1] <- 0
 
  ## Vector containing the number of pixels in the t-th bucket:
  nTotal <- c(0, h$counts)
 
 
  ## Vector containing the count of the pixels
  ## less than the threshold.
  nB <- c(0, cumsum(h$counts))
 
  ## Vector containing the count of pixels
  ## equal to or above the threshold.
  nT <- length(samples) - nB
 
  ## Cluster mean for target pixels
  uT <- vector()
  uT[1] <- sum(breaks * nTotal) / nT[1]
 
  ## Between class variance
  bcv <- vector()
  ##bcv[1] <- nB[1] * nT[1] * (uB[1]-uT[1])^2
  bcv[1] = NaN
  
  for (t in 2:length(nB)) {
    ## The mean value of the background pixels at when thresh
    ## is breaks[t] is a weighted sum of the uB at the previous thresh
    ## and the current thresh.
    uB[t] <- (uB[t-1]*nB[t-1] + breaks[t]*nTotal[t]) / nB[t]
    uT[t] <- (uT[t-1]*nT[t-1] - breaks[t]*nTotal[t]) / nT[t]
    bcv[t] <- nB[t] * nT[t] * (uB[t]-uT[t])^2
  }
  uB[1] = NaN
  ##if (is.na(bcv[length(bcv)])) { bcv[length(bcv)] <- 0 }
 
  ## calculate the threshold that corresponds to the max bcv.
  threshold <- breaks[order(bcv,decreasing=TRUE)[1]]
  result <- list(threshold=threshold, breaks=breaks, bcv=bcv,
                 uB=uB, uT=uT, nB=nB, nT=nT, nTotal=nTotal)
  return(result)
}

rect.rasterlines <- function(levels = 3, xmin, ymin, xmax, ymax, rotate = F) {
  if (rotate) {
    t <- xmin
    xmin <- ymin
    ymin <- t
    t <- xmax
    xmax <- ymax
    ymax <- t
  }
  x.pattern <- c(xmin, xmin, xmax, xmax)
  y.step <- seq(ymin,ymax,length.out = levels*2 + 1)
  y.out <- NULL
  for (i in 1:(length(y.step)-1)) {
  ## i, i+1,
    y.out <- c(y.out, y.step[i], y.step[i+1])
  }
  xy <- data.frame(x = x.pattern, y = y.out)
  if (rotate) {
    xy <- data.frame(x = xy$y, y = xy$x)
  }
  return(xy)
}

looplines <- function(xmin, ymin, xmax, ymax)
{
  return (data.frame(x = c(xmin, xmax, xmax, xmin, xmin),
                     y = c(ymin, ymin, ymax, ymax, ymin)))
}

adp.list.last <- function (listobj)
{
  stopifnot (is.list (listobj),
             length (listobj) > 0)
  tail (listobj, 1) [[ 1 ]]
}

adp.list.first <- function (listobj)
{
  stopifnot (is.list (listobj),
             length (listobj) > 0)
  head (listobj, 1) [[ 1 ]]
}

get.ls.paths <- function (ls.path.list)
{
  ls.path <- matrix (vector(), ncol = 2)
  for (i in 1:length (ls.path.list))
    {
      ls.path <- rbind (ls.path, ls.path.list[[i]] [,  c("x", "y")])
    }
  return (ls.path)
}

get.ls.path.lengths <- function (ls.path)
{
  lengths <- cumsum(sqrt(diff (ls.path[,"x"])^2 + diff (ls.path[,"y"])^2))
  return (lengths)
}

get.image.hat.mse <- function (image, image.hat )
{
  return (mean ((image - image.hat)^2, na.rm = T))
}

get.image.hat <- function (int.points, image.mat, width=NULL, height=NULL)
{
  if (is.null (width)) { width <- ncol (image.mat) }
  if (is.null (height)) { height <- nrow (image.mat) }
  vect.coordinates <- (int.points[,1]-1) * ncol (image.mat) + int.points[,2]
  data.samples <- image.mat [vect.coordinates]
  akima.li <- interp(int.points[,1],
                     int.points[,2],
                     data.samples,
                     duplicate="mean",
                     xo = seq (min = 1, max = ncol (image.mat), length = width),
                     yo = seq (min = 1, max = nrow (image.mat), length = height))
  return (matrix (akima.li$z, ncol = width, byrow = T))
}



get.int.points.from.real.points <- function (ls.path, xdim, ydim)
{
  int.points.agg <- matrix(vector(), ncol=2)
  for (i in (1:(nrow (ls.path)-1)))
    {
      x1 <- ls.path[i,'x']
      y1 <- ls.path[i,'y']
      x2 <- ls.path[i+1,'x']
      y2 <- ls.path[i+1,'y']
      point.start <- pmin (pmax (ceiling (c (x1,
                                             y1)
                                          * c (xdim,
                                               ydim)),
                                 1),
                           c (xdim,
                              ydim))
      point.end <- pmin (pmax (ceiling (c (x2,
                                           y2)
                                        * c (xdim,
                                             ydim)),
                               1),
                         c (xdim,
                            ydim))

      int.points <- intermediate.points (point.start[1], point.start[2], point.end[1], point.end[2])
      int.points.agg <- rbind (int.points.agg, int.points)
    }
  return (int.points.agg)
}

get.image.hat.from.ls.path <- function (ls.path, true.image, width, height)
{
  int.points <- get.int.points.from.real.points (ls.path, nrow (true.image), ncol (true.image))
  image.hat <- get.image.hat (int.points = int.points,
                              image.mat = true.image)
  downsample.index <- round (seq (1, nrow (image.hat), length.out = width))
  return.image <- image.hat [downsample.index, downsample.index]
                             
  return (return.image)
}
