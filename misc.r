## library (grid) ## For the graphics stuff.
## library (fields) ## For rdist.
## library (ggplot2)
## library (doMC)
## if (exists ('draw') && !draw)  { registerDoMC () }
## source ("utils.r")
#' @include utils.r
roxygen()

global.lwd <- 5
global.skip.interacting <- F ## Should be T if using anchored model.
global.diff.agree.scale <- 200 ## Maybe should be 10 if using anchored model.

############################################################
### Data Structures
##
## new.bombing.df: data.frame(x, y, radius)
############################################################


## bombing.r bombing model
## Description:
## The germs are distributed as a Poisson Point Process with intensity parameter lambda.
## The grains are disks with radii drawn from a Weibull distribution with parameters
## shape and scale.
##
## Usage:
## bombing(lambda, shape, scale = 1, xlim = (0,1), ylim = (0,1))
##
## Arguments:
## lambda: Intensity of poisson process for germs.
## shape: Shape parameter for the Weibull distribution (from which the disk radius is drawn).
## scale: Scale parameter for the Weibull distribution (from which disk radius is drawn).
## xlim: The limits (x1, x2) of the observation window.
## ylim: The limits (y1, y2) of the observation window.


##
## Details:
## The bombing model is a special case of a random closed set that is the union of
## disks placed a locations created by a Poisson Point process.
##
## Value:
## bombing(.) generates a sample observation in the specified window in R^2.
## The result is a N x 3 data frame, where each row is the x, y, and radius of a point.
## Note that it's possible that N is 0.
##
## Note:
## There are obvious edge effects which we will ignore for now. But to deal with it, you could
## simulate a much larger window (and possibly specify a maximum disk radius), or you could
## some how explicitly model the number and size of disks that intrude into the observation
## window.
##
## Examples:

## bomb.df = data.frame(x = vector(), y = vector(), radius = vector())
##
new.bombing.empty <- function (x = vector(),
                               y = vector(),
                               radius = vector(),
                               anchor.x = vector(),
                               anchor.y = vector())
{
  stopifnot (length (x) == length (y),
             (length (x) == length (radius))
             || ((length (radius) == 1)
                 && (length (x) > 0)),
             length (anchor.x) == length (anchor.y))
  if (length (anchor.x) > 0)
    {
      stopifnot (length (anchor.x) == length (x))
    }

  bomb.df <- 
    if (length (x) == 0)
      {
        data.frame (x = vector(),
                    y = vector(),
                    radius = vector(),
                    anchor.x = vector(),
                    anchor.y = vector())
      }
    else if ((length (x) > 0)
             && (length (anchor.x) == 0))
      {
        bomb.df <- data.frame (x = x,
                               y = y,
                               radius = radius,
                               anchor.x = NA,
                               anchor.y = NA)
      }
    else if ((length (x) > 0)
             && (length (anchor.x) > 0))
      {
        data.frame(x = x,
                   y = y,
                   radius = radius,
                   anchor.x = anchor.x,## TODO what to do about default values here.
                   anchor.y = anchor.y)
      }
  else
    {
      stop ("Got into an unexpected state.")
    }
}

## OUPUT: date.frame (x, y, radius)
new.bombing <- function(lambda = 1, shape = 1, scale = 1, xlim = c(0,1), ylim = c(0,1), num.points = NULL)
{
  points.df <- poisson.point.process (lambda, xlim, ylim, num.points)
  bombing.df <- mark.points (points.df, shape, scale)
  return (new.bombing.empty (x = bombing.df$x,
                             y = bombing.df$y,
                             radius = bombing.df$radius))
}

poisson.point.process <- function(lambda, xlim = c(0,1), ylim = c(0,1), num.points = NULL)
{
  area <- abs (xlim[1] - xlim[2]) * abs(ylim[1] - ylim[2])
  ## The number of points is a poisson distribution with rate equal to lambda * area.
  num.points <-
    if (is.null (num.points)) { rpois (n = 1, lambda = lambda*area) }
    else { num.points }
  points.df <- data.frame (x = runif(num.points, xlim[1], xlim[2]),
                           y = runif(num.points, ylim[1], ylim[2]))
  return (points.df)
}

mark.points <- function(points.df, shape, scale)
{
  radii <- rweibull (nrow(points.df), shape, scale)
  stopifnot (length(radii) == nrow (points.df))
  marked.points.df <- cbind (points.df, radius = radii)
  return (marked.points.df)
}

grob.bombing <- function(bombing.df)
{
  gp = gpar(col = "white", fill="white", alpha=1)
  grob.circles <- grid.circle (x = bombing.df$x,
                               y = bombing.df$y,
                               r = bombing.df$radius,
                               gp = gp,
                               draw = F)
  return(grob.circles)
}

display.bombing <- function (bombing.df)
{
  ## Create a visual
  grid.newpage ()
  vp <- viewport (width=1, height=1 )
  pushViewport (vp)
  ## Draw the frame
  grid.rect (gp=gpar(fill="black"))
  ## Draw the bombing
  if (nrow (bombing.df) > 0)
    {
      g <- grob.bombing (bombing.df)
      grid.draw (g)
    }
  ## Frame
  ##grid.rect (gp=gpar(col="blue"))
  popViewport()
  return()
}

get.matrix.from.bombing <- function (bombing.df, width, height)
{
  library (ReadImages)
  ## cat('.')
  fname <- paste (tempfile ("get_matrix_from_bombing.", tmpdir="/tmp"), ".jpeg", sep="")
  jpeg (file = fname, width = width, height = height)
  display.bombing (bombing.df)
  dev.off()
  img <- read.jpeg (fname)
  unlink (fname)
  mat <- t(round(img[,,1]))[,nrow(img):1]
  return (mat)
}

## A matrix with the mean values. or NULL if list is empty.
get.mean.matrix.from.chain.list <- function (chain.list, width, height, start.at)
{
  stopifnot (is.list (chain.list),
             !is.null (width),
             !is.null (height),
             start.at <= length (chain.list),
             start.at >= 1)
  if (length (chain.list) == 0)
    {
      return (NULL)
    }
  if (length (chain.list) == 1)
    {
      return (get.matrix.from.bombing (chain.list[[1]]$bombing.df,
                                       width = width,
                                       height = height))
    }
  else
    {
      sum.mat <- matrix(data = 0, nrow = height, ncol = width)
      len <- length (chain.list)
      cat ('\npercent done:  0%')

      for (i in start.at:len)
        {
          percent <- round (i/len * 100)
          if (percent < 10) { cat (paste('\b\b', percent, '%', sep='')) }
          else { cat (paste('\b\b\b', percent, '%', sep='')) }
          sum.mat <-
            {
              matrix.from.bombing <- get.matrix.from.bombing (chain.list[[i]]$bombing.df,
                                                              width = width,
                                                              height = height)
              matrix.weight <- 1 + chain.list[[i]]$end.index - chain.list[[i]]$start.index
              sum.mat + (matrix.weight * matrix.from.bombing)
            }
        }

      mean.mat <-
        {
          start.index <- chain.list[[start.at]]$start.index
          end.index <- chain.list[[len]]$end.index
          stopifnot (end.index >= start.index)
          total.count <- end.index - start.index + 1
          sum.mat / total.count
        }
      return (mean.mat)
    }
}

## ls.list = list( list( p1=c(x1,y1), p2=c(x2,y2),
## union.intersections.df = data.frame (left, right) ))
get.grob.segments <- function (ls.list, color="blue")
{
  
  get.grob.segments.helper <- function (ls)
    {
      grob.segments <- list()
      ## Draw the main line:
      gp = gpar(col = color, lwd=global.lwd)
      grob.segments[[1]] <- grid.segments (x0 = ls$p1[1], y0 = ls$p1[2],
                                           x1 = ls$p2[1], y1 = ls$p2[2],
                                           draw = F, gp = gp)
      len <- nrow (ls$union.intersections.df)
      if (len > 0)
        {
          for (i in (1:len))
            {
              gp = gpar(col="red", lwd=global.lwd)
              result.left <- find.abs.coords (ls, ls$union.intersections.df[i, "left"])
              result.right <- find.abs.coords (ls, ls$union.intersections.df[i, "right"])
              grob.segments[[i+1]] <- grid.segments (x0 = result.left[1], y0 = result.left[2],
                                                     x1 = result.right[1], y1 = result.right[2],
                                                     draw = F, gp = gp)
            }
        }
      return (grob.segments)
    }
  
  list.grob.segments <-
    if (length(ls.list) > 0)
      {
        lapply (ls.list,
                get.grob.segments.helper)
      }
    else {
      list()
    }
  return (list.grob.segments)
}

get.grob.segments.single.grob <- function (ls.list, name, color=NULL)
{
  grob.segments.list <- get.grob.segments (ls.list, color = color)
  gtree <- gTree(name = name)
  if (length (grob.segments.list) > 0)
    {
      for (i in 1:length (grob.segments.list))
        {
          grob.segments <- grob.segments.list[[i]]
          if (length (grob.segments) > 0)
            {
              for (j in 1:length (grob.segments))
                {
                  grob.segment <- grob.segments[[j]]
                  gtree <- addGrob (gtree, grob.segment)
                }
            }
        }
    }
  if (!is.null (color))
    {
      gtree <- applyEdit (gtree, gEdit (gp=gpar (col=color, lwd=global.lwd)))
    }
  return (gtree)
}

display.grob.gtree <- function (ls.list, name, color = "blue")
{
  ## Create a visual
  grid.newpage ()
  vp <- viewport (width=1, height=1 )
  pushViewport (vp)
  ## Draw the frame
  grid.rect (gp=gpar(fill="black"))
  ## Draw the segments
  grob.segments.gtree <- get.grob.segments.single.grob (ls.list, name = name, color = "blue")
  grid.draw (grob.segments.gtree)
  ## Frame
  grid.rect (gp=gpar(col="blue", lwd=global.lwd))
  
}

display.grob.segments <- function (ls.list, bombing.df=NULL)
{
  helper <- function (b.df)
    {
      if (nrow (b.df) > 0)
        {
          grob.bombing (b.df)
        }
      else {
        NULL
      }
    }
  ## Create a visual
  grid.newpage ()
  vp <- viewport (width=1, height=1 )
  pushViewport (vp)
  ## Draw the frame
  grid.rect (gp=gpar(fill="black"))
  ## Draw the bombing
  if (!is.null(bombing.df))
    {
      g <- helper (bombing.df)
      if (!is.null (g))
        {
          grid.draw (g)
        }
    }
  ## Draw the segments
  grob.segments.list <- get.grob.segments (ls.list)
  if (length (grob.segments.list) > 0)
    {
      lapply (grob.segments.list,
              function (grob.segments)
              {
                if (length (grob.segments) > 0)
                  {
                    lapply (grob.segments,
                            grid.draw)
                  }
              })
    }
  ## Frame
  grid.rect (gp=gpar(col="blue", lwd=global.lwd))
}

## This returns a point observation in the bombing.df.
observe.point <- function (bombing.df, x, y)
{
  len <- nrow (bombing.df)
  for (i in 1:len)
    {
      mp <- bombing.df[i,]
      distance <- sqrt ((x - mp$x)^2 + (y - mp$y)^2)
      if (distance <= mp$radius)
        {
          return (1)
        }
    }
  return (0)
}

#############################################
## Data Structure
##
## interacting.bombs.df
#############################################
new.interacting.bombs.mat <- function (bombing.df = NULL)
{
  dnames <- list (NULL, dimnames = c("x1", "y1", "r1", "x2", "y2", "r2", "area"))
  interacting.bombs.mat <-
    if (!is.null (bombing.df) && nrow (bombing.df) > 2)
      {
        ## this is slow, but I don't it's unusual to call
        ## new.interacting.bombs.mat with a non-null bombing.df, so it's
        ## OK.
        len <- nrow (bombing.df)
        indices.df <- expand.grid (outer = (1:len), inner = (1:len))
        indices.refined.df <- transform (subset (indices.df, (inner < outer)), area = 0)
        indices.refined.df$area <-
          mapply (function (outer, inner)
                  {
                    elem.outer.df <- bombing.df[outer,]
                    elem.inner.df <- bombing.df[inner,]
                    get.area.overlap (elem.outer.df$x,
                                      elem.outer.df$y,
                                      elem.outer.df$radius,
                                      elem.inner.df$x,
                                      elem.inner.df$y,
                                      elem.inner.df$radius)
                  },
                  indices.refined.df$outer,
                  indices.refined.df$inner)
        mask <- indices.refined.df$area > 0
        masked.indices <- indices.refined.df[mask,]
        as.matrix (cbind (bombing.df [masked.indices$inner, c('x', 'y', 'radius')],
                          bombing.df [masked.indices$outer, c('x', 'y', 'radius')],
                          masked.indices$area))
      }
    else {
      matrix (vector(), ncol = 7)
    }
  dimnames (interacting.bombs.mat) <- dnames
  return (interacting.bombs.mat)
}

add.interacting.bombs.mat <- function (interacting.bombs.mat, bombing.df, add.bomb.df)
{
  ## If the last element in bombing.df is not equal to add.bomb.df, abort.
  ## For each bombing.df, find the area over lap with add.bomb.df.
  ## For each non-zero area, add the interacting bombs to i.b.m.
  ## Return the new i.b.m.
  stopifnot (is.matrix (interacting.bombs.mat),
             is.data.frame (bombing.df),
             is.data.frame (add.bomb.df),
             nrow (add.bomb.df) == 1)
  stopifnot (all (tail (bombing.df, 1) == add.bomb.df,
                  na.rm = T))
  
  add.x <- add.bomb.df$x
  add.y <- add.bomb.df$y
  add.radius <- add.bomb.df$radius
  len <- nrow(bombing.df)
  trimmed.bombing.df <- bombing.df[-len,]
  new.interactions.mat <-
    if (nrow (trimmed.bombing.df) > 0)
      {
        area.v <- mapply (function (x, y, radius)
                          {
                            get.area.overlap (x, y, radius, add.x, add.y, add.radius)
                          },
                          trimmed.bombing.df$x,
                          trimmed.bombing.df$y,
                          trimmed.bombing.df$radius)
        mask <- area.v > 0
        if (all (!mask))
          {
            new.interacting.bombs.mat()
          }
        else {
          as.matrix (cbind (trimmed.bombing.df [mask, c('x', 'y', 'radius')],
                            add.x, add.y, add.radius,
                            area.v [mask]))
        }
      }
    else {
      new.interacting.bombs.mat()
    }
  stopifnot (is.matrix (new.interactions.mat))
  dimnames (new.interactions.mat) <- dimnames (interacting.bombs.mat)
  added.interacting.bombs.mat <- rbind (interacting.bombs.mat, new.interactions.mat)
  stopifnot (is.matrix (added.interacting.bombs.mat))
  return (added.interacting.bombs.mat)
}

remove.interacting.bombs.mat <- function (interacting.bombs.mat, remove.bomb.df)
{
  stopifnot (is.matrix (interacting.bombs.mat),
             is.data.frame (remove.bomb.df),
             nrow (remove.bomb.df) == 1)
  remove.match1 <- (interacting.bombs.mat[,'x1'] == remove.bomb.df$x
                    & interacting.bombs.mat[,'y1'] == remove.bomb.df$y
                    & interacting.bombs.mat[,'r1'] == remove.bomb.df$radius)
  remove.match2 <- (interacting.bombs.mat[,'x2'] == remove.bomb.df$x
                    & interacting.bombs.mat[,'y2'] == remove.bomb.df$y
                    & interacting.bombs.mat[,'r2'] == remove.bomb.df$radius)
  reduced.interacting.bombs.mat <- interacting.bombs.mat[!(remove.match1 | remove.match2),, drop=F]
  stopifnot (is.matrix (reduced.interacting.bombs.mat))
  return (reduced.interacting.bombs.mat)
}

############################################################
### Data Structures
##
## new.line.segment
############################################################
#' Create a line segment object.
#'
#' Create a line segment object given a set of two end points.
#'
#' @param x1 \code{x} coordinate of the first end point.
#' @param y1 \code{y} coordinate of the first end point.
#' @param x2 \code{x} coordinate of the second end point.
#' @param y2 \code{y} coordinate of the second end point.
#' @return a \code{list} object with the names:
#'   p1, p2, slope, intercept, length, bombs.df,
#'   union.intersections.df, observed.intersections.df,
#'   agreement.length.
#' @export
#' @callGraph
#' @author Andrew Parker \email{adparker@@gmail.com}
#' @examples
#' line <- new.line.segment (0, 0, 1, 1)
new.line.segment <- function (x1, y1, x2, y2)
{
  ## I have to do it this way, since the following should, but doesn't work:
  stopifnot ( is.numeric(x1), is.numeric(y1), is.numeric(x2), is.numeric(y2))
  obj <- list()
  obj$p1 <- c(x1, y1)
  obj$p2 <- c(x2, y2)
  obj$slope <- (y2-y1)/(x2-x1)
  obj$intercept <-y1 - obj$slope * x1
  obj$length <- sqrt ((x1-x2)^2 + (y1-y2)^2)
  obj$bombs.df <- new.bombs ()
  obj$union.intersections.df <- new.union.intersections ()
  obj$observed.intersections.df <- new.union.intersections ()
  obj$agreement.length <- 0
  return (obj)
}

#' Check to see if an object is a line segment.
#'
#' Check to see if an object is a line segment.
#'
#' @param line.segment is the object in question.
#' @return a boolean indicating whether or not \code{line.segment} is a line segment.
#' @export
#' @author Andrew Parker \email{adparker@@gmail.com}
#' @examples
#' line <- new.line.segment (0, 0, 1, 1)
#' is.line.segment (line)
is.line.segment <- function (line.segment)
{
  return (setequal (names (line.segment), names (new.line.segment (0,0,0,0))))
}

## Doesn't copy bombing and intersections.
new.line.segment.copy <- function (ls)
{
  stopifnot (is.line.segment (ls))
  newls <- new.line.segment (x1 = ls$p1[1],
                             y1 = ls$p1[2],
                             x2 = ls$p2[1],
                             y2 = ls$p2[2])
  return (newls)
}

##
new.line.segment.from.observation <- function (ls)
{
  newls <- new.line.segment.copy (ls)
  newls$observed.intersections.df <- ls$union.intersections.df
  newls$agreement.length <- calculate.ls.agreement.length (newls)
  return (newls)
}

#' Finds the absolute coordinates of a point some distance along a line segment.
#'
#' Given a scalar, treat this as a magnitude along the direction given by the
#' line segment, and return the absolute coordinates.
#'
#' @param ls line segment
#' @param len scalar value (can be positive or negative)
#' @return a pair representating the x/y values of the coordinates.
#' @export
#' @author Andrew Parker \email{adparker@@gmail.com}
#' @examples
#'  ls <- new.line.segment (0, 0, 1, 1)
#'  find.abs.coords (ls, 0.5)
find.abs.coords <- function (ls,len)
{
  len.rel <- len / ls$length  
  x.rel <-  len.rel * (ls$p2[1] - ls$p1[1])
  y.rel <- len.rel * (ls$p2[2] - ls$p1[2])
  x.abs <- x.rel + ls$p1[1]
  y.abs <- y.rel + ls$p1[2]
  return (c (x.abs, y.abs))
}

#############################################################
### Data Structure Helpers
##
## new.bombs
## new.union.intersections
#############################################################
new.bombs <- function (x=vector(), y=vector(), radius=vector(), left=vector(), right=vector())
{
  return (data.frame (x=x, y=y, radius=radius, left=left, right=right))
}

new.union.intersections <- function(left=vector(), right=vector())
{
  return (data.frame(left=left, right=right))
}

check.union.intersections <- function (union.intersections.df)
{
  ## Lefts are in order.
  ## Rights are in order.
  ## For each Row, left <= right.
  cat ("\n*****CHECK PASSED********\n")
  len <- nrow (union.intersections.df > 0)
  if (len > 0)
    {
      ## Ordering according to the left should equal ordering according
      ## to the right if there are no overlaps.
      stopifnot (all (order (union.intersections.df$left) == 1:len),
                 all (order (union.intersections.df$right) == 1:len),
                 all (union.intersections.df$left <= union.intersections.df$right))
      if (len > 1)
        {
          stopifnot (all (union.intersections.df$right[-len] <= union.intersections.df$left[-1]))
        }
    }
  return (TRUE)
}

##############################################################
### Client functions
## 
## add.bomb.to.line.segment
## remove.bomb.from.line.segment
## generate.stepfun.line.segment
##############################################################
## Returns a new line segment object with the new bomb and updated union intersection.
add.bomb.to.line.segment <- function (ls, bomb.x, bomb.y, radius)
{
  stopifnot(!is.null(ls), !is.na(bomb.x), !is.na(bomb.y), !is.na(radius))
  ls.new <- add.bomb.to.bombs (ls, x=bomb.x, y=bomb.y, radius=radius)
  if (nrow (ls.new$bombs.df) == (nrow (ls$bombs.df) + 1))
    {
      ## Add the last row of bombs.df to the union intersections.
      ls.new.2 <- add.bomb.to.union.intersections (ls.new, bombs.df = ls.new$bombs.df [nrow (ls.new$bombs.df), ])
      ls.new.2$agreement.length <- calculate.ls.agreement.length (ls.new.2)
      return (ls.new.2)
    }
  else {
    return (ls)
  }
}

add.bombs.to.line.segment <- function (ls, bombs.df)
{
  len <- nrow (bombs.df)
  if (len > 0)
    {
      for (i in (1:len))
        {
          bomb.x <- bombs.df[i, "x"]
          bomb.y <- bombs.df[i, "y"]
          radius <- bombs.df[i, "radius"]
          ls <- add.bomb.to.line.segment (ls, bomb.x=bomb.x, bomb.y=bomb.y, radius=radius)
        }
    }
  return (ls)
}

## Returns a new line segment.
## It removes at most one instance of a bombing with the given parameters.
remove.bomb.from.line.segment <- function (ls, bomb.x, bomb.y, radius)
{
  stopifnot (is.numeric(bomb.x), length(bomb.x)==1,
             is.numeric(bomb.y), length(bomb.y)==1,
             is.numeric(radius), length(radius)==1)
  if (nrow(ls$bombs.df) > 0)
    {
      ls.removed.from.bombs <- remove.from.bombs (ls, x=bomb.x, y=bomb.y, radius=radius)
      ls.rebuilt <- rebuild.union.intersections (ls.removed.from.bombs)
      ls.rebuilt$agreement.length <- calculate.ls.agreement.length (ls.rebuilt)
      return (ls.rebuilt)
    }
  else { return (ls) }
}

## This returns a stepfun object.
## To get a plot, just do:
## > sf = generate.stepfun.line.segment (ls)
## > plot (sf)
generate.stepfun.line.segment <- function (ls)
{
  ## Since the left and right values are the end point of non-overlapping regions, we can
  ## order the regions for stepfun by dumping it all into a vector and then using sort.
  x <- sort (c (ls$union.intersections.df$left,
                ls$union.intersections.df$right))
  y <- rep (0:1, len=length(x)+1)
  step.function <- stepfun (x, y)
  return (step.function)
}

###############################################################
### Helper functions for add.bomb.to.line.segment
###############################################################
## Input
                                        # End points of a line segment and some point in sample space
## Output
                                        # The distance between the point and the line segment
old.get.distance.from.line.segment <- function(px, py, x1, y1, x2, y2)
{
                                        # There are two situations. Either the projection of the point onto
                                        # the line falls within the end points of the segment or not.
  distance.and.closest.point <- get.distance.from.infinite.line.and.closest.point (px=px,
                                                                                   py=py,
                                                                                   x1=x1,
                                                                                   y1=y1,
                                                                                   x2=x2,
                                                                                   y2=y2)
  if (is.contained.on.segment (px=distance.and.closest.point$closest.point[1],
                               py=distance.and.closest.point$closest.point[2],
                               x1=x1, y1=y1, x2=x2, y2=y2))
    {
      return (distance.and.closest.point$distance)
    }
  else
    {
      distance.from.end.point.1 <- sqrt ((px - x1)^2 + (py - y1)^2)
      distance.from.end.point.2 <- sqrt ((px - x2)^2 + (py - y2)^2)
      return ( min (distance.from.end.point.1, distance.from.end.point.2) )
    }
}

is.contained.on.segment <- function (px, py, x1, y1, x2, y2)
{
  return ( is.between (px, x1, x2) && is.between (py, y1, y2) )
}

get.distance.from.infinite.line.and.closest.point <- function(px, py, x1, y1, x2, y2)
{
  stopifnot (is.numeric(px), length(px)==1,
             is.numeric(py), length(py)==1,
             is.numeric(x1), length(x1)==1,
             is.numeric(y1), length(y1)==1,
             is.numeric(x2), length(x2)==1,
             is.numeric(y2), length(y2)==1)
                                        # Check if this is a horizontal or vertical line
  if (y1 == y2)
    {
      return (list (distance = abs (py - y1),
                    closest.point = c(px, y1)))
    }
  else if (x1 == x2)
    {
      return (list (distance = abs(px - x1),
                    closest.point = c(x1, py)))
    }
  else
    {
      z <- get.closest.point.on.infinite.line(px, py, x1, y1, x2, y2)
      return (list(distance = sqrt((z[1]-px)^2 + (z[2] - py)^2),
                   closest.point = z))
    }
}

## Input
                                        # End points of a line segment and some point in sample space
                                        # x1,y1 != x2,y2
## Output
                                        # The distance between the point and the line defined by the segment
get.closest.point.on.infinite.line <- function(px, py, x1, y1, x2, y2)
{
  ## Check if this is a horizontal or vertical line
  if (y1 == y2)
    { ## This is a horizontal line.
      return (c (px, y1))
    }
  else if (x1 == x2)
    { ## This is a vertical line.
      return (c (x1, py))
    }
  else
    {
      delta.x <- x2 - x1
      delta.y <- y2 - y1
      line1 <- get.slope.intercept(x1,y1,x2,y2)
      a1 <- line1[1]
      a2 <- -1/a1
      offset.x <- 100
      b1 <- line1[2]
      b2 <- get.slope.intercept(px, py,
                                px + offset.x,
                                py + a2 * offset.x) [2]
      hat.x <- delta.x * delta.y * (b2 - b1) / (delta.x^2 +delta.y^2)
      hat.y <- a1 * hat.x + b1
      ##return(data.frame(x = hat.x, y = hat.y))
      return (c (hat.x, hat.y))
    }
}

## z and a and b can be vectors
is.between <- function(z, a, b)
{
  (z <= pmax(a,b)) & (z >= pmin(a,b))
}

get.slope.intercept <- function(x1, y1, x2, y2)
{
  slope <-  (y2 - y1) / (x2 - x1)
  intercept <-  y1 - slope * x1
  return (c (slope, intercept))
}

get.norm <- function(v1)
{
  return (sqrt (v1 %*% v1))
}

get.unit.vec <- function(v1)
{
  return (v1 / get.norm(v1))
}

get.mag.of.proj.vecs <- function(v1, unit.vec)
{
  return (v1 %*% unit.vec)
}

## the magnitude of p3 -pbase1 onto pbase2 - pbase1
get.mag.of.proj.points <- function(pbase1, pbase2, p3)
{
  va <- p3 - pbase1
  vb <- pbase2 - pbase1
  unit.vec <- get.unit.vec(vb)
  mag <- get.mag.of.proj.vecs(v1=va, unit.vec=unit.vec)
}

## Returns a two element vector.
## A range can also be (NA, NA)
get.range.intersection <- function(range1, range2)
{
  stopifnot(length(range1) == 2,
            length(range2) == 2,
            is.vector(range1),
            is.vector(range2),
            !any(is.nan(range1)),
            !any(is.nan(range2)))
  
  retval <- c ( max (range1[1], range2[1]),
               min (range1[2], range2[2]) )
  if (any (is.null(retval)))
    { ## Deal with NAs.
      return (c(NA, NA))
    }
  else if (retval[1] > retval[2])
    { ## There is no intersection
      return (c(NA, NA))
    }
  else
    { ## We're good to go.
      return (retval)
    }
}

## Returns list (distance from x1,y1 the start of intersection,
##               distance from x1,y1 the end of intersection)
## OR
## NULL if there's no intersection.
get.intersection.of.line.segment.and.circle <- function (x1, y1, x2, y2, xc, yc, radius)
{
  ## Check for the special case where the line.segment is actually a point.
  if ( all (x1==x2, y1==y2))
    {
      return (NULL)
    }
  else {
    distance.and.closest.point <- get.distance.from.infinite.line.and.closest.point (px=xc, py=yc,
                                                                                     x1=x1, y1=y1,
                                                                                     x2=x2, y2=y2)
    if (distance.and.closest.point$distance >= radius)
      {
        return (NULL)
      }
    infinite.line.intersect.half.length <- sqrt (radius^2 - distance.and.closest.point$distance^2)
    pbase1 <- c (x1,y1)
    pbase2 <- c (x2,y2)
    p3 <- c (distance.and.closest.point$closest.point[1], distance.and.closest.point$closest.point[2])
    relative.coord.of.projected.point <- get.mag.of.proj.points (pbase1=pbase1,
                                                                 pbase2=pbase2,
                                                                 p3=p3)
    relative.range.of.infinite.intersection <-
      range (relative.coord.of.projected.point - infinite.line.intersect.half.length,
             relative.coord.of.projected.point + infinite.line.intersect.half.length)
    mag.of.base <- get.norm (pbase2 - pbase1)
    range.of.segment.intersection <- get.range.intersection (c(0, mag.of.base),
                                                             relative.range.of.infinite.intersection)
    if (any (is.na (range.of.segment.intersection)))
      {
        return (NULL)
      }
    else {
      return (range.of.segment.intersection)
    }
  }
}

## Returns a new ls
## Assumes that ls is not-empty.
add.bomb.to.bombs <- function (ls, x, y, radius)
{
                                        # Figure out the intersection and create a row for bombs.df
  int.range <- get.intersection.of.line.segment.and.circle (ls$p1[1],
                                                            ls$p1[2],
                                                            ls$p2[1],
                                                            ls$p2[2],
                                                            x,
                                                            y,
                                                            radius)
  if (!is.null(int.range))
    {
      ls$bombs.df <- rbind (ls$bombs.df,
                            new.bombs (x=x,
                                       y=y,
                                       radius=radius,
                                       left=int.range[1],
                                       right=int.range[2]))
    }
  
  return (ls)
}

## Bomb df needs to be a single row.
## Returns a new ls.
add.bomb.to.union.intersections <- function (ls, bombs.df)
{
  stopifnot (nrow (bombs.df) == 1)
  if ( any (is.na (bombs.df$left), is.na (bombs.df$right)))
    {
      return (ls)
    }
  else
    {
      ls$union.intersections.df <- add.segment.union.intersections (ls$union.intersections.df,
                                                                    left = bombs.df$left,
                                                                    right = bombs.df$right)
      return (ls)
    }
}

## Assumes left and right are not NA.
## union.intersections.df may be empty.
## Returns a new union.intersections.df.
add.segment.union.intersections <- function (union.intersections.df, left, right, do.check=T)
{
  stopifnot (is.numeric (left), !is.na (left), length (left) == 1,
             is.numeric (right), !is.na (right), length (right) == 1,
             left <= right)
  
  if (any (is.na (left), is.na (right)))
    {
      return (union.intersections.df)
    }
  else
    {
      
      left.mask <- is.between (union.intersections.df$left,
                               left,
                               right)
      right.mask <- is.between (union.intersections.df$right,
                                left,
                                right)
      old.contains.new.mask <- is.between (left,
                                           union.intersections.df$left,
                                           union.intersections.df$right)
      intersecting.mask <- left.mask | right.mask | old.contains.new.mask
      ## It's OK if intersecting.df is empty.
      intersecting.df <- union.intersections.df [intersecting.mask , ]
      new.left = min (intersecting.df$left, left)
      new.right = max (intersecting.df$right, right)
      ## Remove the intersecting rows from union.intersections.df, append the new row and sort.
      union.intersections.df.appended <- rbind (union.intersections.df [!intersecting.mask,],
                                                data.frame (left=new.left, right=new.right))
      union.intersections.df.sorted <- union.intersections.df.appended [order (union.intersections.df.appended$left), ]
      return (union.intersections.df.sorted)
    }
}

################################################################
### Helper Functions for remove.bomb.from.line.segment
################################################################
rebuild.union.intersections <- function (ls)
{
  stopifnot (is.list (ls))
  union.intersections.df <- new.union.intersections ()
  ls$union.intersections.df <-
    if (nrow(ls$bombs.df) == 0)
      {
        union.intersections.df
      }
    else {
      len <- nrow (ls$bombs.df)
      for (i in 1:len)
        {
          union.intersections.df <- add.segment.union.intersections (union.intersections.df,
                                                                     left=ls$bombs.df[i,"left"],
                                                                     right=ls$bombs.df[i,"right"],
                                                                     do.check = F)
        }
      union.intersections.df
    }
  return (ls)
}

remove.from.bombs <- function (ls, x, y, radius)
{
  stopifnot (is.list(ls), !is.null(ls),
             is.numeric(x), length(x)==1,
             is.numeric(y), length(y)==1,
             is.numeric(radius), length(radius)==1)
  
  if (nrow(ls$bombs.df) == 0)
    {
      return (ls)
    }
  
  mask <- find.bomb.mask (ls, x, y, radius)

  if (any (mask))
    {
      ls.new <- ls
      ls.new$bombs.df <- ls$bombs.df [!mask, ]
      return (ls.new)
    }
  else {
    return (ls)
  }    
}

find.index.bomb.slow <- function (bombing.df, x, y, r)
{
  stopifnot (nrow (bombing.df) > 0)
  len <- nrow (bombing.df)
  target.vect <- c (x, y, r)
  row <- NULL
  for (i in (1:len))
    {
      if (all (target.vect == bombing.df [i, ], na.rm = T))
        {
          row <- i
          break;
        }
    }
  return (row)
} 

## Returns bombs.df data frame.
find.bombs <- function (ls, x, y, radius)  {
  stopifnot (is.list(ls), !is.null(ls),
             is.numeric(x), length(x)==1,
             is.numeric(y), length(y)==1,
             is.numeric(radius), length(radius)==1)
  
  mask <- find.bomb.mask (ls, x, y, radius);
  return (ls$bombs.df [mask,])
}

helper.find.bomb.mask <- function (ls, x, y, radius)
{
  stopifnot (is.list(ls), !is.null(ls),
             is.numeric(x), length(x)==1,
             is.numeric(y), length(y)==1,
             is.numeric(radius), length(radius)==1)
  
  mask <- (ls$bombs.df$x == x) & (ls$bombs.df$y == y) & (ls$bombs.df$radius == radius)
  return (mask)
}

find.bomb.mask.mat <- Vectorize(helper.find.bomb.mask, c('x', 'y', 'radius'), SIMPLIFY=T)
find.bomb.mask <- function (ls, x, y, radius)
{
  stopifnot (is.list(ls), !is.null(ls),
             is.numeric(x), length(x)==1,
             is.numeric(y), length(y)==1,
             is.numeric(radius), length(radius)==1)
  
  mask.mat <- find.bomb.mask.mat (ls, x, y, radius)
  if (is.matrix (mask.mat))
    {
      mask.flat <- apply(mask.mat, MARGIN=1, "all")
      return (mask.flat)
    } else {
      return (mask.mat)
    }
}

##############################################
## Potential for a bombing configuration
##############################################
get.distance.between.points <- function (x1, y1, x2, y2)
{
  stopifnot (is.numeric(x1), length(x1)==1,
             is.numeric(y1), length(y1)==1,
             is.numeric(x2), length(x2)==1,
             is.numeric(y2), length(y2)==1)
  
  return (sqrt ( (x1-x2)^2 + (y1-y2)^2))
}

get.lens.area <- function (radius, triangular.height)
{
  R <- radius
  d <- triangular.height
  area <- R^2 * acos (d/R) - d * sqrt (R^2 - d^2)
  return (area)
}

## Requires that is.a.intersect.b is true.
## Requires that is.a.contains.b is false for both.
get.area.of.overlap.helper <- function (x1, y1, r1, x2, y2, r2)
{
  R <- r1
  r <- r2
  d <- get.distance.between.points (x1, y1, x2, y2)
  x <- (d^2 - r^2 + R^2) / (2 * d)  ## distance from c1 center to mid-lens.
  d1 <- x
  d2 <- d - x
  seg1 <- get.lens.area (r1, d1)
  seg2 <- get.lens.area (r2, d2)
  return (seg1 + seg2)
}

## Vector friendly
is.a.overlap.b <- function (x1, y1, r1, x2, y2, r2)
{
  d <- get.distance.between.points (x1, y1, x2, y2)
  return (d < (r1 + r2))
}

is.a.contains.b <- function (x1, y1, r1, x2, y2, r2)
{
  d <- get.distance.between.points (x1, y1, x2, y2)
  return ((d + r2) <= r1)
}

get.area.overlap <- function (x1, y1, r1, x2, y2, r2)
{
  if (!is.a.overlap.b (x1, y1, r1, x2, y2, r2))
    {
      return (0)
    } else if (is.a.contains.b (x1, y1, r1, x2, y2, r2))
      {
        area.of.b <- pi * r2^2
        return (area.of.b)
      } else if (is.a.contains.b (x2, y2, r2, x1, y1, r1))
        {
          area.of.b <- pi * r1^2
          return (area.of.b)
        } else {
          area.of.overlap <- get.area.of.overlap.helper (x1, y1, r1, x2, y2, r2)
          return (area.of.overlap)
        }
}

get.potential <- function (list.line.segment.observations,
                           proposed.bombing.df,
                           proposed.ls.list,
                           interacting.bombs.mat,
                           point.cost = 1,
                           mark.cost.scale = 1,
                           excl.scale = 1,
                           disagree.scale = 1)
{
  stopifnot (is.list (list.line.segment.observations),
             is.data.frame (proposed.bombing.df),
             is.list (proposed.ls.list),
             is.numeric (point.cost), length (point.cost) == 1,
             is.numeric (mark.cost.scale), length (mark.cost.scale) == 1,
             is.numeric (excl.scale), length (excl.scale) == 1,
             is.numeric (disagree.scale), length (disagree.scale) == 1)
  
  pot.internal <- get.potential.internal (proposed.bombing.df = proposed.bombing.df,
                                          point.cost = point.cost,
                                          mark.cost.scale = mark.cost.scale)
  pot.external <- get.potential.external (list.line.segment.observations = list.line.segment.observations,
                                          proposed.bombing.df = proposed.bombing.df,
                                          proposed.ls.list = proposed.ls.list,
                                          interacting.bombs.mat = interacting.bombs.mat,
                                          excl.scale = excl.scale,
                                          disagree.scale = disagree.scale)
  stopifnot (!is.na (pot.internal),
             !is.na (pot.external))
  return (pot.internal + pot.external)
}

## Returns line segments.
get.proposed.line.segment.observations <- function (list.line.segment.observations,
                                                    proposed.bombing.df) 
{
  ## More than once I've passed a single line.segment instead of a
  ## list of line segments.
  stopifnot (is.list (list.line.segment.observations),
             is.data.frame (proposed.bombing.df),
             !is.line.segment (list.line.segment.observations)) 
  proposed.line.segments <-
    if (length (list.line.segment.observations) == 0)
      {
        list()
      } else {
        lapply (list.line.segment.observations,
                function (ls)
                {
                                        #new.line.segment.from.observation (add.bombs.to.line.segment (ls, proposed.bombing.df))
                  add.bombs.to.line.segment (ls, proposed.bombing.df)
                })
      }
  return (proposed.line.segments)
}

get.potential.internal <- function (proposed.bombing.df,
                                    point.cost = 1,
                                    mark.cost.scale = 1)
{
  stopifnot (is.data.frame (proposed.bombing.df),
             is.numeric (point.cost), length (point.cost) ==1,
             is.numeric (mark.cost.scale), length (mark.cost.scale) ==1)
  len <- nrow (proposed.bombing.df)
  pot.internal.accum <- (len * point.cost
                         + if (mark.cost.scale == 0) { 0 }
                           else { mark.cost.scale * sum(proposed.bombing.df[,"radius"]^2)})
  stopifnot (!is.na (pot.internal.accum))
  return (pot.internal.accum)
}

get.potential.external <- function (list.line.segment.observations,
                                    proposed.bombing.df,
                                    proposed.ls.list,
                                    interacting.bombs.mat,
                                    excl.scale = 1,
                                    disagree.scale = 1)
{
  stopifnot (is.list (list.line.segment.observations),
             is.data.frame (proposed.bombing.df),
             is.list (proposed.ls.list),
             (length (list.line.segment.observations) == length (proposed.ls.list)),
             is.numeric (excl.scale), length (excl.scale) == 1,
             is.numeric (disagree.scale), length (disagree.scale) == 1)
  
  pot.exclude <- get.potential.exclude (interacting.bombs.mat = interacting.bombs.mat,
                                        excl.scale = excl.scale)
  pot.data <- get.potential.data (proposed.ls.list = proposed.ls.list,
                                  disagree.scale = disagree.scale)
  return (pot.exclude + pot.data)
}

## Returns the potential due to interacting bombs.
get.potential.exclude <- function (interacting.bombs.mat, excl.scale)
{
  #stopifnot (excl.scale >= 0)
  result <- 
    if ( (excl.scale == 0) || (nrow (interacting.bombs.mat) == 0) || global.skip.interacting) { 0 }
    else { excl.scale * sum (interacting.bombs.mat[,'area']) }
  stopifnot (!is.na (result))
  return (result)
}

get.potential.data <- function (proposed.ls.list, disagree.scale)
{
  stopifnot (is.list (proposed.ls.list),
             is.numeric (disagree.scale),
             length(disagree.scale) == 1)
  result <-
    if (length (proposed.ls.list) == 0)
      { 0 }
    else {
      ## This vector is (disagreement.length - agreement.length)
      ## Which is (total.length - 2 * agreement.length)
      disagree.minus.agree.v <- sapply (proposed.ls.list,
                                        function (ls)
                                        {
                                          diff.agree <- ls$length - 2 * ls$agreement.length
                                          if (diff.agree > 0) { global.diff.agree.scale * diff.agree }
                                          else { diff.agree }
                                        })
      stopifnot (is.vector (disagree.minus.agree.v))
      disagree.scale * sum (disagree.minus.agree.v)
    }
  return (result)
}

is.line.segments.identical <- function (ls1, ls2)
{
  return ((ls1$p1 == ls2$p1) && (ls1$p2 == ls2$p2))
}

## Returns a scalar value.
calculate.ls.agreement.length <- function (ls)
{
  ## Get change points (xor.segments)
  change.points <- sort (c (0,
                            ls$observed.intersections.df$left,
                            ls$observed.intersections.df$right,
                            ls$union.intersections.df$left,
                            ls$union.intersections.df$right,
                            ls$length))
  ##
  seg.lengths <- diff (change.points)
  ## Sum every other element of seg.lengths
  agreement.length <- sum (seg.lengths [seq.int (1, length (seg.lengths), 2)])
  return (agreement.length)
}

is.kernel.death.possible <- function (bombing.df)
{
  return (nrow (bombing.df) > 0)
}

is.kernel.resize.possible <- is.kernel.death.possible
is.kernel.translate.possible <- is.kernel.death.possible
is.kernel.split.possible <- is.kernel.death.possible
is.kernel.merge.possible <- is.kernel.death.possible
is.kernel.jump.possible <- is.kernel.translate.possible
is.kernel.slide.possible <- is.kernel.translate.possible


## The kernel selects randomly one point u in z (bombing.df) and proposes z' = z\u.
## ACTUALLY, it returns the proposed bombing and the updated line segments.
## Requires that the length of bombing.df be at least 1.
kernel.death <- function (bombing.df,
                          ls.list,
                          interacting.bombs.mat,
                          xlim,
                          ylim,
                          shape,
                          scale,
                          selected = NULL,
                          added.bomb.df = NULL,
                          skip.interacting = F)
{
  stopifnot (is.data.frame (bombing.df),
             is.list (ls.list))
  
  len <- nrow (bombing.df)
  ## This kernel requires that there be at least one point to remove.
  stopifnot (len > 0)
  if (is.null (selected))
    {
      selected <- sample (nrow (bombing.df), 1)
    }
  removed.bombing.df <- bombing.df [selected, ]
  proposed.bombing.df <- bombing.df [-selected, ]
  proposed.ls.list <- lapply (ls.list,
                              function (ls)
                              {
                                return (remove.bomb.from.line.segment (ls,
                                                                       removed.bombing.df[1,"x"],
                                                                       removed.bombing.df[1,"y"],
                                                                       removed.bombing.df[1,"radius"]))
                              })
  
  proposed.interacting.bombs.mat <-
    if (skip.interacting) { matrix() }
    else { remove.interacting.bombs.mat (interacting.bombs.mat, removed.bombing.df) }
  return (list (bombing.df = proposed.bombing.df,
                ls.list = proposed.ls.list,
                interacting.bombs.mat = proposed.interacting.bombs.mat,
                removed.bombing.df = removed.bombing.df,
                added.bombing.df = NULL))
}

## This kernel generates a new point u according to the uniform measure |.| / |S|
## and proposed z` = z U u.
## INPUT: bombing.df -- a bombing configuration.
## INPUT: ls.list -- a list of line segment objects.
## INPUT: interacting.bombs.mat -- a matrix of interacting bombs and overlap area.
## INPUT: xlim -- the x-range of the observation window.
## INPUT: ylim -- the y-range of the observation window.
## INPUT: shape -- the shape parameter for rweibull(.)
## INPUT: scale -- the scale parameter for rweibull(.)
## OUTPUT: list (bombing.df, ls.list).
kernel.birth <- function (bombing.df,
                          ls.list,
                          interacting.bombs.mat,
                          xlim,
                          ylim,
                          shape,
                          scale,
                          selected = NULL,
                          added.bomb.df = NULL,
                          skip.interacting = F)
{
  stopifnot (is.data.frame (bombing.df),
             is.list (ls.list),
             is.numeric (xlim), length (xlim) == 2,
             is.numeric (ylim), length (ylim) == 2,
             is.numeric (shape), length (shape) == 1,
             is.numeric (scale), length (scale) == 1,
             is.matrix(interacting.bombs.mat))
  if (is.null (added.bomb.df))
    {
      added.bomb.df <- new.bombing.empty (x = runif (1, xlim[1], xlim[2]),
                                          y = runif (1, ylim[1], ylim[2]),
                                          radius = rweibull (1, shape, scale))
    }
  proposed.bombing.df <- rbind (bombing.df, added.bomb.df)
  proposed.ls.list <- lapply (ls.list,
                              function (ls)
                              {
                                return (add.bombs.to.line.segment (ls, added.bomb.df))
                              })
  proposed.interacting.bombs.mat <- 
    if (skip.interacting)
      {
        matrix()
      }
    else
      {
        add.interacting.bombs.mat (interacting.bombs.mat = interacting.bombs.mat,
                                   bombing.df = proposed.bombing.df,
                                   add.bomb.df = added.bomb.df)
      }
  
  return (list (bombing.df = proposed.bombing.df,
                ls.list = proposed.ls.list,
                interacting.bombs.mat = proposed.interacting.bombs.mat,
                removed.bombing.df = NULL,
                added.bombing.df = added.bomb.df))
}

## Assumes bombing.df not empty.
## Returns NULL if not valid.
kernel.resize <- function (bombing.df,
                           ls.list,
                           interacting.bombs.mat,
                           xlim,
                           ylim,
                           shape,
                           scale,
                           selected = NULL,
                           added.bomb.df = NULL)
{
  stopifnot (nrow (bombing.df) > 0)
  delta <- diff (xlim) / 20
  delta.selected <- runif (1, min = -abs (delta), max = abs (delta))
  row.selected <- sample (nrow (bombing.df), 1)
  selected.radius <- bombing.df [row.selected, "radius"] + delta.selected
  if (selected.radius <= 0)
    {
      return (NULL)
    }
  removed.list <- kernel.death (bombing.df = bombing.df,
                                ls.list = ls.list,
                                interacting.bombs.mat = interacting.bombs.mat,
                                xlim = xlim,
                                ylim = ylim,
                                shape = shape,
                                scale = scale,
                                selected = row.selected)
  ## Call kernel.birth with the right args.
  added.bomb.df <- bombing.df [row.selected, ]
  added.bomb.df [1, "radius"] <- selected.radius
  added.list <- kernel.birth (bombing.df = removed.list$bombing.df,
                              ls.list = removed.list$ls.list,
                              interacting.bombs.mat = removed.list$interacting.bombs.mat,
                              xlim = xlim,
                              ylim = ylim,
                              shape = shape,
                              scale = scale,
                              added.bomb.df = added.bomb.df)
  return.list <- added.list
  return.list$removed.bombing.df <- removed.list$removed.bombing.df
  return (return.list)
}

kernel.translate <- function (bombing.df,
                              ls.list,
                              interacting.bombs.mat,
                              xlim,
                              ylim,
                              shape,
                              scale,
                              selected = NULL,
                              added.bomb.df = NULL)
{
  stopifnot (nrow (bombing.df) > 0)
  delta.x <- diff (xlim) / 20
  delta.y <- diff (ylim) / 20
  delta.x.selected <- runif (1, min = -abs (delta.x), max = abs (delta.x))
  delta.y.selected <- runif (1, min = -abs (delta.y), max = abs (delta.y))
  row.selected <- sample (nrow (bombing.df), 1)
  selected.x <- bombing.df [row.selected, "x"] + delta.x.selected
  selected.y <- bombing.df [row.selected, "y"] + delta.y.selected
  ## Remove the old bomb.
  removed.list <- kernel.death (bombing.df = bombing.df,
                                ls.list = ls.list,
                                interacting.bombs.mat = interacting.bombs.mat,
                                xlim = xlim,
                                ylim = ylim,
                                shape = shape,
                                scale = scale,
                                selected = row.selected)
  ## Add the new bomb.
  added.bomb.df <- bombing.df [row.selected, ]
  added.bomb.df [1, "x"] <- selected.x
  added.bomb.df [1, "y"] <- selected.y
  added.list <- kernel.birth (bombing.df = removed.list$bombing.df,
                              ls.list = removed.list$ls.list,
                              interacting.bombs.mat = removed.list$interacting.bombs.mat,
                              xlim = xlim,
                              ylim = ylim,
                              shape = shape,
                              scale = scale,
                              added.bomb.df = added.bomb.df)
  return.list <- added.list
  return.list$removed.bombing.df <- removed.list$removed.bombing.df
  return (return.list)
}

kernel.split <- function (bombing.df,
                          ls.list,
                          interacting.bombs.mat,
                          xlim,
                          ylim,
                          shape,
                          scale,
                          selected = NULL,
                          added.bomb.df = NULL)
{
  ## Randomly select a bomb:
  stopifnot (nrow (bombing.df) > 0)

  row.selected <- sample (nrow (bombing.df), 1)
  old.radius <-  bombing.df [row.selected, "radius"]
  old.area <- pi * old.radius^2
  old.x <- bombing.df [row.selected, "x"]
  old.y <- bombing.df [row.selected, "y"]

  ## Deal with the anchor bomb.
  x1 <- old.x
  y1 <- old.y
  area1 <- runif (1,
                  min = old.area / 2,
                  max = old.area)
  r1 <- sqrt (area1 / pi)
  
  ## Deal with the aux bomb.
  area2 <- old.area - area1
  r2 <- sqrt (area2 / pi)
  max.distance <- r1 + r2
  while (1)
    {
      ## Find coordinates such that aux bomb overlaps with the anchor bomb.
      x2.delta <- runif (1, min = -max.distance, max = max.distance)
      y2.delta <- runif (1, min = -max.distance, max = max.distance)
      distance.2 <- sqrt (abs (x2.delta)^2 + abs (y2.delta)^2)
      if (distance.2 <= max.distance)
        {
          break;
        }
    }
  x2 <- old.x + x2.delta
  y2 <- old.y + y2.delta

  ## Remove the selected bomb:
  removed.list <- kernel.death (bombing.df = bombing.df,
                                ls.list = ls.list,
                                interacting.bombs.mat = interacting.bombs.mat,
                                xlim = xlim,
                                ylim = ylim,
                                shape = shape,
                                scale = scale,
                                selected = row.selected)
  ## Add new bombs.
  added.bomb.df <- new.bombing.empty (x = c (x1, x2),
                                      y = c (y1, y2),
                                      radius = c (r1, r2))
  added1.list <- kernel.birth (bombing.df = removed.list$bombing.df,
                               ls.list = removed.list$ls.list,
                               interacting.bombs.mat = removed.list$interacting.bombs.mat,
                               xlim = xlim,
                               ylim = ylim,
                               shape = shape,
                               scale = scale,
                               added.bomb.df = added.bomb.df[1,])
  added2.list <- kernel.birth (bombing.df = added1.list$bombing.df,
                               ls.list = added1.list$ls.list,
                               interacting.bombs.mat = added1.list$interacting.bombs.mat,
                               xlim = xlim,
                               ylim = ylim,
                               shape = shape,
                               scale = scale,
                               added.bomb.df = added.bomb.df[2,])
  return.list <- added2.list
  return.list$removed.bombing.df <- removed.list$removed.bombing.df
  return.list$added.bombing.df <- rbind (added1.list$added.bombing.df,
                                         added2.list$added.bombing.df)
  return (return.list)
}

## dnames <- list (NULL, dimnames = c("x1", "y1", "r1", "x2", "y2", "r2", "area"))
kernel.merge <- function  (bombing.df,
                           ls.list,
                           interacting.bombs.mat,
                           xlim,
                           ylim,
                           shape,
                           scale,
                           selected = NULL,
                           added.bomb.df = NULL)
{
  if (nrow (interacting.bombs.mat) == 0)
    {
      return (NULL)
    }
  ## Randomly select a pair of interacting bombs.
  interacting.row.selected <- sample (nrow (interacting.bombs.mat), 1)
  int.bombs <- interacting.bombs.mat [interacting.row.selected, , drop=F]
  ## Find the bomb that has the bigger area. This is the center of the new bomb.
  r.new <- {
    area1 <- pi * int.bombs [1, "r1"]^2
    area2 <- pi * int.bombs [1, "r2"]^2
    sqrt ((area1 + area2) / pi)
  }
  new.bomb.xy <-
    if (int.bombs [1, "r1"] >= int.bombs [1, "r2"])
      {
        int.bombs [1, c("x1", "y1")]
      }
    else
      {
        int.bombs [1, c("x2", "y2")]
      }
  ## Remove the old bombs.
  remove.row1 <- find.index.bomb.slow (bombing.df = bombing.df,
                                       x = int.bombs[1, "x1"],
                                       y = int.bombs[1, "y1"],
                                       r = int.bombs[1, "r1"])
  removed.list1 <- kernel.death (bombing.df = bombing.df,
                                 ls.list = ls.list,
                                 interacting.bombs.mat = interacting.bombs.mat,
                                 xlim = xlim,
                                 ylim = ylim,
                                 shape = shape,
                                 scale = scale,
                                 selected = remove.row1)
  remove.row2 <- find.index.bomb.slow (bombing.df = removed.list1$bombing.df,
                                       x = int.bombs[1, "x2"],
                                       y = int.bombs[1, "y2"],
                                       r = int.bombs[1, "r2"])
  removed.list2 <- kernel.death (bombing.df = removed.list1$bombing.df,
                                 ls.list = removed.list1$ls.list,
                                 interacting.bombs.mat = removed.list1$interacting.bombs.mat,
                                 xlim = xlim,
                                 ylim = ylim,
                                 shape = shape,
                                 scale = scale,
                                 selected = remove.row2)
  ## Add the new bomb.
  added.bomb.df <- new.bombing.empty (x = new.bomb.xy [1],
                                      y = new.bomb.xy [2],
                                      radius = r.new)
  added.list <- kernel.birth (bombing.df = removed.list2$bombing.df,
                              ls.list = removed.list2$ls.list,
                              interacting.bombs.mat = removed.list2$interacting.bombs.mat,
                              xlim = xlim,
                              ylim = ylim,
                              shape = shape,
                              scale = scale,
                              added.bomb.df = added.bomb.df)
  return.list <- added.list
  return.list$removed.bombing.df <- rbind (removed.list1$removed.bombing.df,
                                           removed.list2$removed.bombing.df)
  return (return.list)
}

## This is a type of a kernel function, passed to turn.crank::helper
## R_death (z, z\u)
get.green.ratio.death <- function (old.bombing.df,
                                   old.potential.annealed,
                                   old.interacting.bombs.mat,
                                   new.bombing.df,
                                   new.potential.annealed,
                                   new.interacting.bombs.mat,
                                   removed.bombing.df,
                                   added.bombing.df,
                                   area.window,
                                   lambda)
{

  result <- nrow (old.bombing.df) / (area.window * lambda) * exp (old.potential.annealed - new.potential.annealed)
  stopifnot (!is.na (result), is.numeric (result), length (result) == 1)
  return (result)
}

## R_birth (z, z U u)
get.green.ratio.birth <- function (old.bombing.df,
                                   old.potential.annealed,
                                   old.interacting.bombs.mat,
                                   new.bombing.df,
                                   new.potential.annealed,
                                   new.interacting.bombs.mat,
                                   removed.bombing.df,
                                   added.bombing.df,
                                   area.window,
                                   lambda)
{
  result <- (area.window * lambda) / nrow (new.bombing.df) * exp (old.potential.annealed - new.potential.annealed)
  stopifnot (!is.na (result))
  return (result)
}

## R_resize (z, (z / v) U u) I think it's just the usual MH ratio:
## h(z') / h(z) since it's a non-jumping transformation, and the
## change is based on a parameter drawn from a symetric set. In my
## case, the set is the interval [-delta, delta].
get.green.ratio.resize <- function (old.bombing.df,
                                    old.potential.annealed,
                                    old.interacting.bombs.mat,
                                    new.bombing.df,
                                    new.potential.annealed,
                                    new.interacting.bombs.mat,
                                    removed.bombing.df,
                                    added.bombing.df,
                                    area.window,
                                    lambda)
{
  result <- exp (old.potential.annealed - new.potential.annealed)
  stopifnot (!is.na (result))
  return (result)
}

## R_translate (z, (z / v) U u) I think it's just the usual MH ratio:
## h(z') / h(z) since it's a non-jumping transformation, and the
## change is based on a parameter drawn from a symetric set. In my
## case, the set is the interval [-delta, delta] for both x and y coordinates.
get.green.ratio.translate <- function (old.bombing.df,
                                       old.potential.annealed,
                                       old.interacting.bombs.mat,
                                       new.bombing.df,
                                       new.potential.annealed,
                                       new.interacting.bombs.mat,
                                       removed.bombing.df,
                                       added.bombing.df,
                                       area.window,
                                       lambda)
{
  result <- exp (old.potential.annealed - new.potential.annealed)
  stopifnot (!is.na (result))
  return (result)
}

get.green.ratio.jump <- get.green.ratio.translate
get.green.ratio.slide <- get.green.ratio.translate


get.green.ratio.split <- function (old.bombing.df,
                                   old.potential.annealed,
                                   old.interacting.bombs.mat,
                                   new.bombing.df,
                                   new.potential.annealed,
                                   new.interacting.bombs.mat,
                                   removed.bombing.df,
                                   added.bombing.df,
                                   area.window,
                                   lambda)
{
  stopifnot (nrow (added.bombing.df) == 2,
             nrow (removed.bombing.df) == 1,
             nrow (new.interacting.bombs.mat) > 0,
             nrow (old.bombing.df) > 0)
  
  ## j.merge and j.split are the prob. of selecting exactly this move under this kernel.
  j.merge <- 1 / nrow (new.interacting.bombs.mat)
  j.split <- 1 / nrow (old.bombing.df) / (pi * removed.bombing.df[1, "radius"]^2 / 2)  / (lambda * pi * (added.bombing.df[1,"radius"] + added.bombing.df[2,"radius"])^2)
  return (exp (old.potential.annealed - new.potential.annealed) * j.merge / j.split)
}

get.green.ratio.merge <- function (old.bombing.df,
                                   old.potential.annealed,
                                   old.interacting.bombs.mat,
                                   new.bombing.df,
                                   new.potential.annealed,
                                   new.interacting.bombs.mat,
                                   removed.bombing.df,
                                   added.bombing.df,
                                   area.window,
                                   lambda)
{
  stopifnot (nrow (removed.bombing.df) == 2,
             nrow (added.bombing.df) == 1,
             nrow (old.interacting.bombs.mat) > 0,
             nrow (new.bombing.df) > 0)
  j.merge <- 1 / nrow (old.interacting.bombs.mat)
  j.split <- 1 / nrow (new.bombing.df) / (pi * added.bombing.df[1, "radius"]^2 / 2) / (lambda * pi * (removed.bombing.df[1, "radius"] + removed.bombing.df[2, "radius"])^2)
  return (exp (old.potential.annealed - new.potential.annealed) * j.split / j.merge)
}

## OUTPUT: 
## return (list (bombing.df = proposal.list$bombing.df,
##                            ls.list = proposal.list$ls.list,
##                            potential = proposal.potential,
##                            interacting.bombs.mat = proposal.list$interacting.bombs.mat,
##                            annealing.scale = annealing.scale,
##                            removed.bombing.df = proposal.list$removed.bombing.df,
##                            added.bombing.df = proposal.list$added.bombing.df,
##                            start.index = 1,
##                            end.index = 1))
turn.crank <- function (list.line.segment.observations, ## This is the observed data.
                        current.bombing.df,
                        current.lines,
                        current.potential,
                        current.interacting.bombs.mat,
                        current.added.bombing.df,
                        current.removed.bombing.df,
                        annealing.scale,
                        area.window,
                        point.cost,
                        mark.cost.scale,
                        excl.scale,
                        disagree.scale,
                        xlim,
                        ylim,
                        shape,
                        scale,
                        lambda,
                        use.anchors)
{
  ## Returns list (bombing.df, ls.list)
  stopifnot (is.list (list.line.segment.observations),
             is.data.frame (current.bombing.df),
             is.list (current.lines),
             is.matrix (current.interacting.bombs.mat),
                                        #is.data.frame (current.added.bombing.df),
                                        #is.data.frame (current.removed.bombing.df),
             is.numeric (area.window), length (area.window) == 1,
             is.numeric (point.cost), length (point.cost) == 1,
             is.numeric (mark.cost.scale), length (mark.cost.scale) == 1,
             is.numeric (excl.scale), length (excl.scale) == 1,
             is.numeric (disagree.scale), length (disagree.scale) == 1,
             is.numeric (xlim), length (xlim) == 2,
             is.numeric (ylim), length (ylim) == 2,
             is.numeric (shape), length (shape) == 1,
             is.numeric (scale), length (scale) == 1)
  
  
  ## The helper function produces the next step of the MC (including calculating the Green ratio).
  ## INPUT: The chosen kernel and corresponding Green function.
  ## OUTPUT: The next step X_(t+1) = (z' or z)
  helper <- function (kernel.fun, kernel.check.fun, green.fun, msg)
    {
      ##  current.list <- list (bombing.df = current.bombing.df,
      ##                             ls.list = current.lines,
      ##                             potential = current.potential,
      ##                             interacting.bombs.mat = current.interacting.bombs.mat,
      ##                             annealing.scale = annealing.scale,
      ##                             added.bombing.df = current.added.bombing.df,
      ##                             removed.bombing.df = current.removed.bombing.df,
      ##                             start.index = 1,
      ##                             end.index = 1)
      
      if (kernel.check.fun (current.bombing.df))
        {
          if (dbmsg) cat (paste ("Proposed: ", msg, " "))
          proposal.list <- kernel.fun (bombing.df = current.bombing.df,
                                       ls.list = current.lines,
                                       interacting.bombs.mat = current.interacting.bombs.mat,
                                       xlim = xlim,
                                       ylim = ylim,
                                       shape = shape,
                                       scale = scale)
          if (is.null (proposal.list))
            { ## proposal.list could be null if the kernel proposed a non-admissible configuration.
              ## return (current.list)
              return (NULL) ## NULL means no change.
            }
          else
            {
              proposal.potential <- get.potential (list.line.segment.observations = list.line.segment.observations,
                                                   proposed.bombing.df = proposal.list$bombing.df,
                                                   proposed.ls.list = proposal.list$ls.list,
                                                   interacting.bombs.mat = proposal.list$interacting.bombs.mat,
                                                   point.cost = point.cost,
                                                   mark.cost.scale = mark.cost.scale,
                                                   excl.scale = excl.scale,
                                                   disagree.scale = disagree.scale)
              if (dbmsg) cat (paste ("Potential (current, proposed): ", current.potential * annealing.scale, proposal.potential * annealing.scale, "\n"))
              green.ratio <- green.fun (old.bombing.df = current.bombing.df,
                                        old.potential.annealed = annealing.scale * current.potential,
                                        old.interacting.bombs.mat = current.interacting.bombs.mat,
                                        new.bombing.df = proposal.list$bombing.df,
                                        new.potential.annealed = annealing.scale * proposal.potential,
                                        new.interacting.bombs.mat = proposal.list$interacting.bombs.mat,
                                        removed.bombing.df = proposal.list$removed.bombing.df,
                                        added.bombing.df = proposal.list$added.bombing.df,
                                        area.window = area.window,
                                        lambda = lambda)
              if (dbmsg) cat (paste (" Green Ratio: ", green.ratio))
              if (runif (1) < green.ratio)
                {
                  ## Select the proposed state.
                  if (dbmsg) cat (paste ("............SELECTED "))
                  return (list (bombing.df = proposal.list$bombing.df,
                                ls.list = proposal.list$ls.list,
                                potential = proposal.potential,
                                interacting.bombs.mat = proposal.list$interacting.bombs.mat,
                                annealing.scale = annealing.scale,
                                removed.bombing.df = proposal.list$removed.bombing.df,
                                added.bombing.df = proposal.list$added.bombing.df,
                                start.index = 1,
                                end.index = 1))
                }
              else
                {
                  ## Select the current state (no change).
                  ## return (current.list)
                  return (NULL)
                }
            }
        }
      else
        {
          ##return (current.list)
          return (NULL)
        }
    }
  
  ## This part chooses a perturbation kernel Q_m(.,.) with probability p_m(z)
  if (!use.anchors)
    {
      chosen.kernel <- sample (6, 1)
      result <- switch (chosen.kernel,
                        ## Birth
                        helper (kernel.fun = kernel.death,
                                kernel.check.fun = is.kernel.death.possible,
                                green.fun = get.green.ratio.death,
                                msg = "Death"),
                        ## Death
                        helper (kernel.fun = kernel.birth,
                                kernel.check.fun = function (x)
                                { TRUE },
                                green.fun = get.green.ratio.birth,
                                msg = "Birth"),
                        ## Resize
                        helper (kernel.fun = kernel.resize,
                                kernel.check.fun = is.kernel.resize.possible,
                                green.fun = get.green.ratio.resize,
                                msg = "Resize"),
                        ## Translate
                        helper (kernel.fun = kernel.translate,
                                kernel.check.fun = is.kernel.translate.possible,
                                green.fun = get.green.ratio.translate,
                                msg = "Translate"),
                        ## Split
                        helper (kernel.fun = kernel.split,
                                kernel.check.fun = is.kernel.split.possible,
                                green.fun = get.green.ratio.split,
                                msg = "Split"),
                        ## Merge
                        helper (kernel.fun = kernel.merge,
                                kernel.check.fun = is.kernel.merge.possible,
                                green.fun = get.green.ratio.merge,
                                msg = "Merge")
                        )
    }
  else
    {
      chosen.kernel <- sample (2, 1)
      result <- switch (chosen.kernel,
                        ## Jump
                        helper (kernel.fun = kernel.jump,
                                kernel.check.fun = is.kernel.jump.possible,
                                green.fun = get.green.ratio.jump,
                                msg = "Jump"),
                        ## Slide
                        helper (kernel.fun = kernel.slide,
                                kernel.check.fun = is.kernel.slide.possible,
                                green.fun = get.green.ratio.slide,
                                msg = "Slide"))
    }
  return (result)
}

## OUTPUT: list (..., list (bombing.df,
##                          ls.list, ## line segment
##                          potential,
##                          interacting.bombs.mat,
##                          annealing.scale,
##                          added.bombing.df,
##                          removed.bombing.df,
##                          start.index,
##                          end.index))
turn.crank.n <- function (n,
                          list.line.segment.observations, ## This is the observed data.
                          point.cost,
                          mark.cost.scale,
                          excl.scale,
                          disagree.scale,
                          xlim,
                          ylim,
                          shape,
                          scale,
                          lambda,
                          temp.start,
                          annealing.alpha,
                          prev.list = NULL,
                          use.anchors)
{
  stopifnot (is.numeric (n), length (n) == 1,
             is.list (list.line.segment.observations),
             is.numeric (point.cost), length (point.cost) == 1,
             is.numeric (mark.cost.scale), length (mark.cost.scale) == 1,
             is.numeric (excl.scale), length (excl.scale) == 1,
             is.numeric (disagree.scale), length (disagree.scale) == 1,
             is.numeric (xlim), length (xlim) == 2,
             is.numeric (ylim), length (ylim) == 2,
             is.numeric (shape), length (shape) == 1,
             is.numeric (scale), length (scale) == 1,
             is.numeric (lambda), length (lambda) == 1)

  area.window <- abs((xlim[1] - xlim[2]) * (ylim[1] - ylim[2]))
  ## Create empty bombing configuration:
  next.list <- list (bombing.df = new.bombing.empty(),
                     ls.list =
                     if (length (list.line.segment.observations) > 0)
                     {
                       lapply (list.line.segment.observations, new.line.segment.from.observation)
                     }
                     else {
                       list ()
                     },
                     potential = 0,
                     interacting.bombs.mat = new.interacting.bombs.mat (),
                     annealing.scale = 1,
                     added.bombing.df = NULL,
                     removed.bombing.df = NULL,
                     start.index = 1,
                     end.index = 1)
  if (!is.null (prev.list))
    {
      next.list $ ls.list <- get.proposed.line.segment.observations (next.list$ls.list,
                                                                     prev.list$bombing.df)
      next.list $ bombing.df <- prev.list$bombing.df
      ## TODO I'm commenting this out as a bandaid to *force* the first proposal through.
      ## next.list $ potential <- prev.list$potential
      next.list $ potential <- 100000
      next.list $ interacting.bombs.mat <- prev.list$interacting.bombs.mat
    }
  ## Loop
  chain.list <- list(next.list)
  current.index <- 1
  i <- 1
  window.size <- 1
  workers <- getDoParWorkers()
  pb <- txtProgressBar (min = 1, max = n, style = 3)
  cat (paste ('\nturn.crank.n ()\n'))
  while (i <= n)
    {
      setTxtProgressBar (pb, i)
      if (global.grow.window.size) if (dbmsg) cat ('\n          ***************\n');
      next.list.list <-
        foreach (j = seq (i, length = workers * window.size)) %:% when (j <= n) %dopar%
      {
        if (dbmsg) cat (paste("\nStep: ",j," ", "NumPoints", nrow (next.list$bombing.df), " "))
        annealing.scale <- (annealing.alpha^(-j) * temp.start)
        turn.crank (list.line.segment.observations = list.line.segment.observations,
                    current.bombing.df = next.list$bombing.df,
                    current.lines = next.list$ls.list,
                    current.potential = next.list$potential,
                    current.interacting.bombs.mat = next.list$interacting.bombs.mat,
                    current.added.bombing.df = next.list$added.bombing.df,
                    current.removed.bombing.df = next.list$removed.bombing.df,
                    annealing.scale = annealing.scale,
                    area.window = area.window,
                    point.cost = point.cost,
                    mark.cost.scale = mark.cost.scale,
                    excl.scale = excl.scale,
                    disagree.scale = disagree.scale,
                    xlim = xlim,
                    ylim = ylim,
                    shape = shape,
                    scale = scale,
                    lambda = lambda,
                    use.anchors = use.anchors)
      }
      for (k in 1:length (next.list.list))
        {
          if (is.null (next.list.list [[k]]))
            { ## This is a repeat. Update end.index and try again.
              chain.list[[current.index]]$end.index <- i + k - 1
              next.list <- chain.list[[current.index]]
            }
          else
            { ## Not a repeat.
              next.list <- next.list.list [[k]]
              next.list$end.index <- next.list$start.index <- i + k - 1
              current.index <- current.index + 1
              chain.list[[current.index]] <- next.list
              if (draw)
                {
                  display.grob.segments (list.line.segment.observations, next.list$bombing.df)
                }
              break;
            }
        }
      i <- i + k
      if (global.grow.window.size)
        {
          window.size <-
            if (k == length (next.list.list)) { window.size + 1 }
            else { max (1, floor (window.size / 2)) }
        }
    }
  close (pb)
  return (chain.list)
}

## returns
##   2 col matrix
##   always includes the first and last points, even if they're the same.
intermediate.points <- function(x1, y1, x2, y2)
{
  dx <- abs(x2 - x1)
  dy <- abs(y2 - y1)
  ## Check for degenerate case.
  if ((dx == 0) && (dy == 0))
    {
      x <- x1
      y <- y1
      return (data.frame(x = c(x,x), y = c(y,y)))
    }
  flip <- 0
  if (dx >= dy)
    {
      if (x1 > x2)
        {
          ## Always draw from left to right.
          t <- x1; x1 <- x2; x2 <- t;
          t <- y1; y1 <- y2; y2 <- t;
          flip <- 1
        }
      m <- (y2 - y1)/(x2 - x1)
      x <- x1:x2
      y <- round(y1 + m*(x - x1))
    } else
  {
    if (y1 > y2)
      {
        ## Always draw from left to right.
        t <- x1; x1 <- x2; x2 <- t;
        t <- y1; y1 <- y2; y2 <- t;
        flip <- 1
      }
    m <- (x2 - x1)/(y2 - y1);
    y <- y1:y2
    x <- round(x1 + m*(y - y1))
  }
  if (flip)
    {
      x <- rev(x)
      y <- rev(y)
    }
  return (cbind(x,y))
}

## Sample pot.field.mat along the specified line.
## INPUT line.segment = x1, y1, x2, y2, pot.field.mat, dist.density
##       -- This is the proposed next sample
## INPUT pot.field.mat is the potential field.
## OUPUT A potential for the line
get.potential.proposed.line.segment <- function (x1, y1, x2, y2, pot.field.mat, dist.density, prc.mat)
{
  stopifnot (is.matrix (pot.field.mat),
             nrow (pot.field.mat) == ncol (pot.field.mat),
             nrow (pot.field.mat) == nrow (dist.density),
             ncol (pot.field.mat) == ncol (dist.density)) ## I'd like to work with squares.
  ## pot.field.mat[5,10] (row 5 column 10) is x=5, y=10.
  ##  Convert end points of line segment to matrix coordinates.
  point.start <- pmin (pmax (ceiling (c (x1,
                                         y1)
                                      * c (ncol (pot.field.mat),
                                           nrow (pot.field.mat))),
                             1),
                       c (ncol (pot.field.mat),
                          nrow (pot.field.mat)))
  point.end <- pmin (pmax (ceiling (c (x2,
                                       y2)
                                    * c (ncol (pot.field.mat),
                                         nrow (pot.field.mat))),
                           1),
                     c (ncol (pot.field.mat),
                        nrow (pot.field.mat)))
  
                                        #point.end <- pmin( pmax (ceiling (c(x2, y2) * nrow (pot.field.mat)), 1), nrow (pot.field.mat))
  ##  Get intermediate coordinates. cbind (x,y)
  int.points <- intermediate.points (point.start[1], point.start[2], point.end[1], point.end[2])
  ##  Conver intermediate coordinates to vector coordinates for a
  ##  matrix so that I can use a single index.
  vect.coordinates <- (int.points[,1]-1) * ncol (pot.field.mat) + int.points[,2]
  line.seg.length <- sqrt( (x1 - x2)^2 + (y1 - y2)^2)
  integration.scale <- (line.seg.length / length (vect.coordinates))
  unit.line.y.norm <- (x2 - x1) / line.seg.length
  unit.line.x.norm <- (y2 - y1) / line.seg.length
  data.potential <-
    {
      ##  Sample the intermediate vector coordinates.
      data.samples <- pot.field.mat [vect.coordinates]
      line.dot.rot <-
        if (!is.null (prc.mat))
          {
            abs (unit.line.x.norm * prc.mat ['rot.x', vect.coordinates]
                 + unit.line.y.norm * prc.mat ['rot.y', vect.coordinates])
          }
        else
          {
            1
          }
      line.dot.rel.center <-
        if (!is.null (prc.mat))
          {
            abs (unit.line.x.norm * prc.mat ['unit.rel.center.x', vect.coordinates]
                 + unit.line.x.norm * prc.mat ['unit.rel.center.y', vect.coordinates])
          }
        else
          {
            1
          }
      -1 * sum (data.samples * line.dot.rel.center * line.dot.rot, na.rm = T) * integration.scale
    }
  dist.potential <-
    {
      dist.samples <- dist.density [vect.coordinates]
      -1 * sum (dist.samples) * integration.scale
    }
  return (list (data.potential = data.potential,
                dist.potential = dist.potential))
}

## Assumes a unit square.
## INPUT: the x,y coordinates of the anchor point.
## OUTPUT: A list (x, y) of coordinates of a randomly selected border point
##  not on the same side as the anchor.  
get.random.point.from <- function (x1, y1)
{
  stopifnot ((x1 == 0) || (x1 == 1) || (y1 == 0) || (y1 == 1))
  stopifnot ((0 <= x1) && (x1 <= 1) && (0 <= y1) && (y1 <= 1))
  interior.x <- runif(1)
  interior.y <- runif(1)
  orig.candidates <- data.frame(pos = c ('top', 'bottom', 'left', 'right'),
                                x = c (runif(1), runif(1), 0, 1),
                                y = c (1, 0, runif(1), runif(1)))
  cand.1 <-
    if (x1 == 0)
      {  orig.candidates [ ! orig.candidates$pos == 'left', ] }
    else if (x1 == 1)
      { orig.candidates [ ! orig.candidates$pos == 'right', ] }
    else { orig.candidates }
  stopifnot (is.data.frame (cand.1))
  cand.2 <-
    if (y1 == 0)
      { cand.1 [ ! cand.1$pos == 'bottom', ] }
    else if (y1 == 1)
      { cand.1 [ ! cand.1$pos == 'top', ] }
    else { cand.1 }
  stopifnot (is.data.frame (cand.2),
             nrow (cand.2) >= 2,
             nrow (cand.2) <= 4)
  selected <- sample (nrow (cand.2), 1)
  return (list (x=cand.2[selected, 'x'],
                y=cand.2[selected, 'y']))
}

####
## DATA STRUCTURE: matrix with col names:
## c("x", "y", "angle", "angle.cos", "data.potential", "dist.potential", "len"))
####
new.ls.path <- function (x=NA, y=NA, angle=NA, angle.cos=NA, data.potential=NA, dist.potential=NA, len=NA)
{
  stopifnot (length (x) > 0,
             length (y) > 0,
             length (x) == length (y))
  dnames <- list (NULL, dimnames = c("x", "y", "angle", "angle.cos",  "data.potential", "dist.potential", "len" ))
  if (is.na (angle))
    { angle <- rep (NA, length (x)) }
  if (is.na (angle.cos))
    { angle.cos <- rep (NA, length (x)) }
  if (is.na (data.potential))
    { data.potential <- rep (NA, length (x)) }
  if (is.na (dist.potential))
    { dist.potential <- rep (NA, length (x)) }
  if (is.na (len))
    { len <- rep (NA, length (x)) }
  data <- c (x, y, angle, angle.cos, data.potential, dist.potential, len)
  stopifnot (length (data) %% 7 == 0)
  ls.path <- matrix (data,
                     ncol = length (dnames$dimnames),
                     dimnames = dnames)
  return (ls.path)
}

## This will updates all the potential precomputes as well.
insert.ls.path <- function (ls.path,
                            x,
                            y,
                            position,
                            pot.field.mat,
                            dist.density,
                            prc.mat)
{
  stopifnot (is.matrix (ls.path),
             is.matrix (pot.field.mat),
             0 <= position,
             position <= nrow (ls.path))
  ## Insert into position.
  updated.ls.path <- rbind (ls.path [0:position,],
                            new.ls.path (x, y),
                            if (position < nrow (ls.path))
                            { ls.path [(position+1):nrow (ls.path), ] })
  new.position <- position + 1
  next.position <- new.position + 1
  prev.position <- new.position - 1
  ls.path <- NULL
  position <- NULL
  updated.ls.path [new.position, ] <- update.path.point (ls.path = updated.ls.path,
                                                         position = new.position,
                                                         pot.field.mat = pot.field.mat,
                                                         role = "new",
                                                         dist.density = dist.density,
                                                         prc.mat = prc.mat)
  if (1 <= prev.position)
    {
      updated.ls.path [prev.position, ] <-
        update.path.point (ls.path = updated.ls.path,
                           position = prev.position,
                           pot.field.mat,
                           role = "prev",
                           dist.density = dist.density,
                           prc.mat = prc.mat)
    }
  if (next.position <= nrow (updated.ls.path))
    {
      updated.ls.path [next.position, ] <-
        update.path.point (ls.path = updated.ls.path,
                           position = next.position,
                           pot.field.mat = pot.field.mat,
                           role = "next",
                           dist.density = dist.density,
                           prc.mat = prc.mat)
    }
  return (updated.ls.path)
}

remove.ls.path <- function (ls.path,
                            position,
                            pot.field.mat,
                            dist.density,
                            prc.mat)
{
  stopifnot (is.matrix (ls.path),
             is.matrix (pot.field.mat),
             0 < position,
             position <= nrow (ls.path))
  ## Remove from position
  updated.ls.path <- ls.path [-position, ]
  ls.path <- NULL
  ## Update the prev and next guys.
  prev.pos <- position - 1
  next.pos <- position

  if (1 <= prev.pos)
    {
      updated.ls.path [prev.pos, ] <-
        update.path.point (ls.path = updated.ls.path,
                           position = prev.pos,
                           pot.field.mat = pot.field.mat,
                           role = "prev",
                           dist.density = dist.density,
                           prc.mat = prc.mat)
    }
  if (next.pos <= nrow (updated.ls.path))
    {
      updated.ls.path [next.pos, ] <-
        update.path.point (ls.path = updated.ls.path,
                           position = next.pos,
                           pot.field.mat = pot.field.mat,
                           role = "next",
                           dist.density = dist.density,
                           prc.mat = prc.mat)
    }
  return (updated.ls.path)
  
}

translate.ls.path <- function (ls.path,
                               position,
                               x,
                               y,
                               pot.field.mat,
                               dist.density,
                               prc.mat)
{
  stopifnot (1 < position  && position <= nrow (ls.path),
             is.matrix (ls.path),
             is.matrix (pot.field.mat))
  prev.position <- position - 1
  next.position <- position + 1
  updated.ls.path <- ls.path
  updated.ls.path [position, "x"] <- x
  updated.ls.path [position, "y"] <- y
  updated.ls.path [position, ] <- update.path.point (ls.path = updated.ls.path,
                                                     position = position,
                                                     pot.field.mat = pot.field.mat,
                                                     role = "new",
                                                     dist.density = dist.density,
                                                     prc.mat = prc.mat)
  
  if (1 <= prev.position)
    {
      updated.ls.path [prev.position, ] <-
        update.path.point (ls.path = updated.ls.path,
                           position = prev.position,
                           pot.field.mat = pot.field.mat,
                           role = "prev",
                           dist.density = dist.density,
                           prc.mat = prc.mat)
    }
  if (next.position <= nrow (updated.ls.path))
    {
      updated.ls.path [next.position, ] <-
        update.path.point (ls.path = updated.ls.path,
                           position = next.position,
                           pot.field.mat = pot.field.mat,
                           role = "next",
                           dist.density = dist.density,
                           prc.mat = prc.mat)
    }
  return (updated.ls.path)
}

update.path.point <- function (ls.path,
                               position,
                               pot.field.mat,
                               role,
                               dist.density,
                               prc.mat)
{
  stopifnot (any (role == "next",
                  role == "new",
                  role == "prev"))
  ## figure out if I have an angle.
  new.angle <-
    if ((1 < position) && (position < nrow (ls.path)))
      { ## I have an angle if I'm not the first nor the last.
        slopeA <-
          diff (ls.path [(position-1):position, "y"]) /
            diff (ls.path [(position-1):position, "x"])
        slopeB <-
          diff (ls.path [position:(position+1), "y"]) /
            diff (ls.path [position:(position+1), "x"])
        diff.angle <- abs (atan (slopeA) - atan (slopeB))
        min (diff.angle, 2*pi - diff.angle)
      }
    else
      {
        NA
      }
  new.angle.cos <- cos (new.angle)

  ## Figure out the data potential for the line segment starting at this point.
  new.data.dist.and.len.potential <-
    if ((position < nrow (ls.path)) && (role != "next"))
      {
        x1 <- ls.path [position, "x"]
        y1 <- ls.path [position, "y"]
        x2 <- ls.path [position+1, "x"]
        y2 <- ls.path [position+1, "y"]
        dist.and.data.potentials <- get.potential.proposed.line.segment (x1 = x1,
                                                                         y1 = y1,
                                                                         x2 = x2 ,
                                                                         y2 = y2,
                                                                         pot.field.mat = pot.field.mat,
                                                                         dist.density = dist.density,
                                                                         prc.mat = prc.mat)
        len <- sqrt ( (y2 - y1)^2 + (x2 - x1)^2 )
        c (dist.and.data.potentials, list (len = len))
      }
    else
      {
        list (data.potential = NA,
              dist.potential = NA,
              len = NA)
      }
  return (new.ls.path (x = ls.path [position, "x"],
                       y = ls.path [position, "y"],
                       angle = new.angle,
                       angle.cos = new.angle.cos,
                       data.potential = new.data.dist.and.len.potential$data.potential,
                       dist.potential = new.data.dist.and.len.potential$dist.potential,
                       len = new.data.dist.and.len.potential$len))
}

## INPUT: c (x1, y1, x2, y2, tx1, ty1, tx2, ty2)
helper.interacting.lines <- function (row)
{
  points <- get.bounding.points (x1 = row[1],
                                 y1 = row[2],
                                 x2 = row[3],
                                 y2 = row[4],
                                 tx1 = row[5],
                                 ty1 = row[6],
                                 tx2 = row[7],
                                 ty2 = row[8])
  if (length (points) == 0)
    { 0 }
  else { sqrt ((points[4] - points[2])^2 + (points[3] + points[1])^2) }
}

## INPUT: end.points.anchor is the new line.
## INPUT: end.points.mat.rest are the rest of the lines.
## RETURN: a matrix with rows: c (x1, y1, x2, y2, tx1, ty1, tx2, ty2, length)
get.interacting.lines <- function (end.points.anchor,
                                   end.points.mat.rest)
{
  stopifnot (is.vector (end.points.anchor),
             is.matrix (end.points.mat.rest))
  if (nrow (end.points.mat.rest) > 0)
    {
      intersection.lengths <-
        {
          apply (cbind (end.points.mat.rest,
                        end.points.anchor [1],
                        end.points.anchor [2],
                        end.points.anchor [3],
                        end.points.anchor [4]),
                 MARGIN = 1,
                 FUN = helper.interacting.lines)
        }
      intersection.mask <- intersection.lengths != 0
      if (!any(intersection.mask))
        {
          return (new.interacting.lines.mat ())
        }
      else
        {
          return (cbind (end.points.mat.rest [intersection.mask, ,drop=F],
                         end.points.anchor [1],
                         end.points.anchor [2],
                         end.points.anchor [3],
                         end.points.anchor [4],
                         intersection.lengths [intersection.mask]))
        }
    }
  else
    {
      return (new.interacting.lines.mat ())
    }
}

get.interacting.lines.filter <- function (x1,
                                          y1,
                                          x2,
                                          y2,
                                          interacting.lines.mat)
{
  ## Find end points to remove. x1,y1,x2,y2
  tester <- c(x1, y1, x2, y2)
  results <- apply (interacting.lines.mat,
                    MARGIN = 1,
                    FUN = function (row)
                    {
                      if (all (row[1:4] == tester))
                        { return (F) }
                      else if (all (row[5:8] == tester))
                        { return (F) }
                      else { return (T) }
                    })
  stopifnot (is.vector (results),
             length (results) == nrow (interacting.lines.mat))
  return (results)
}

new.interacting.lines.mat <- function ()
{
  matrix (vector(), ncol = 9)
}

## RETURN: Updated interacting.lines.mat (with things removed and added).
add.to.interacting.lines <- function (interacting.lines.mat,
                                      updated.ls.path,
                                      x,
                                      y,
                                      position)
{
  stopifnot (is.matrix (interacting.lines.mat),
             is.matrix (updated.ls.path))
  if (nrow (updated.ls.path) < 2)
    {
      return (interacting.lines.mat)
    }
  ## Prepare a matrix representing line segment end points.
  end.points.mat <- cbind (updated.ls.path [ -nrow (updated.ls.path), "x"],
                           updated.ls.path [ -nrow (updated.ls.path), "y"],
                           updated.ls.path [ -1, "x"],
                           updated.ls.path [ -1, "y"])
  ## Find the index into end.points.mat that correspond to the new path segments.
                                        #before.index <- if (position > 0)
  { position }
                                        #after.index  <- if ((position+2) <= nrow (updated.ls.path))  { position + 2 }
  
  interacting.lines.filter <-
    if (all (position > 0,
             position + 2 <= nrow (updated.ls.path)))
      {
        get.interacting.lines.filter (updated.ls.path [position, "x"],
                                      updated.ls.path [position, "y"],
                                      updated.ls.path [position + 2, "x"],
                                      updated.ls.path [position + 2, "y"],
                                      interacting.lines.mat = interacting.lines.mat)
      }
    else
      {
        rep (T, nrow (interacting.lines.mat))
      }
  interacting.lines.1 <-
    if (position > 0)
      {
        get.interacting.lines (end.points.anchor = end.points.mat [position, , drop = T],
                               end.points.mat.rest = end.points.mat [-1 * c (position, position + 1), , drop = F])
      }
  interacting.lines.2 <-
    if (position + 1 <= nrow (end.points.mat))
      {
        get.interacting.lines (end.points.anchor = end.points.mat [position + 1, , drop = T],
                               end.points.mat.rest = end.points.mat [-1 * (position + 1), , drop = F ])
      }
  return (rbind (interacting.lines.mat [interacting.lines.filter, ], interacting.lines.1, interacting.lines.2))
}

translate.interacting.lines <- function (interacting.lines.mat,
                                         updated.ls.path,
                                         x, ## old.x
                                         y, ## old.y
                                         position)
{
  stopifnot (is.matrix (interacting.lines.mat),
             is.matrix (updated.ls.path),
             position > 1, ## You can't translate pos. 1.
             position <= nrow (updated.ls.path))
  ## Prepare a matrix representing line segment end points.
  end.points.mat <- cbind (updated.ls.path [ -nrow (updated.ls.path), "x"],
                           updated.ls.path [ -nrow (updated.ls.path), "y"],
                           updated.ls.path [ -1, "x"],
                           updated.ls.path [ -1, "y"])
                                        #before.index <- position - 1
                                        #after.index <- if (position + 1 <= nrow (updated.ls.path))
  { position + 1}
  interacting.lines.filter.1 <- get.interacting.lines.filter (updated.ls.path [position - 1, "x"],
                                                              updated.ls.path [position - 1, "y"],
                                                              x,
                                                              y,
                                                              interacting.lines.mat = interacting.lines.mat)
  
  interacting.lines.filter.2 <-
    if (position + 1 <= nrow (updated.ls.path))
      {
        get.interacting.lines.filter (x,
                                      y,
                                      updated.ls.path [position + 1, "x"],
                                      updated.ls.path [position + 1, "y"],
                                      interacting.lines.mat = interacting.lines.mat)
      }
  interacting.lines.filter <-
    if (!is.null (interacting.lines.filter.2))
      { interacting.lines.filter.1 & interacting.lines.filter.2 }
    else { interacting.lines.filter.1 }

  interacting.lines.1 <-
    get.interacting.lines (end.points.anchor = end.points.mat [position - 1, , drop = T],
                           end.points.mat.rest = end.points.mat [-1 * c (position - 1, position), , drop = F])
  interacting.lines.2 <-
    if (position <= nrow (end.points.mat))
      {
        get.interacting.lines (end.points.anchor = end.points.mat [position, , drop = T],
                               end.points.mat.rest = end.points.mat [-1 * (position), ,drop = F])
      }
  return (rbind (interacting.lines.mat [interacting.lines.filter, ], interacting.lines.1, interacting.lines.2))
}

## RETURN: Updated interacting.lines.mat (with things removed and added)
remove.from.interacting.lines <- function (interacting.lines.mat,
                                           updated.ls.path,
                                           x,
                                           y,
                                           position)
{
  stopifnot (is.matrix (interacting.lines.mat),
             is.matrix (updated.ls.path),
             position > 1, ## You can't remove position one.
             position <= nrow (updated.ls.path) + 1)
  if (nrow (updated.ls.path) < 2)
    {
      return (new.interacting.lines.mat ())
    }
  ## Prepare a matrix presenting line segment end points.
  end.points.mat <- cbind (updated.ls.path [ -nrow (updated.ls.path), "x"],
                           updated.ls.path [ -nrow (updated.ls.path), "y"],
                           updated.ls.path [ -1, "x"],
                           updated.ls.path [ -1, "y"])
  ## Find the index into end.points.mat that corresponds to the new path segment.
  ## before.index <- position - 1
  ## after.index <- if (position <= nrow (updated.ls.path))
  { position }
  interacting.lines.filter.1 <- get.interacting.lines.filter (updated.ls.path [position-1, "x"],
                                                              updated.ls.path [position-1, "y"],
                                                              x,
                                                              y,
                                                              interacting.lines.mat = interacting.lines.mat)
  interacting.lines.filter.2 <-
    if (position <= nrow (updated.ls.path))
      {
        get.interacting.lines.filter (x,
                                      y,
                                      updated.ls.path [position, "x"],
                                      updated.ls.path [position, "y"],
                                      interacting.lines.mat = interacting.lines.mat)
      }
  interacting.lines.filter <-
    if (!is.null (interacting.lines.filter.2))
      { interacting.lines.filter.1 & interacting.lines.filter.2 }
    else { interacting.lines.filter.1 }
  
  interacting.lines.add <-
    if (position - 1 <= nrow (end.points.mat))
      {
        get.interacting.lines (end.points.anchor = end.points.mat [position - 1, , drop = T],
                               end.points.mat.rest = end.points.mat [-1 * (position - 1), , drop = F])
      }
  return (rbind (interacting.lines.mat [interacting.lines.filter, ], interacting.lines.add))
}

## ls.path is a matric with the columns:
## c("x", "y", "angle", "angle.cos", "data.potential"))
get.potential.ls.path <-  function (ls.path,
                                    interacting.lines.mat,
                                    ls.path.point.cost,
                                    ls.path.angle.scale,
                                    ls.path.data.scale,
                                    ls.path.interaction.scale,
                                    ls.path.dist.scale,
                                    ls.path.len.scale)
{
  pot.angle <- sum (abs (ls.path[, "angle.cos"]), na.rm = T)
  pot.data <- sum (ls.path[, "data.potential"], na.rm = T)
  pot.points <- nrow (ls.path)
  pot.interaction <- sum (interacting.lines.mat[, 9])
  pot.dist <- sum (ls.path[, "dist.potential"], na.rm = T)
  pot.len <- sum (ls.path[, "len"], na.rm = T)
  ## cat (paste ('\npot.angle, scaled:', pot.angle, ls.path.angle.scale * pot.angle,'\n'))
  ## cat (paste ('pot.data, scaled:', pot.data, ls.path.data.scale * pot.data,'\n'))
  ## cat (paste ('pot.dist, scaled:', pot.dist, ls.path.dist.scale * pot.dist,'\n'))
  ## cat (paste ('pot.points, scaled:', pot.points, ls.path.point.cost * pot.points,'\n'))
  ## cat (paste ('pot.interaction, scaled:', pot.interaction, ls.path.interaction.scale * pot.interaction,'\n'))
  ## cat (paste ('pot.len, scaled:', pot.len, ls.path.len.scale * pot.len, '\n'))
  return (ls.path.angle.scale * pot.angle
          + ls.path.data.scale * pot.data
          + ls.path.point.cost * pot.points
          + ls.path.interaction.scale * pot.interaction
          + ls.path.dist.scale * pot.dist
          + ls.path.len.scale * pot.len)
}

## Given line observations, run a MC chain to get various states (no annealing) of the field initialized with
## some start state.
## Given the field configurations (and their potentials, to be used later) and
## Given a current proposed line sample, select a new proposed line sample.
## Then calculate the Green ratio (is it just the ratio of potentials?) and accept or reject.
## OUTPUT: the updated proposed line observation AND potential.
##    list (line.segment, line.segment.potential)
line.turn.crank <- function (########### turn.crank.n options...
                             pot.field.mat,
                             current.ls.path, ## The current path to sample.
                             current.ls.path.potential,
                             current.interacting.lines.mat,
                             xlim,
                             ylim,
                             ls.path.angle.scale,
                             ls.path.point.cost,
                             ls.path.lambda,
                             ls.path.annealing.scale,
                             ls.path.data.scale,
                             ls.path.interaction.scale,
                             dist.density,
                             ls.path.dist.scale,
                             ls.path.len.scale,
                             prc.mat) 
{

  ## The helper function produces the next step of the path selection
  ## MC (including calculating the Green ratio).
  ## INPUT: The chosen kernel and corresponding Green Function.
  ## OUPUT: The next step X_(t+1) = (z' or z).
  helper <- function (line.kernel.fun, is.line.kernel.ok, line.green.fun, msg)
    {
      if (is.line.kernel.ok (current.ls.path))
        {
          ## cat (paste ("\nLine Proposed: ", msg, " "))
          
          ## Proposes a new line segment path.
          line.kernel.result <- line.kernel.fun (ls.path = current.ls.path,
                                                 pot.field.mat = pot.field.mat,
                                                 interacting.lines.mat = current.interacting.lines.mat,
                                                 xlim = xlim,
                                                 ylim = ylim,
                                                 ls.path.angle.scale = ls.path.angle.scale,
                                                 ls.path.point.cost = ls.path.point.cost,
                                                 dist.density = dist.density,
                                                 prc.mat = prc.mat)
          proposed.ls.path <- line.kernel.result $ ls.path
          proposed.interacting.lines.mat <- line.kernel.result $ interacting.lines.mat
          
          if (is.null(proposed.ls.path))
            {
              return (list (ls.path = current.ls.path,
                            ls.path.potential = current.ls.path.potential,
                            interacting.lines.mat = current.interacting.lines.mat))
            }
          else
            {
              proposed.ls.path.potential <- get.potential.ls.path (ls.path = proposed.ls.path,
                                                                   interacting.lines.mat = proposed.interacting.lines.mat,
                                                                   ls.path.point.cost = ls.path.point.cost,
                                                                   ls.path.angle.scale = ls.path.angle.scale,
                                                                   ls.path.data.scale = ls.path.data.scale,
                                                                   ls.path.interaction.scale = ls.path.interaction.scale,
                                                                   ls.path.dist.scale = ls.path.dist.scale,
                                                                   ls.path.len.scale = ls.path.len.scale)
              line.green.ratio <- line.green.fun (current.ls.path = current.ls.path,
                                                  current.ls.path.potential.annealed = ls.path.annealing.scale * current.ls.path.potential,
                                                  proposed.ls.path = proposed.ls.path,
                                                  proposed.ls.path.potential.annealed = ls.path.annealing.scale * proposed.ls.path.potential,
                                                  xlim = xlim,
                                                  ylim = ylim,
                                                  ls.path.lambda = ls.path.lambda)
              ## cat (paste (" cur.scale", ls.path.annealing.scale,
              ##            " cur.pot", current.ls.path.potential,
              ##            " prop.pot", proposed.ls.path.potential,
              ##            "\n"))
              ## cat (paste (" Line Green Ratio: ", line.green.ratio))
              if (runif (1) < line.green.ratio)
                {
                  ## Select the proposed state.
                  ## cat (paste ("..........SELECTED "))
                  list (ls.path = proposed.ls.path,
                        ls.path.potential = proposed.ls.path.potential,
                        interacting.lines.mat = proposed.interacting.lines.mat)
                }
              else
                {
                  ## Select the current state.
                  list (ls.path = current.ls.path,
                        ls.path.potential = current.ls.path.potential,
                        interacting.lines.mat = current.interacting.lines.mat)
                }
            }
        }
      else
        {
          list (ls.path = current.ls.path,
                ls.path.potential = current.ls.path.potential,
                interacting.lines.mat = current.interacting.lines.mat)
        }
    }
  
  ## Next, choose a perturbation kernel Q_m(.,.) with probability p_m(z)
  ## Actually, in this case, we have only one perturbation kernel.
  result <- switch (sample (3, 1),
                    ## Birth,
                    helper (line.kernel.fun = line.kernel.birth,
                            is.line.kernel.ok = is.line.kernel.birth.possible,
                            line.green.fun = get.line.green.ratio.birth,
                            msg = " Birth"),
                    ## Death
                    helper (line.kernel.fun = line.kernel.death,
                            is.line.kernel.ok = is.line.kernel.death.possible,
                            line.green.fun = get.line.green.ratio.death,
                            msg = " Death"),
                    ## Translate
                    helper (line.kernel.fun = line.kernel.translate,
                            is.line.kernel.ok = is.line.kernel.translate.possible,
                            line.green.fun = get.line.green.ratio.translate,
                            msg = "Translate")
                    )
  return (result)
}

## Returns an updated ls.path
line.kernel.birth <- function (ls.path,
                               interacting.lines.mat,
                               pot.field.mat,
                               xlim,
                               ylim,
                               ls.path.angle.scale,
                               ls.path.point.cost,
                               dist.density,
                               prc.mat)
{
  stopifnot (is.matrix (ls.path),
             is.matrix (pot.field.mat))
  ## Randomly choose a position.
  ## Randomly choose a point.
  ## Insert point into path.

  new.position <- sample (nrow (ls.path), 1)
  new.x <- runif (1, min = min(xlim), max = max(xlim))
  new.y <- runif (1, min = min(ylim), max = max(ylim))
  updated.ls.path <- insert.ls.path (ls.path = ls.path,
                                     x = new.x,
                                     y = new.y,
                                     position = new.position,
                                     pot.field.mat = pot.field.mat,
                                     dist.density = dist.density,
                                     prc.mat = prc.mat)
  updated.interacting.lines.mat <- add.to.interacting.lines (interacting.lines.mat = interacting.lines.mat,
                                                             updated.ls.path = updated.ls.path,
                                                             x = new.x,
                                                             y = new.y,
                                                             position = new.position)
  return (list (ls.path = updated.ls.path,
                interacting.lines.mat = updated.interacting.lines.mat))
}

line.kernel.death <- function (ls.path,
                               interacting.lines.mat,
                               pot.field.mat,
                               xlim,
                               ylim,
                               ls.path.angle.scale,
                               ls.path.point.cost,
                               dist.density,
                               prc.mat)
{
  stopifnot (is.matrix (ls.path),
             is.matrix (pot.field.mat))
  rm.position <- sample (nrow (ls.path) - 2, 1) + 2
  updated.ls.path <- remove.ls.path (ls.path = ls.path,
                                     position = rm.position,
                                     pot.field.mat = pot.field.mat,
                                     dist.density = dist.density,
                                     prc.mat = prc.mat)
  updated.interacting.lines.mat <- remove.from.interacting.lines (interacting.lines.mat = interacting.lines.mat,
                                                                  updated.ls.path = updated.ls.path,
                                                                  x = ls.path [rm.position, "x"],
                                                                  y = ls.path [rm.position, "y"],
                                                                  position = rm.position)
  return (list (ls.path = updated.ls.path,
                interacting.lines.mat = updated.interacting.lines.mat))
  
}

line.kernel.translate <- function (ls.path,
                                   interacting.lines.mat,
                                   pot.field.mat,
                                   xlim,
                                   ylim,
                                   ls.path.angle.scale,
                                   ls.path.point.cost,
                                   dist.density,
                                   prc.mat)
{
  stopifnot (is.matrix (ls.path),
             is.matrix (pot.field.mat))
  translated.position <- sample (nrow (ls.path) - 1, 1) + 1
  xlim.delta <- diff (xlim) / 20
  ylim.delta <- diff (ylim) / 20
  xlim.new <- ls.path [translated.position, "x"] + c (-abs (xlim.delta), abs (xlim.delta))
  ylim.new <- ls.path [translated.position, "y"] + c (-abs (ylim.delta), abs (ylim.delta))
  x.new <- runif (1, min = xlim.new[1], max = xlim.new[2])
  y.new <- runif (1, min = ylim.new[1], max = ylim.new[2])

  if (!all (xlim [1] <= x.new,
            x.new <= xlim [2],
            ylim[1] <= y.new,
            y.new <= ylim [2]))
    {
      return (NULL)
    }
  else
    {
      updated.ls.path <- translate.ls.path (ls.path = ls.path,
                                            position = translated.position,
                                            x = x.new,
                                            y = y.new,
                                            pot.field.mat = pot.field.mat,
                                            dist.density = dist.density,
                                            prc.mat = prc.mat)
      updated.interacting.lines.mat <- translate.interacting.lines (interacting.lines.mat = interacting.lines.mat,
                                                                    updated.ls.path = updated.ls.path,
                                                                    x = ls.path [translated.position, "x"],
                                                                    y = ls.path [translated.position, "y"],
                                                                    position = translated.position
                                                                    )
      
      return (list (ls.path = updated.ls.path,
                    interacting.lines.mat = updated.interacting.lines.mat))
    }
}

## INPUT: x,y are the starting coordinates.
## INPUT, xlim, ylim are vectors containing min and max values for valid x and y values.
## OUTPUT: A Line Segment data structure with a start point of x,y and an end point on the border.
line.kernel.tumble.new <- function (x, y, xlim, ylim)
{
  stopifnot (is.vector (xlim), nrow (xlim) == 2, is.numeric (xlim), xlim[1] <= xlim[2],
             is.vector (ylim), nrow (ylim) == 2, is.numeric (ylim), ylim[1] <= ylim[2])
  x.len <- xlim[2] - xlim[1]
  y.len <- ylim[2] - ylim[1]
  x.norm <- (x - xlim[1]) / x.len
  y.norm <- (y - ylim[1]) / y.len
  end.point <- get.random.point.from (x.norm, y.norm)
  x.new <- end.point$x * x.len + xlim[1]
  y.new <- end.point$y * y.len + ylim[1]
  line.segment <- new.line.segment (x1=x, y1=y, x2=x.new, y2=y.new)
  return (line.segment)
}

## is.kernel.tumble.new.possible (list.line.segment.observations, current.line.segment)
## RETURNS Logical
is.kernel.tumble.new.possible <- function (current.ls.path)
{
  TRUE
}

is.line.kernel.birth.possible <- function (current.ls.path)
{
  TRUE
}

is.line.kernel.death.possible <- function (current.ls.path)
{
  return (2 < nrow (current.ls.path))
}

is.line.kernel.translate.possible <- function (current.ls.path)
{
  return ( 1 < nrow (current.ls.path))
}

get.line.green.ratio.birth <- function (current.ls.path,
                                        current.ls.path.potential.annealed,
                                        proposed.ls.path,
                                        proposed.ls.path.potential.annealed,
                                        xlim,
                                        ylim,
                                        ls.path.lambda)
{
  area.window <- abs(diff(xlim)) * abs(diff(ylim))
  result <- (area.window * ls.path.lambda) / nrow (proposed.ls.path) * exp (current.ls.path.potential.annealed - proposed.ls.path.potential.annealed)
  stopifnot (!is.na (result),
             length (result) == 1)
  return (result)
}

get.line.green.ratio.death <- function (current.ls.path,
                                        current.ls.path.potential.annealed,
                                        proposed.ls.path,
                                        proposed.ls.path.potential.annealed,
                                        xlim,
                                        ylim,
                                        ls.path.lambda)
{
  area.window <- abs(diff(xlim)) * abs(diff(ylim))
  result <- nrow (current.ls.path) / (area.window * ls.path.lambda) * exp (current.ls.path.potential.annealed - proposed.ls.path.potential.annealed)
  stopifnot (!is.na (result))
  return (result)
}

get.line.green.ratio.translate <- function (current.ls.path,
                                            current.ls.path.potential.annealed,
                                            proposed.ls.path,
                                            proposed.ls.path.potential.annealed,
                                            xlim,
                                            ylim,
                                            ls.path.lambda)
{
  result <- exp (current.ls.path.potential.annealed - proposed.ls.path.potential.annealed)
  stopifnot (!is.na (result))
  return (result)
}

## RETURNS float (the green ratio)
get.line.green.ratio.tumble.new <- function (current.line.segment,
                                             current.potential,
                                             proposed.line.segment,
                                             proposed.potential,
                                             xlim,
                                             ylim)
{
  ## Normally I would have to calculate the valid lengths on the top
  ## and bottom, but they cancel.
  ratio <- exp (-1 * proposed.potential + current.potential)
  return (ratio)
}

line.turn.crank.n <- function (list.line.segment.observations,
                               line.n,
                               pot.field.mat,
                               xlim,
                               ylim,
                               start.x,
                               start.y,
                               ls.path.angle.scale,
                               ls.path.point.cost,
                               ls.path.lambda,
                               ls.path.temp.start,
                               ls.path.annealing.alpha,
                               ls.path.data.scale,
                               ls.path.interaction.scale,
                               draw.line.pots = NULL,
                               dist.density,
                               ls.path.dist.scale,
                               ls.path.len.scale,
                               prc.mat) 
{
###### Boot strap new line segment:
  ## Generate the first candidate line segment:
  current.ls.path <- new.ls.path (x = start.x, y = start.y) 
  current.ls.path.potential <- 0 # This seems like a good property to have.
  current.interacting.lines.mat <- new.interacting.lines.mat ()
  line.chain.list <- list ()
  line.chain.list [[1]] <- list (ls.path = current.ls.path,
                                 ls.path.potential = current.ls.path.potential)

  ## Show the current observations
  if (draw)
    {
      ls.path.gtree <- display.grob.gtree (list.line.segment.observations, name = "segments") 
      new.ls.path.grob <- NULL
    }
  ## Do the loop
  cat (paste ('\nline.turn.crank.n\n'))
  pb <- txtProgressBar (min = 1, max = line.n, style = 3)
  for (i in (1:line.n))
    {
      setTxtProgressBar (pb, i)
      ## cat (paste ("\nLINE STEP: ", i, ' '))
      ls.path.annealing.scale <- ls.path.annealing.alpha^(-i) * ls.path.temp.start
      next.ls.path.and.potential <- line.turn.crank (pot.field.mat = pot.field.mat,
                                                     current.ls.path = current.ls.path,
                                                     current.ls.path.potential = current.ls.path.potential,
                                                     current.interacting.lines.mat = current.interacting.lines.mat,
                                                     xlim = xlim,
                                                     ylim = ylim,
                                                     ls.path.angle.scale = ls.path.angle.scale,
                                                     ls.path.point.cost = ls.path.point.cost,
                                                     ls.path.lambda = ls.path.lambda,
                                                     ls.path.annealing.scale = ls.path.annealing.scale,
                                                     ls.path.data.scale = ls.path.data.scale,
                                                     ls.path.interaction.scale = ls.path.interaction.scale,
                                                     dist.density = dist.density,
                                                     ls.path.dist.scale = ls.path.dist.scale,
                                                     ls.path.len.scale = ls.path.len.scale,
                                                     prc.mat = prc.mat)
      line.chain.list [[i+1]] <- next.ls.path.and.potential          
      current.ls.path <- next.ls.path.and.potential$ls.path
      current.ls.path.potential <- next.ls.path.and.potential$ls.path.potential
      current.interacting.lines.mat <- next.ls.path.and.potential$interacting.lines.mat
      ## TODO: Display the selected line against the mean field image (function of chain.list)
      ## if (0 == i %% 1)
      
      if (draw && (0 == i %% 100))
        {
          if (!is.null (draw.line.pots))
            { draw.line.pots (line.chain.list) }
          if (!is.null (new.ls.path.grob))
            {
              grid.remove (gPath ("segments", "newseg"))
            }
          
          new.ls.path.obs <- get.list.line.segments.from.ls.path (current.ls.path)
          new.ls.path.grob <- get.grob.segments.single.grob (new.ls.path.obs,
                                                             name = "newseg",
                                                             color = "green")
          grid.add (gPath ("segments"),
                    applyEdit(new.ls.path.grob,
                              gEdit (gp=gpar (col="green", lwd=global.lwd))))
        }
    }
  close (pb)
  return (line.chain.list)
}

#' Get a list of line segment observations from a matrix.
#'
#' Given a list of line segments and a binary image matrix,
#' create a new list of line segment observations with the matrix
#' projected onto them.
#'
#' @param list.line.segment.observations a list of line segments
#' @param bin.image.mat an image matrix
#' @return a list of line segment objects, but with updated union.intersection.df.
get.line.segment.observations.from.mat <- function (list.line.segment.observations, bin.image.mat)
  {
      stopifnot (is.list (list.line.segment.observations),
                 is.matrix (bin.image.mat))
      proposed.line.segments <-
        if (length (list.line.segment.observations) == 0)
          {
            list()
          }
        else
          {
            ## Turn jpegimage into a matrix.
            ## Flip and transpose the matrix.
            len <- nrow (list.line.segment.observations)
            lapply (list.line.segment.observations,
                    function (ls)
                    {
                      x1 <- ls$p1[1]
                      y1 <- ls$p1[2]
                      x2 <- ls$p2[1]
                      y2 <- ls$p2[2]
                      
                      ## Convert end point of line segment to matrix coordinates.
                      point.start <- pmin (pmax (ceiling (c (x1, y1) * c (ncol (bin.image.mat), nrow (bin.image.mat))),
                                                 1),
                                           nrow (bin.image.mat))
                      point.end <- pmin (pmax (ceiling (c (x2, y2) * c (ncol (bin.image.mat), nrow (bin.image.mat))),
                                               1),
                                         nrow (bin.image.mat))
                      ## Get the intermediate points
                      int.points <- intermediate.points (x1 = point.start[1],
                                                         y1 = point.start[2],
                                                         x2 = point.end[1],
                                                         y2 = point.end[2])
                      ## Convert intermediate coordinates to vector
                      ## coordinates for a matrix so that I can use a
                      ## single index.
                      vect.coordinates <- (int.points[,1]-1) * ncol (bin.image.mat) + int.points[,2]
                      ## Sample the intermediate vector coordinates.
                      samples.vect <-bin.image.mat [vect.coordinates]
                      ## Find the change points.
                      diff.samples.vect <- diff (samples.vect)
                      stopifnot (all (unique (diff.samples.vect) %in% c (-1, 0, 1)))
                      ## Prepend and append points to cover cases when the
                      ## end points are inside a bomb.
                      change.points.vect <- c (samples.vect[1],
                                               diff.samples.vect,
                                               -1 * samples.vect[length (samples.vect)])
                      stopifnot (sum (change.points.vect) == 0)
                      change.points.mask <- (change.points.vect == 1) | (change.points.vect == -1)
                      ## Get locations of the change points.
                      index.change.points <- (1:length(change.points.vect)) [change.points.mask]
                      ## Assert that index.change.points is an even number
                      ## (even 0)
                      stopifnot ((length (index.change.points) %% 2) == 0) 
                      if (length (index.change.points) == 0)
                        {
                          return (ls)
                        }
                      else
                        {
                          ## Normalize the index (there's a rounding
                          ## error... can't get to zero)
                          scaled.change.points <- index.change.points / length (change.points.vect) * ls$length
                          ## The change points alternate between, open,
                          ## close, open, close...
                          ls$union.intersections.df <- new.union.intersections (left = scaled.change.points[seq(1, length (index.change.points), 2)],
                                                                                right = scaled.change.points[seq(2, length (index.change.points), 2)])
                          return (ls)
                        }
                    })
          }
      return (proposed.line.segments)
    }

get.list.line.segments.from.ls.path <- function (ls.path)
  {
      stopifnot (is.matrix (ls.path))
      list.line.segments <-
        if (nrow (ls.path) < 2)
          {
            list()
          }
        else
          {
            len <- nrow (ls.path)
            mapply (function (x1, y1, x2, y2)
                    {
                      new.ls <- new.line.segment (x1 = x1, y1 = y1, x2 = x2, y2 = y2)
                    },
                    ls.path [-len, "x"],
                    ls.path [-len, "y"],
                    ls.path [-1, "x"],
                    ls.path [-1, "y"],
                    SIMPLIFY = F)
          }
      return (list.line.segments)
    }

## return (list (list.line.segment.observations, chain.list, pot.field.mat))
adaptive.sample.n <- function (moves.n,
                               actual.bombing.mat,
                               n,
                               point.cost,
                               mark.cost.scale,
                               excl.scale,
                               disagree.scale,
                               xlim,
                               ylim,
                               shape,
                               scale,
                               lambda,
                               temp.start,
                               annealing.alpha,
                               start.x,
                               start.y,
                               dnorm.sd,
                               line.n,
                               mean.field.width,
                               mean.field.height,
                               ls.path.angle.scale,
                               ls.path.point.cost,
                               ls.path.lambda,
                               ls.path.temp.start,
                               ls.path.annealing.alpha,
                               ls.path.data.scale,
                               ls.path.interaction.scale,
                               ls.path.dist.scale,
                               ls.path.len.scale,
                               sample.perimeter,
                               distance.field.width,
                               distance.field.height,
                               inherit.next.list,
                               use.bombing,
                               use.prc,
                               use.anchors
                               )
  {
      stopifnot (moves.n >= 1)

      list.circle.matrices <-
        if (use.prc)
          {
            cat ("Creating list circle matrices...\n")
            create.list.circle.matrices (mean.field.width)
          }
        else
          {
            NULL
          }
      
      ## Start with a sampled perimeter or with no observations.
      loop.points <- looplines (0, 0, 1, 1)
      list.line.segment.observations <-
        if (sample.perimeter)
          {
            ls.list <- get.list.line.segments.from.ls.path (as.matrix (loop.points))
            get.line.segment.observations.from.mat (ls.list,
                                                    actual.bombing.mat)
          }
        else
          {
            list()
          }
      list.line.segment.observations.list <- list (list.line.segment.observations)
      
      rdist.precomp <- create.precomp (max.col = distance.field.width,
                                       max.row = distance.field.height)
      dist.density.raw <- looplines.dist.density (rdist.precomp, 0, 0, 1, 1)
      dist.density.raw.list <- list (dist.density.raw)
      next.list <- NULL

      
      results <-
        {
          ls.path.loop <- new.ls.path (x = loop.points$x,
                                       y = loop.points$y)
          list (list.line.segment.observations.list =  list (list.line.segment.observations),
                dist.density.raw.list = list (dist.density.raw),
                ls.path.list          = list (ls.path.loop),
                mean.mat.list         = list (),
                chain.list.list       = list (),
                pot.field.mat.list    = list ())
        }

      for (i in (1:moves.n))
        {
          cat (paste ('\n**********adaptive.sample.n ', i, '/', moves.n, '\n'))
          ## Generate field estimates.
          if (draw)
            {
              if (i == 1)
                {
                  disp (title = "GROUND TRUTH")
                  plot (levelplot (t(actual.bombing.mat), col=grey(0:1)))

                  if (use.bombing)
                    {
                      disp (title = "turn.crank.n")
                      dev.crank <- dev.cur()
                    }

                  if (use.prc)
                    {
                      disp (title = "prc.sdev")
                      dev.prc.sdev <- dev.cur ()

                      disp (title = "prc.rot")
                      dev.prc.rot <- dev.cur ()

                      disp (title = "prc.rel.center")
                      dev.prc.rel.center <- dev.cur()
                    }
                  
                  disp (title = paste("pot field", i))
                  dev.potfield <- dev.cur()
                  
                  disp (title = paste("mean.field", i))
                  dev.meanfield <- dev.cur()

                  disp (title = "line.turn.crank.n")
                  dev.line.crank <- dev.cur()

                  disp (title = paste("Line Segment potentials", i))
                  dev.linepots <- dev.cur()

                  disp (title = "Min Distance (in pixels) Field")
                  dev.dist.density.raw <- dev.cur()

                  disp (title = "MSE vs. Distance")
                  dev.mse <- dev.cur()
                }
              if (use.bombing)
                { dev.set (dev.crank) }
              
            }

          mean.mat <-
            if (use.bombing)
              {
                prev.list <-
                  if (use.anchors)
                    {
                      global.skip.interacting <<- T ## Should be T if using anchored model.
                      anchors.mat <- get.anchors (list.line.segment.observations)
                      anchored.bombs.df <- new.anchored.bombs (anchors.mat)
                      list (bombing.df = anchored.bombs.df,
                            potential = 0,
                            interacting.bombs.mat = new.interacting.bombs.mat ())
                    }
                  else if (inherit.next.list)
                    {
                      next.list
                    }
                  else
                    {
                      NULL
                    }
                
                chain.list <- turn.crank.n (n = n,
                                            list.line.segment.observations = list.line.segment.observations,
                                            point.cost = point.cost,
                                            mark.cost.scale = mark.cost.scale,
                                            excl.scale = excl.scale,
                                            disagree.scale = disagree.scale,
                                            xlim = xlim,
                                            ylim = ylim,
                                            shape = shape,
                                            scale = scale,
                                            lambda = lambda,
                                            temp.start = temp.start,
                                            annealing.alpha = annealing.alpha,
                                            prev.list = prev.list,
                                            use.anchors = use.anchors)
                next.list <- if (inherit.next.list) { chain.list [[length (chain.list)]] }
                stopifnot (length (chain.list) > 0)
                ## Get the mean image and then potential field
                get.mean.matrix.from.chain.list (chain.list = chain.list,
                                                 width = mean.field.width,
                                                 height = mean.field.height,
                                                 ##start.at = ceiling (length (chain.list) / 2))
                                                 start.at = 1)
              }
            else
              {
                ls.path <- get.ls.paths (results $ ls.path.list)
                t (get.image.hat.from.ls.path (ls.path = ls.path,
                                               true.image = actual.bombing.mat,
                                               width = mean.field.width,
                                               height = mean.field.height))
              }
          rm (prc.mat); gc()
          prc.mat <-
            if (use.prc)
              {
                stopifnot (use.bombing == T,
                           mean.field.width == mean.field.height)
                list.giant.mat <- create.x.y.cover.matrices.from.chain.list (chain.list = chain.list,
                                                                             image.size = mean.field.width,
                                                                             list.circle.matrices = list.circle.matrices,
                                                                             ##start.at = ceiling (length (chain.list) / 2))
                                                                             start.at = 1)
                create.prcomp.vect.mat (list.giant.mat $ giant.mat.x,
                                        list.giant.mat $ giant.mat.y)
              }
          rm (list.giant.mat); gc()
          
          pot.field.mat <-
            {
              ## mean.thresh <- otsu (mean.mat) $ threshold
              mean.thresh <- 0.5
              pot.field.mat.raw <- dnorm ((mean.mat - mean.thresh),
                                          sd = dnorm.sd)
              ## Make some of the values neg (this gets flipped later.)
              ## pot.thresh <- otsu (pot.field.mat.raw) $ threshold
              pot.field.mat.centered <- (pot.field.mat.raw - mean (pot.field.mat.raw))
              if (use.prc)
                {
                  sdev.scale.mat <- t (matrix (prc.mat ['sdev', ],
                                               ncol = mean.field.width,
                                               nrow = mean.field.width)) / mean (prc.mat ['sdev', ], na.rm = T)
                  sdev.scale.mat [ is.na (sdev.scale.mat) ] <- 0
                  sdev.scale.mat * pot.field.mat.centered
                }
              else
                {
                  pot.field.mat.centered
                }
            }
          
          if (draw)
            {
              dev.set (dev.potfield)
              plot (levelplot( pot.field.mat))
                                        # zlim = c (dnorm (-0.5, sd = 0.447),
                                        # dnorm (0, sd = 0.447)) - dnorm (dnorm.sd, sd = dnorm.sd))
              dev.set (dev.meanfield)
              plot (levelplot (mean.mat)) ##,
                               ##zlim = c (0,1),
                               ##col = grey(seq(0,1,length.out=256))))
              dev.set (dev.dist.density.raw)
              plot (levelplot ((dist.density.raw - mean (dist.density.raw)) / max (dist.density.raw)))

              if (use.prc)
                {
                  dev.set (dev.prc.sdev)
                  plot (levelplot (sdev.scale.mat))

                  prc.df <- transform (get.prc.df (prc.mat),
                                       pot.field = as.vector (t (pot.field.mat)))
                  sdev.scale <- 0.01
                  dev.set (dev.prc.rot)
                  p <- ggplot (prc.df, aes (x = x, y = y))
                  p1 <- p + geom_segment (aes (xend = x + rot.x * pot.field * 10,
                                               yend = y + rot.y * pot.field * 10),
                                          arrow = arrow(length=unit(0.1,"cm")))
                  print (p1)
                  
                  dev.set (dev.prc.rel.center)
                  p2 <- p + geom_segment (aes (xend = x + unit.rel.center.x,
                                               yend = y + unit.rel.center.y))
                  print (p2)
                  ##plot (levelplot (t (matrix (prc.mat ['sdev', ],
                  ##                            ncol = mean.field.width,
                  ##                            nrow = mean.field.width))))
                }
              
              dev.set (dev.line.crank)
            }

          draw.line.pots <- function (res)
            {
              if (draw)
                {
                  dev.set(dev.linepots)
                  potentials <- lapply (res,
                                        function (elem)
                                        {elem$ls.path.potential})
                  plot (1:length (potentials),
                        potentials)
                  dev.set (dev.line.crank)
                }
            }
          
          ## Find the next path to sample.
          list.line.turn.crank.n.results <-
            line.turn.crank.n (list.line.segment.observations = list.line.segment.observations,
                               line.n = line.n,
                               pot.field.mat = t(pot.field.mat),
                               xlim = xlim,
                               ylim = ylim,
                               start.x = start.x,
                               start.y = start.y,
                               ls.path.angle.scale = ls.path.angle.scale,
                               ls.path.point.cost = ls.path.point.cost,
                               ls.path.lambda = ls.path.lambda,
                               ls.path.temp.start = ls.path.temp.start,
                               ls.path.annealing.alpha = ls.path.annealing.alpha,
                               ls.path.data.scale = ls.path.data.scale,
                               ls.path.interaction.scale = ls.path.interaction.scale,
                               draw.line.pots = draw.line.pots,
                               dist.density = t ((dist.density.raw - mean (dist.density.raw)) / max (dist.density.raw)),
                               ls.path.dist.scale = ls.path.dist.scale,
                               ls.path.len.scale = ls.path.len.scale,
                               prc.mat = prc.mat)
          
          ## Sample it.
          last.ls.path <- adp.list.last (list.line.turn.crank.n.results) $ ls.path
          ## Update starting point.
          start.x <- last.ls.path [nrow (last.ls.path), "x"]
          start.y <- last.ls.path [nrow (last.ls.path), "y"]
          ## For each row in the matrix, create a line segment object.
          list.line.segment.observations.new <-
            {
              temp.line.segs <- get.list.line.segments.from.ls.path (last.ls.path)
              get.line.segment.observations.from.mat (temp.line.segs,
                                                      actual.bombing.mat)
            }
          list.line.segment.observations <- c (list.line.segment.observations,
                                               list.line.segment.observations.new)
          dist.density.raw <- update.dist.density (dist.density = dist.density.raw,
                                                   ls.path = last.ls.path,
                                                   rdist.precomp = rdist.precomp)
          
### Build up results to return
          {
            results $ list.line.segment.observations.list [[ i + 1 ]] <- list.line.segment.observations.new
            results $ dist.density.raw.list  [[ i + 1 ]] <- dist.density.raw
            results $ ls.path.list           [[ i + 1 ]] <- last.ls.path
            results $ mean.mat.list          [[ i ]]     <- mean.mat
            results $ ls.path.potential.list [[ i ]]     <- adp.list.last(list.line.turn.crank.n.results) $ ls.path.potential
            results $ chain.list.list        [[ i ]]     <- if (use.bombing) { chain.list }
            results $ pot.field.mat.list     [[ i ]]     <- pot.field.mat
          }
          if (draw)
            {
              list.mses.lengths <- plot.mse.vs.length.of.bombing.from.test.adaptive (mean.mat.list = results $ mean.mat.list,
                                                                                     ls.path.list = results $ ls.path.list,
                                                                                     actual.bombing.mat = actual.bombing.mat,
                                                                                     show = F)
              dev.temp <- dev.cur()
              dev.set (dev.mse)
              q <- qplot (list.mses.lengths$lengths, list.mses.lengths$mses, geom = c('point', 'line'))
              print(q)
              dev.set (dev.temp)
            }
        } ## End giant for-loop.

      return (results)
    }

update.dist.density <- function (dist.density,
                                 ls.path,
                                 rdist.precomp)
  {
      stopifnot (nrow (ls.path) > 1)
      row <- nrow (rdist.precomp)
      col <- ncol (rdist.precomp)
      accum.dist.density <- dist.density
      ls.path.end.points <- cbind (ls.path [ -nrow (ls.path), "x"],
                                   ls.path [ -nrow (ls.path), "y"],
                                   ls.path [ -1, "x"],
                                   ls.path [ -1, "y"])
      x.max <- (ncol (rdist.precomp) + 1) / 2
      y.max <- (nrow (rdist.precomp) + 1) / 2
      for (i in 1:nrow (ls.path.end.points))
        {
          ## for each line segment
          temp.density <- get.sample.distance.mat.from.line (rdist.precomp = rdist.precomp,
                                                             x1 = get.coord (ls.path.end.points [i, 1], x.max),
                                                             y1 = get.coord (ls.path.end.points [i, 2], y.max),
                                                             x2 = get.coord (ls.path.end.points [i, 3], x.max),
                                                             y2 = get.coord (ls.path.end.points [i, 4], y.max),
                                                             x.min = 1,
                                                             y.min = 1,
                                                             x.max = x.max,
                                                             y.max = y.max)
          accum.dist.density <- pmin (accum.dist.density, temp.density)
        }
      return (accum.dist.density)
    }

get.mse.of.bombing.df <- function (bombing.df, trans.actual.bombing.mat)
  {
    ## Convert the bombing.df into a matrix.
    bombing.mat <- get.matrix.from.bombing (bombing.df = bombing.df,
                                            width = dim (trans.actual.bombing.mat) [2],
                                            height = dim (trans.actual.bombing.mat) [1])
      return (mean ((bombing.mat - trans.actual.bombing.mat)^2, na.rm = T))
  }

get.mse.vect.of.chain.list <- function (chain.list, actual.bombing.mat)
  {
      stopifnot (length (chain.list) > 0)
      lapply (chain.list,
              function (elem)
              {
                get.mse.of.bombing.df (elem$bombing.df, bombing.mat)
              })
    }

get.start.steps.vect.of.chain.list <- function (chain.list)
  {
      stopifnot (length (chain.list) > 0)
      lapply (chain.list,
              function (elem) elem$start.index)
    }

get.end.steps.vect.of.chain.list <- function (chain.list)
  {
      stopifnot (length (chain.list) > 0)
      lapply (chain.list,
              function (elem) elem$end.index)
    }

## actual.bombing.mat may need to be transposed first.
get.experiment.results.of.chain.list <- function (chain.list, actual.bombing.mat)
  {
      
    }

## Introduction:
  ##  This code is based on the solution of these two input equations:
  ##   Pa = P1 + ua (P2-P1)
  ##   Pb = P3 + ub (P4-P3)

  ##  Where line one is composed of points P1 and P2 and line two is composed
  ##   of points P3 and P4.

  ##  ua/b is the fractional value you can multiple the x and y legs of the
  ##   triangle formed by each line to find a point on the line.

  ##  The two equations can be expanded to their x/y components:
  ##   Pa.x = p1.x + ua(p2.x - p1.x) 
  ##   Pa.y = p1.y + ua(p2.y - p1.y) 

  ##   Pb.x = p3.x + ub(p4.x - p3.x)
  ##   Pb.y = p3.y + ub(p4.y - p3.y)

  ##  When Pa.x == Pb.x and Pa.y == Pb.y the lines intersect so you can come 
  ##   up with two equations (one for x and one for y):

  ##  p1.x + ua(p2.x - p1.x) = p3.x + ub(p4.x - p3.x)
  ##  p1.y + ua(p2.y - p1.y) = p3.y + ub(p4.y - p3.y)

  ##  ua and ub can then be individually solved for.  This results in the
  ##   equations used in the following code.     

  ## Returns NA if no intersection or parallel.
  ## Else returns c(x, y) of the intersection.
get.line.segment.intersection <- function (x1, y1, x2, y2,
                                             x3, y3, x4, y4)
  {
      d <- (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)
      if (d == 0)
        {  ## lines are parallel
          return (NULL)
        }
      n.a <- (x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)
      n.b <- (x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)
      u.a <- n.a / d
      u.b <- n.b / d
      if (any (u.a < 0,
               u.a > 1,
               u.b < 0,
               u.b > 1))
        { ## not long enough to intersect.
          return (NULL)
        }
      x <- x1 + u.a * (x2 - x1)
      y <- y1 + u.a * (y2 - y1)
      return (c (x, y))
    }

get.perimeter.points.from.ls <- function (x1, y1, x2, y2, offset=0.05)
  {
      r <- sqrt ((y2 - y1)^2 + (x2 - x1)^2)
      cosine.orthog <- -1 * (y2 - y1) / r
      sine.orthog <- (x2 - x1) / r
      xa <- x1 + cosine.orthog * offset
      ya <- y1 + sine.orthog * offset
      xb <- x1 - cosine.orthog * offset
      yb <- y1 - sine.orthog * offset
      xc <- x2 - cosine.orthog * offset
      yc <- y2 - sine.orthog * offset
      xd <- x2 + cosine.orthog * offset
      yd <- y2 + sine.orthog * offset
      return (matrix (c (xa, xb, xc, xd, ya, yb, yc, yd), ncol = 2))
    }

test.get.perimeter.points.from.ls <- function ()
  {
      points.mat <- get.perimeter.points.from.ls (0, 0, 1, 1, 0.05)
      return (points.mat)
    }

rotate.vect <- function (v)
  {
      if (length (v) == 0)
        {
          v
        }
      else
        {
          c (v[-1], v[1])
        }
    }

is.point.close.to.ls <- function (px, py, x1, y1, x2, y2, offset = 0.05)
  {
      result <- F
      p0 <- c (px, py)
      p1 <- c (x1, y1)
      p2 <- c (x2, y2)
      if (any (all (p0 == p1),
               all (p0 == p2)))
        {
          return (T)
        }
      else
        {
          distance.and.closest.point <- get.distance.from.infinite.line.and.closest.point (px = px,
                                                                                           py = py,
                                                                                           x1 = x1,
                                                                                           y1 = y1,
                                                                                           x2 = x2,
                                                                                           y2 = y2)
          cond1 <- is.contained.on.segment (px = distance.and.closest.point$closest.point[1],
                                            py = distance.and.closest.point$closest.point[2],
                                            x1 = x1, y1=y1, x2=x2, y2=y2)
          cond2 <- distance.and.closest.point$distance <= offset
          return (cond1 && cond2)
        }
    }

## RETURN: c (x1, y1, x2, y2)
  ## RETURN: c ()
get.bounding.points <- function (x1, y1, x2, y2,
                                   tx1, ty1, tx2, ty2)
  {
      perimeter.points.mat <- get.perimeter.points.from.ls (x1=x1,
                                                            y1=y1,
                                                            x2=x2,
                                                            y2=y2)
      intersection.coords <- unlist (mapply (function (helper.x1,
                                                       helper.y1,
                                                       helper.x2,
                                                       helper.y2)
                                             {## Function returns the x,y coordinates of intersection.
                                               get.line.segment.intersection (helper.x1,
                                                                              helper.y1,
                                                                              helper.x2,
                                                                              helper.y2,
                                                                              tx1,
                                                                              ty1,
                                                                              tx2,
                                                                              ty2)
                                             },
                                             perimeter.points.mat [,1],
                                             perimeter.points.mat [,2],
                                             rotate.vect (perimeter.points.mat [,1]),
                                             rotate.vect (perimeter.points.mat [,2])))
      ## intersection.coords may be length 0, 2, or 4.
      if (is.null (intersection.coords))
        {
          intersection.coords <- c()
        }
      stopifnot (length (intersection.coords) %in% c (0, 2, 4))
      if (length (intersection.coords) == 4)
        {
          return (intersection.coords)
        }
      else
        {
          if (is.point.close.to.ls (px = tx1,
                                    py = ty1,
                                    x1 = x1,
                                    y1 = y1,
                                    x2 = x2,
                                    y2 = y2)
              && ((length (intersection.coords) == 0)
                  || any (intersection.coords != c (tx1, ty1))))
            {
              intersection.coords <- c (intersection.coords, tx1, ty1)
              if (length (intersection.coords) == 4)
                {
                  return (intersection.coords)
                }
            }
          if (length (intersection.coords) != 2)
            {
              return (vector())
            }
          else if (is.point.close.to.ls (px = tx2,
                                         py = ty2,
                                         x1 = x1,
                                         y1 = y1,
                                         x2 = x2,
                                         y2 = y2)
                   && any (intersection.coords != c (tx2, ty2)))
            {
              intersection.coords <- c (intersection.coords, tx2, ty2)
              return (intersection.coords)
            }
          else
            {
              return (vector())
            }
        }
    }

test.get.bounding.points <- function ()
  {
      anchor.ls <- new.line.segment (0, 0, 1, 1)
      target.ls <- new.line.segment (0.45, 0.5, 1, 0.5)
      bounding.points <- get.bounding.points (anchor.ls = anchor.ls,
                                              target.ls = target.ls)
      return (bounding.points)
    }

## Create a min.map (all Inf).
  ## Update min.map with selected ls.path. -> this is just like the data potential.
  ## During line.crank, keep track of min.dist line segment potential just like data potential.

  ## density.new = get.sample.distance.mat.from.line (rdist.precomp, pos.x, pos.y, x, y)
  ## density.cur = pmin (density.cur, density.new)
get.coord <- function (real, coord.max)
  {
      floor (real * (coord.max - 1) + 1)
    }

test.dist.pot <- function(x1.real, y1.real, x2.real, y2.real)
  {
      x.dim <- 100
      y.dim <- 100
      ## x1.real <- 0
      ##   y1.real <- 0
      ##   x2.real <- 0.1
      ##   y2.real <- 1
      x1.coord <- get.coord (x1.real, x.dim)
      y1.coord <- get.coord (y1.real, y.dim)
      x2.coord <- get.coord (x2.real, x.dim)
      y2.coord <- get.coord (y2.real, y.dim)
      
      rdist.precomp <- create.precomp (max.col = y.dim, max.row = x.dim)
      cat ("done.\n")
      density.new <- get.sample.distance.mat.from.line (rdist.precomp,
                                                        x1.coord,
                                                        y1.coord,
                                                        x2.coord,
                                                        y2.coord,
                                                        x.min = 1,
                                                        y.min = 1,
                                                        x.max = (ncol (rdist.precomp) + 1) / 2,
                                                        y.max = (nrow (rdist.precomp) + 1) / 2)
      return (density.new)
    }

## Create an rdist.precomp.
create.precomp <- function(max.row,
                             max.col)
  {
      cat("create.precomp()...\n")
      ncols <- 2 * max.col - 1
      nrows <- 2 * max.row - 1
      rdist.precomp <- matrix (rdist (matrix (c (max.row, max.col), ncol=2, byrow=T),
                                      as.matrix (expand.grid (col=1:ncols, row=1:nrows))),
                               ncol=ncols, byrow=T)
      return (rdist.precomp)
    }


## samples: a 2 col matrix containing x and y coordinates representing end-points
                                        # result: a matrix where the element contains min distance to a sample
get.sample.distance.mat.from.line <- function(rdist.precomp,
                                                x1,
                                                y1,
                                                x2,
                                                y2,
                                                x.min,
                                                y.min,
                                                x.max,
                                                y.max)
  {
      dist.cur <- rdist.lookup.min (rdist.precomp, data.frame (x = c(x1, x2), y = c(y1, y2)))
      dist.line <- get.density.from.line (x1, y1, x2, y2, x.min, y.min, x.max, y.max)
      dist.cur <- pmin (dist.cur, dist.line)
      return (t (dist.cur))
    }

## pos is a data.frame(x = vector(int), y = vector(int))
rdist.lookup.min <- function (rdist.precomp,
                                pos)
  {
      dist.cur <- rdist.lookup (rdist.precomp,
                                pos[1,])
      if (nrow (pos) > 1)
        {
          for (i in (2:nrow (pos)))
            {
              dist.new <- rdist.lookup (rdist.precomp, pos[i,])
              dist.cur <- pmin (dist.cur, dist.new)
            }
        }
      return (dist.cur)
    }

rdist.lookup <- function (rdist.precomp,
                            pos)
  {
      mid.x <- max.col <- (ncol (rdist.precomp) + 1) / 2
      mid.y <- max.row <- (nrow (rdist.precomp) + 1) / 2
      start.x <- mid.x - (pos$x - 1)
      start.y <- mid.y - (pos$y - 1)
      end.x <- start.x + max.col - 1
      end.y <- start.y + max.row - 1
      relative.rdist <- rdist.precomp [start.y:end.y, start.x:end.x]
      return (relative.rdist)
    }

get.density.from.line <- function(x1,
                                    y1,
                                    x2,
                                    y2,
                                    x.min,
                                    y.min,
                                    x.max,
                                    y.max)
  {
      b <- get.boundary.lines(x1, y1, x2, y2, x.min, y.min, x.max, y.max)
      mat <- min.dist.to.line.with.boundaries(x1, y1, x2, y2, b$left, b$right, x.max, y.max)
      return(mat)
    }

## input
  ## two end points and dimensions
  ## output
  ## two lines that represent boundaries formed by borders and lines orthogonal to the end points
get.boundary.lines <- function(x1, y1, x2, y2, x.min, y.min, x.max, y.max)
  {
      ## Make sure that x1,y1 is the left-most point
      if (x2 < x1)
        {
          t <- x1; x1 <- x2; x2 <- t
          t <- y1; y1 <- y2; y2 <- t
        }
                                        # Get the slope and intercepts at boundary points
      if (y1 == y2)
        {
          return(get.boundary.lines.main.horizontal(x1,x2,y.min,y.max))
        } else if (x1 == x2)
          {
            return(get.boundary.lines.main.vertical(y1,y2,x.min,x.max))
          }
        else {
          mainline.si <- get.slope.intercept(x1, y1, x2, y2)
          slope <- mainline.si [1]
          orthog.slope <-  -(1/slope)
          intercept <- mainline.si [2]
          x.offset <- 100
          ## x3, y3 ####### FIX
          x3 <- x1 + x.offset
          y3 <- x.offset * orthog.slope + y1 
          ## x4, y4 ###### FIX
          x4 <- x2 + x.offset 
          y4 <- (x.offset) * orthog.slope + y2
          intercept3 <- get.slope.intercept(x1, y1, x3, y3) [2]
          intercept4 <- get.slope.intercept(x2, y2, x4, y4) [2]
          line3 <- int.border.points(orthog.slope, intercept3, x.min, y.min, x.max, y.max)
          line4 <- int.border.points(orthog.slope, intercept4, x.min, y.min, x.max, y.max)
          return(list(left = line3, right = line4))
        }
    }

get.boundary.lines.main.horizontal <- function(x1, x2, y.min, y.max)
  {
      left <- data.frame(x = x1, y = (y.min : y.max))
      right <- data.frame(x = x2, y = (y.min : y.max))
      return (list(left = left, right = right))
    }

get.boundary.lines.main.vertical <- function(y1, y2, x.min, x.max)
  {
      left <- data.frame(x = (x.min:x.max), y = y1)
      right <- data.frame(x = (x.min:x.max), y = y2)
      return (list(left = left, right = right))
    }

## Input
                                        # End points of a line segment
                                        # Boundaries
                                        #  b1 is the left-most boundary and is a data.frame with x,y
                                        #  b2 is the right-most boundary and is a data.frame with x,y
                                        # Matrix dimensions
  ## Output
                                        # Matrix with min distance to line segment within boundaries
min.dist.to.line.with.boundaries <- function(x1, y1, x2, y2, left, right, xdim, ydim)
  {
      ## make x1,y1 the left most point
      if (x2 < x1)
        {
          t <- x1; x1 <- x2; x2 <- t
          t <- y1; y1 <- y2; y2 <- t
        }

      mat <- matrix(Inf, ncol = xdim, nrow = ydim)
      ## In case line vertical
      if (x1 == x2)
        {
          hline <- abs( (1:xdim) - x1 )
          if (y2 > y1)
            {
              mat[y1:y2,] <- matrix(hline, nrow = abs(y1 - y2) + 1, ncol = length (hline), byrow = T)
            }
          else
            {
              mat[y2:y1,] <- matrix(hline, nrow = abs(y1 - y2) + 1, ncol = length (hline),  byrow = T)
            }
          return(mat)
        }
      else
        {
          left <- get.unique.ys(left$x, left$y)
          right <- get.unique.ys(right$x, right$y)
          ## left and right should be the same length
          ## ignore those pairs where the x values are the same
          boundaries <- data.frame(left.x = left$x, left.y = left$y,
                                   right.x = right$x, right.y = right$y)
          boundaries <- boundaries[boundaries$left.x != boundaries$right.x,]
          for (i in (1:length(boundaries$left.x)))
            {
              lx <- boundaries$left.x[i]
              ly <- boundaries$left.y[i]
              
              starting.val <- get.distance.from.line.segment(lx, ly,
                                                             x1, y1,
                                                             x2, y2)
              next.val <- get.distance.from.line.segment(lx+1, ly,
                                                         x1, y1,
                                                         x2, y2)
              ##step <- get.hline.step(x1,y1,x2,y2)
              ##step <- step * (next.val - starting.val)
              step <- next.val - starting.val
              hline <- ((0 : (boundaries$right.x[i] - boundaries$left.x[i])) * step) + starting.val
              hline <- abs(hline)
              mat[boundaries$left.y[i], boundaries$left.x[i]:boundaries$right.x[i]] <- hline
            }
          return (mat)
        }
    }

## input
  ##   slope is the slope of the boarder.
  ##   bug is when abs(slope) is greater than 1
  ## output
  ##  data.frame(x = vector(), y = vector())
int.border.points <- function(slope, intercept, x.min = 1, y.min = 1, x.max, y.max)
  {

      ## We want x.bar and y.bar to be ordered by ascending y values
      ## Just find the segment of the line within the boarders
      ## x-major
      if (abs(slope) <= 1)
        {
          ## x-major
          if (slope >= 0)
            x.bar <- x.min : x.max
          else
            x.bar <- x.max : x.min
          y.bar <- round(x.bar * slope + intercept)
          ## Keep those coordinates that are inside the box
          mask <- y.min <= y.bar & y.bar <= y.max
          x.bar <- x.bar[mask]
          y.bar <- y.bar[mask]
        }
      else
        {
          ## y-major
          ## boundaries are y.min and y.max
          y.bar <- y.min : y.max
          x.bar <- round((y.bar - intercept) / slope)
          mask <- x.min <= x.bar & x.bar <= x.max
          x.bar <- x.bar[mask]
          y.bar <- y.bar[mask]
        }

      
      ## x.min and x.max line line
      ## These are the lines pressed up against the edge of the box.
      left.x.bar <- NULL
      left.y.bar <- NULL
      right.y.bar <- NULL
      right.x.bar <- NULL
      if (slope >= 0)
        {
          if (y.bar[1] > y.min)
            {
              left.y.bar <- y.min : (y.bar[1] - 1)
              left.x.bar <- rep(x.min, length(left.y.bar))
            }
          if (y.bar[length(y.bar)] < y.max)
            {
              right.y.bar <- (y.bar[length(y.bar)] + 1) : y.max
              right.x.bar <- rep(x.max, length(right.y.bar))
            }
          x.bar <- c(left.x.bar, x.bar, right.x.bar)
          y.bar <- c(left.y.bar, y.bar, right.y.bar)
          
        } else {
          if (y.bar[1] > y.min)
            {
              right.y.bar <- y.min : (y.bar[1] - 1)
              right.x.bar <- rep(x.max, length(right.y.bar))
            }
          if (y.bar[length(y.bar)] < y.max)
            {
              left.y.bar <- (y.bar[length(y.bar)] + 1) : y.max
              left.x.bar <- rep(x.min, length(left.y.bar))
            }
          x.bar <- c(right.x.bar, x.bar, left.x.bar)
          y.bar <- c(right.y.bar, y.bar, left.y.bar)
          
        }
      
      
      return (data.frame(x = x.bar, y = y.bar))
    }

## Input
                                        # x y coordinates
                                        # Assumes that y values sorted (either ascending or descending)
                                        # Output
                                        # x y coordinates with unique ys
get.unique.ys <- function(x, y)
  {
      prev.x <- new.x <- x[1]
      prev.y <- new.y <- y[1]
      if (length(x) == 1)
        {
          return (data.frame(x = new.x, y = new.y))
        }
      else
        {
          for (i in (2:length(x)))
            {
              yi <- y[i]
              if (yi != prev.y)
                {
                  prev.y <- yi
                  new.y <- c(new.y, yi)
                  new.x <- c(new.x, x[i])
                }
            }
        }
      return (data.frame(x = new.x, y = new.y))
    }

looplines.dist.density <- function (rdist.precomp, xmin, ymin, xmax, ymax)
  {
      x.min.mat.coord <- 1
      y.min.mat.coord <- 1
      x.max.mat.coord <- (ncol (rdist.precomp) + 1) / 2
      y.max.mat.coord <- (nrow (rdist.precomp) + 1) / 2
      x.vect <- c (xmin, xmax, xmax, xmin)
      y.vect <- c (ymin, ymin, ymax, ymax)
      
      for (i in 1:4)
        {
          j <- if (i < 4)
            { i + 1 } else { 1 }
          temp.density <- get.sample.distance.mat.from.line (rdist.precomp = rdist.precomp,
                                                             x1 = get.coord (x.vect [i], x.max.mat.coord),
                                                             y1 = get.coord (y.vect [i], y.max.mat.coord),
                                                             x2 = get.coord (x.vect [j], x.max.mat.coord),
                                                             y2 = get.coord (y.vect [j], y.max.mat.coord),
                                                             x.min = x.min.mat.coord,
                                                             y.min = y.min.mat.coord,
                                                             x.max = x.max.mat.coord,
                                                             y.max = y.max.mat.coord)
          ##browse()
          accum.density <- if (i == 1)
            { temp.density } else { pmin (accum.density, temp.density) }
        }
      return (accum.density)
    }


## Input
                                        # End points of a line segment and some point in sample space
  ## Output
                                        # The distance between the point and the line segment
get.distance.from.line.segment <- function(px, py, x1, y1, x2, y2)
  {
                                        # Check if this is a horizontal or vertical line
      if (y1 == y2)
        {
          return (abs(py - y1))
        }
      else if (x1 == x2)
        {
          return (abs(px - x1))
        }
      else
        {
          z <- get.closest.point.on.infinite.line(px, py, x1, y1, x2, y2)
          if (between(z [1], x1, x2) && between(z [2], y1, y2))
            {
              return (sqrt((z [1] - px)^2 + (z [2] - py)^2))
            }
          else
            {
              d1 <- sqrt((px - x1)^2 + (py - y1)^2)
              d2 <- sqrt((px - x2)^2 + (py - y2)^2)
              return (min(d1, d2))
            }
        }
    }

between <- function(z, a, b)
  {
      (z <= max(a,b)) && (z >= min(a,b))
    }

get.downsample.mat <- function (mat, dim)
  {
    index <- round (seq (1, nrow (mat), length = dim))
    mat [index, index]
  }

iden <- function (x) { x }

plot.mse.vs.length.of.bombing.from.test.adaptive <- 
    function (
        mean.mat.list, 
        ls.path.list, 
        actual.bombing.mat, 
        show = T, 
        f.tweak = iden)
  {
      ##   mean.mat.list <- results $ asn.results $ mean.mat.list
      ##   ls.path.list <- results $ asn.results $ ls.path.list
      ##   actual.bombing.mat <- results $ actual.bombing.mat      
      mses <-
          {
            corrected.actual.bombing.mat <-
                if (any (dim (adp.list.first (mean.mat.list)) 
                        != dim (actual.bombing.mat)))
                {
                  cat (paste ("Downsampling actual.bombing.mat to match mean.mat.\n"))
                  downsample.index <- 
                      round (seq (
                              1, 
                              nrow (actual.bombing.mat), 
                              length = nrow (adp.list.first (mean.mat.list))))
                  actual.bombing.mat [downsample.index, downsample.index]
                }
                else
                {
                  actual.bombing.mat
                }
            sapply (
                mean.mat.list,
                function (mean.mat)
                {
                  mean ( f.tweak ((mean.mat - t (corrected.actual.bombing.mat))^2))
                })
          }
      ## Make sure the length of mses and lengths are the same.
      lengths <- get.lengths.from.ls.path.list (ls.path.list) [1:length (mses)] 
      stopifnot (length (lengths) == length (mses))
      if (show) { plot (lengths, mses) }
      return (list (mses = mses, lengths = lengths))
    }


get.lengths.from.ls.path.list <- function (ls.path.list)
{
  each.length <- sapply (ls.path.list,
                         function (ls.path)
                         {
                           xs <- ls.path[,'x']
                           ys <- ls.path[,'y']
                           sum (sqrt ((diff (xs))^2 + (diff (ys))^2))
                         })
  lengths <- cumsum (each.length)
  return (lengths)
  
}

#################
## Data structures and functions for anchored bombs.
#################

## Returns the x, y anchor points generated by observed line segments.
get.anchors <- function (ls.list)
  {
    ## For each line segment, find the list of observed.intersections and translate that into absolute x, y.
    get.anchors.helper <- function (ls)
      {
        coords.x <- vector()
        coords.y <- vector()
        len <- nrow (ls$union.intersections.df)
        if (len > 0)
          {
            for (i in (1:len))
              {
                is.start.point <- ls$union.intersections.df [i, "left"] == 0
                is.end.point <- ls$union.intersections.df [i, "right"] == ls$length
                result.left  <- if (!is.start.point) find.abs.coords (ls, ls$union.intersections.df[i, "left"])
                result.right <- if (!is.end.point) find.abs.coords (ls, ls$union.intersections.df[i, "right"])
                coords.x <- c (coords.x, result.left[1], result.right[1])
                coords.y <- c (coords.y, result.left[2], result.right[2])
              }
          }
        return (list (coords.x = coords.x,
                      coords.y = coords.y))
      }
    
    list.anchors <-
      if (length (ls.list) > 0) { lapply (ls.list, get.anchors.helper) }
      else { list() }
    x <- as.vector (unlist (sapply (list.anchors, function (list.pair) list.pair$coords.x)))
    y <- as.vector (unlist (sapply (list.anchors, function (list.pair) list.pair$coords.y)))
    mat <- cbind (x, y)
    return (mat)
  }

new.anchored.bombs <- function (anchors.mat)
  {
    stopifnot (is.matrix (anchors.mat),
               ncol (anchors.mat) == 2,
               nrow (anchors.mat) > 0)
    radius <- 0.05
    bombing.df <- new.bombing.empty (x = anchors.mat [, 'x'],
                                     y = anchors.mat [, 'y'] + radius,
                                     radius = radius,
                                     anchor.x = anchors.mat[, 'x'],
                                     anchor.y = anchors.mat[, 'y'])
    return (bombing.df)
  }

## Combination move and resize about an anchor point within a 1/10 by 1/10 square.
kernel.slide <- function (bombing.df,
                          ls.list,
                          interacting.bombs.mat,
                          xlim,
                          ylim,
                          shape,
                          scale,
                          selected = NULL,
                          added.bomb.df = NULL)
  {
    stopifnot (nrow (bombing.df) > 0)
    delta.x <- diff (xlim) / 40
    delta.y <- diff (ylim) / 40
    delta.x.selected <- runif (1, min = -abs (delta.x), max = abs (delta.x))
    delta.y.selected <- runif (1, min = -abs (delta.y), max = abs (delta.y))
    row.selected <- sample (nrow (bombing.df), 1)
    selected.x <- bombing.df [row.selected, 'x'] + delta.x.selected
    selected.y <- bombing.df [row.selected, 'y'] + delta.y.selected
    ## Remove the old bomb.
    removed.list <- kernel.death (bombing.df = bombing.df,
                                  ls.list = ls.list,
                                  interacting.bombs.mat = interacting.bombs.mat,
                                  xlim = xlim,
                                  ylim = ylim,
                                  shape = shape,
                                  scale = scale,
                                  selected = row.selected,
                                  skip.interacting = global.skip.interacting)
    ## Add the new bomb.
    added.bomb.df <- bombing.df [row.selected, ]
    added.bomb.df [1, "x"] <- selected.x
    added.bomb.df [1, "y"] <- selected.y
    added.bomb.df [1, 'radius'] <- sqrt (  (selected.x - added.bomb.df [1, 'anchor.x'])^2
                                         + (selected.y - added.bomb.df [1, 'anchor.y'])^2 ) 
    
    added.list <- kernel.birth (bombing.df = removed.list$bombing.df,
                                ls.list = removed.list$ls.list,
                                interacting.bombs.mat = removed.list$interacting.bombs.mat,
                                xlim = xlim,
                                ylim = ylim,
                                shape = shape,
                                scale = scale,
                                added.bomb.df = added.bomb.df,
                                skip.interacting = global.skip.interacting)
    return.list <- added.list
    return.list$removed.bombing.df <- removed.list$removed.bombing.df
    return (return.list)
  }

## Combination move and resize anywhere on the field.
kernel.jump <- function (bombing.df,
                         ls.list,
                         interacting.bombs.mat,
                         xlim,
                         ylim,
                         shape,
                         scale,
                         selected = NULL,
                         added.bomb.df = NULL)
  {
    stopifnot (nrow (bombing.df) > 0,
               !any (is.na (bombing.df$anchor.x)),
               !any (is.na (bombing.df$anchor.y)))
    row.selected <- sample (nrow (bombing.df), 1)
    selected.x <- runif (1, xlim [1], xlim [2])
    selected.y <- runif (1, ylim [1], ylim [2])
    
    ## Remove the old bomb.
    removed.list <- kernel.death (bombing.df = bombing.df,
                                  ls.list = ls.list,
                                  interacting.bombs.mat = interacting.bombs.mat,
                                  xlim = xlim,
                                  ylim = ylim,
                                  shape = shape,
                                  scale = scale,
                                  selected = row.selected,
                                  skip.interacting = global.skip.interacting)
    ## Add the new bomb.
    added.bomb.df <- bombing.df [row.selected, ]
    added.bomb.df [1, "x"] <- selected.x
    added.bomb.df [1, "y"] <- selected.y
    added.bomb.df [1, 'radius'] <- sqrt (  (selected.x - added.bomb.df [1, 'anchor.x'])^2
                                         + (selected.y - added.bomb.df [1, 'anchor.y'])^2 ) 
    added.list <- kernel.birth (bombing.df = removed.list$bombing.df,
                                ls.list = removed.list$ls.list,
                                interacting.bombs.mat = removed.list$interacting.bombs.mat,
                                xlim = xlim,
                                ylim = ylim,
                                shape = shape,
                                scale = scale,
                                added.bomb.df = added.bomb.df,
                                skip.interacting = global.skip.interacting)
    return.list <- added.list
    return.list$removed.bombing.df <- removed.list$removed.bombing.df
    return (return.list)
  }

## Generate quarter circle
create.quarter.circle.matrix <- function (size)
  {
    mat <- matrix (0, nrow = size, ncol = size)
    for (r in 1:nrow (mat))
      {
        col.lim <- max (1, round (size * (sqrt (1 - (r / size)^2))))
        tail.len <- size - col.lim
        new.row <- c (rep.int (1, col.lim) , rep.int (0, size - col.lim))
        mat [r, ] <- new.row
      }
    return (mat)
  }

create.full.circle.matrix <- function (qmat.size)
  {
    qmat <- create.quarter.circle.matrix (qmat.size)
    bmat <-
      {
        foo <- matrix (0, nrow = qmat.size * 2, ncol = qmat.size * 2)
        ## Quad III
        foo [1:qmat.size, 1:qmat.size] <- qmat [qmat.size:1, qmat.size:1]
        ## Quad IV
        foo [1:qmat.size, (qmat.size+1):(qmat.size*2)] <- qmat [qmat.size:1, ]
        ## Quad II
        foo [(qmat.size+1):(qmat.size*2), 1:qmat.size] <- qmat [, qmat.size:1]
        ## Quad I
        foo [(qmat.size+1):(qmat.size*2), (qmat.size+1):(qmat.size*2)] <- qmat
        foo
      }
    return (bmat)
  }

create.sub.sample <- function (new.size, ref.matrix)
  {
    
    col <- ncol (ref.matrix)
    row <- nrow (ref.matrix)
    stopifnot (col == row,
               new.size <= row)
    if (new.size == 1)
      {
        return (matrix (1, nrow = 1, ncol = 1))
      }
    else
      {
        sub.seq <- floor (seq.int (1, col, length.out = new.size))
        sub.mat <- ref.matrix [sub.seq, sub.seq]
        return (sub.mat)
      }
  }
## Generate a list of circle matrix images.
## Start with 100 x 100, and then down sample.
create.list.circle.matrices <- function (max.size)
  {
    
    dot <- matrix(1, nrow = 1, ncol = 1)
    full.circle.matrix <- create.full.circle.matrix (ceiling (max.size / 2))
    the.list <- lapply (1:max.size,
                        function (size)
                        {
                          create.sub.sample (size, full.circle.matrix)
                        })
    return ( the.list )
  }

get.relative.valid.ranges <- function (circle.mat.size, image.mat.size, abs.ll.x, abs.ll.y)
  {
    helper <- function (abs.ll)
      {
        lower.abs <- max (abs.ll,
                          1)
        upper.abs <- min (abs.ll + circle.mat.size - 1,
                          image.mat.size)
        if (lower.abs > upper.abs)
          {
            lower.abs <- NA
            upper.abs <- NA
          }
        lower.rel <- max (lower.abs - abs.ll + 1,
                          1)
        upper.rel <- min (upper.abs - abs.ll + 1,
                          circle.mat.size)
        if (!is.na (lower.abs))
          {
            stopifnot ((upper.abs - lower.abs) == (upper.rel - lower.rel))
          }
        return (list (lower.abs = lower.abs,
                      upper.abs = upper.abs,
                      lower.rel = lower.rel,
                      upper.rel = upper.rel))
      }
    ranges.x <- helper (abs.ll.x)
    ranges.y <- helper (abs.ll.y)
    if (any (is.na (c (ranges.x, ranges.y))))
      {
        return (NA)
      }
    else
      {
        return (list (ranges.x = ranges.x,
                      ranges.y = ranges.y))
      }
  }

## Generate an x and y coordinate matrix image.
create.x.y.cover.matrices <- function (bombing.df, image.size, list.circle.matrices)
  {
    stopifnot (nrow (bombing.df) > 0)
    mat.x <- matrix (NA, nrow = image.size, ncol = image.size)
    mat.y <- matrix (NA, nrow = image.size, ncol = image.size)
    ## Sort bombing.df by radius size small to big.
    sorted.bombing.df <- bombing.df [order (bombing.df[, 'radius']), ]
    for (i in 1:nrow (sorted.bombing.df))
      {
        bomb.df <- sorted.bombing.df [i, ]
        pixel.diameter <- floor (bomb.df [1, 'radius'] * 2 * image.size)
        stopifnot (pixel.diameter < length (list.circle.matrices))
        abs.ll.x <- floor ((bomb.df [1, 'x'] - bomb.df [1, 'radius']) * image.size)
        abs.ll.y <- floor ((bomb.df [1, 'y'] - bomb.df [1, 'radius']) * image.size)
        ranges <- get.relative.valid.ranges (circle.mat.size = pixel.diameter,
                                             image.mat.size = image.size,
                                             abs.ll.x = abs.ll.x,
                                             abs.ll.y = abs.ll.y)
        if (all (!is.na (unlist (ranges))))
          {
            circle.mat.clipped <-
              if ( all (ranges $ ranges.y $ lower.rel == 1,
                        ranges $ ranges.y $ upper.rel == pixel.diameter,
                        ranges $ ranges.x $ lower.rel == 1,
                        ranges $ ranges.x $ upper.rel == pixel.diameter))
                {
                  list.circle.matrices [[pixel.diameter]]
                }
              else
                {
                  list.circle.matrices [[pixel.diameter]] [ (ranges $ ranges.y $ lower.rel) : (ranges $ ranges.y $ upper.rel),
                                                           (ranges $ ranges.x $ lower.rel) : (ranges $ ranges.x $ upper.rel) ]
                }
            range.row <-  (ranges $ ranges.y $ lower.abs) : (ranges $ ranges.y $ upper.abs)
            range.col <- (ranges $ ranges.x $ lower.abs) : (ranges $ ranges.x $ upper.abs)
            mat.x [ range.row, range.col ] [ circle.mat.clipped != 0 ] <- bomb.df [1, 'x']
            mat.y [ range.row, range.col ] [ circle.mat.clipped != 0 ] <- bomb.df [1, 'y']            
          }
      }
    return (list (mat.x = mat.x,
                  mat.y = mat.y))
  }

## Take a chain.list and create a x.y cover matrices.
create.x.y.cover.matrices.from.chain.list <- function (chain.list, image.size, list.circle.matrices, start.at)
  {
    stopifnot (is.list (chain.list),
               start.at <= length (chain.list),
               start.at >= 1)
    num.row <- tail (chain.list, 1) [[1]] $ end.index - chain.list [[start.at]] $ start.index + 1
    num.col <- image.size^2
    index.start <- chain.list [[start.at]] $ start.index
    giant.mat.x <- matrix (NA,
                           nrow = num.row,
                           ncol = num.col)
    giant.mat.y <- matrix (NA,
                           nrow = num.row,
                           ncol = num.col)
    pb <- txtProgressBar (min = start.at, max = length (chain.list), style = 3)
    for (i in start.at:(length (chain.list)))
      {
        ## cat (paste (i, '\n'))
        setTxtProgressBar (pb, i)
        if (nrow (chain.list [[i]]$bombing.df) > 0)
          {
            list.mat <- create.x.y.cover.matrices (bombing.df = chain.list[[i]]$bombing.df,
                                                   image.size = image.size,
                                                   list.circle.matrices = list.circle.matrices)
            mat2.x <- matrix (as.vector (list.mat$mat.x),
                              nrow = ((chain.list[[i]] $ end.index) - (chain.list[[i]] $ start.index) + 1),
                              ncol = num.col,
                              byrow = T)
            mat2.y <- matrix (as.vector (list.mat$mat.y),
                              nrow = ((chain.list[[i]] $ end.index) - (chain.list[[i]] $ start.index) + 1),
                              ncol = num.col,
                              byrow = T)
            index.range <- ((chain.list[[i]] $ start.index) : (chain.list[[i]] $ end.index)) - index.start + 1
            giant.mat.x [index.range, ] <- mat2.x
            giant.mat.y [index.range, ] <- mat2.y
          }
      }
    close (pb)
    return (list (giant.mat.x = giant.mat.x,
                  giant.mat.y = giant.mat.y))
  }

## Returns:   cbind (mean.vect.x,
##                   mean.vect.y)
create.mean.vect.mat <- function (giant.mat.x, giant.mat.y)
  {
    stopifnot (all (dim (giant.mat.x) == dim (giant.mat.y)))
    image.side <- sqrt (ncol (giant.mat.x))
    helper <- function (giant.mat)
      {
        mean.vect <- apply (X = giant.mat,
                            MARGIN = 2,
                            FUN = function (col) { mean (col, na.rm = T) })
      }
    mean.vect.abs.x <- helper (giant.mat.x) 
    mean.vect.abs.y <- helper (giant.mat.y)
    ## x,y vals of the points
    vect.vals.x <- rep ((1:image.side) / image.side, each = image.side)
    vect.vals.y <- rep ((1:image.side) / image.side, image.side)
    mean.vect.rel.x <- mean.vect.abs.x - vect.vals.x
    mean.vect.rel.y <- mean.vect.abs.y - vect.vals.y
    return (cbind (mean.vect.rel.x,
                   mean.vect.rel.y))
  }

## Returns: anon list [ prc | NA ] with element number corresponding to a pixel.
create.prcomp.vect.mat <- function (giant.mat.x, giant.mat.y)
  {
    stopifnot (all (dim (giant.mat.x) == dim (giant.mat.y)))
    giant.rows <- nrow (giant.mat.x)
    coords.mat <- get.coords.from.index (index = 1:(ncol (giant.mat.x)),
                                          collength = sqrt (ncol (giant.mat.x)))
    prc.mat <- matrix (NA,
                       nrow = 11,
                       ncol = ncol (giant.mat.y),
                       ## Row names assumes that x is V1, and y is V2 in prcomp (...).
                       dimnames = list (c (
                         'rot.x',
                         'rot.y',
                         'center.x',
                         'center.y',
                         'sdev',
                         'cover.prob',
                         'rel.center.x',
                         'rel.center.y',
                         'center.dist',
                         'unit.rel.center.x',
                         'unit.rel.center.y')))
    pb <- txtProgressBar (min = 1, max = ncol (giant.mat.x), style = 3)

    for (i in 1:ncol (giant.mat.x))
      {
        setTxtProgressBar (pb, i)
        nao <-na.omit (as.data.frame (cbind (giant.mat.x [, i],
                                             giant.mat.y [, i])))
        cover.prob <- nrow (nao) / giant.rows 
        if (nrow (nao) > 0)
          {
            prc <- prcomp (nao)
            prc.mat [, i] <- c (prc$rotation [, 'PC1'], # rot.{x,y}
                                prc$center, # center.{x,y}
                                prc$sdev [1], # sdev
                                cover.prob, # cover.prob
                                NA, NA, NA, NA, NA) # rel.center.{x,y}, center.dist, unit.rel.center.{x,y} 
          }
      }
    close (pb)

    ## Convert NA cover.probs to be 1.
    prc.mat ['cover.prob', is.na (prc.mat ['cover.prob', ])] <- 0
    
    stopifnot (ncol (prc.mat) == nrow (coords.mat))
    prc.mat [c ('rel.center.x', 'rel.center.y'), ] <-
      prc.mat [ c('center.x', 'center.y'), ] - t (coords.mat)
    prc.mat ['center.dist', ] <-
      sqrt (prc.mat ['rel.center.x', ]^2 + prc.mat[ 'rel.center.y', ]^2)
    prc.mat ['unit.rel.center.x', ] <-
      prc.mat ['rel.center.x', ] / prc.mat ['center.dist', ]
    prc.mat ['unit.rel.center.y', ] <-
      prc.mat ['rel.center.y', ] / prc.mat ['center.dist', ]

      
    return (prc.mat)
  }

## INPUT: index may be a vector
## return (cbind (x,y))
get.coords.from.index <- function (index, collength)
  {
    x <- (index - 1) %/% collength + 1
    y <- (index - 1) %% collength + 1
    return (cbind (x, y) / collength)
  }

get.prc.df <- function (prc.mat)
  {
    ## create a data.frame with x, y, delta.x, delta.y.
    image.size <- sqrt (ncol (prc.mat))
    x.vect <- rep (1:image.size, each = image.size)
    y.vect <- rep (1:image.size, tmes = image.size)
    df <- data.frame (x = x.vect,
                      y = y.vect)
    prc.df <- as.data.frame (t (prc.mat))
    prc.df <- cbind (prc.df, x = x.vect, y = y.vect)
    return (prc.df)
                      
  }

#' Creates a function that produces an XOR (a vector of arrivals),
#' where 0 has an arrival rate of q0, and 1 has a rate of q1.
#'
#' If the initial state is 1, then the first arrival time is "0.0".
#' Arrivals greater than the duration are ignored.
#' The duration must be positive.
#' @param q0 the transition arrival rate when the chain is in state 0.
#' @param q1 the transition arrival rate when the chain is in state 1 (flip).
#' @return A function that takes \code{duration} and \code{initial_state = NULL} and
#'        then returns a vector of arrivals (transitions), starting with 0->1 transition.
#' @export
#' @author Andrew Parker \email{adparker@@gmail.com}
#' @examples
#'  f <- createCTMCFunction (1, 10)
#'  f (duration = 10)
createCTMCFunction <- function (q0, q1)
  {
    qvect <- c (q0, q1)
    p <- q0 / (q0 + q1) ## prob. of starting in state 1.
     
    function (duration, initial_state = NULL)
      {
        stopifnot (duration > 0.0)
        if (is.null (initial_state))
          {
            initial_state <- rbinom (1, 1, p)
          }
        pointer <- initial_state
        arrivals <- vector ()
        last_arrival <- 0.0
        if (initial_state == 1) { arrivals <- 0.0 }
        while (T)
          {
            step <-  rexp (1, qvect [ pointer + 1 ])
            stopifnot (step > 0.0)
            last_arrival <- step + last_arrival
            if (last_arrival > duration) { break }
            arrivals <- c (arrivals, last_arrival)
            pointer <- (pointer + 1) %% 2
          }
        return (arrivals)
      }
  }

#' Adds CTMC noise to a line segment object.
#'
#' Takes a CTMC function and line segment object, and then creates a new line segment object
#' where the union.interesects.df field has been XOR'd with the output of the CTMC function.
#'
#' @param ctmcFun is a function produced by \code{createCTMCFunction}.
#' @param ls is a \code{line segment object}.
#' @return a new \code{line segment object} identical to \code{ls}, except with noise added.
#' @export
#' @author Andrew Parker \email{adparker@@gmail.com}
addCTMCNoise <- function (ctmcFun, ls)
  {
    arrivals <- ctmcFun (duration = ls $ length)
    ## If arrivals is odd, then it changes the end thing.
    ## If the last right entry is == duration, then remove it.
    ## Otherwise, add it.
    jumble <- c (arrivals,
                 ls $ union.intersections.df $ left,
                 ls $ union.intersections.df $ right ## There is code
                                                     ## later that
                                                     ## depends on
                                                     ## this entry
                                                     ## being last.
                 )
    ## If arrivals is empty, just return ls.
    if (length (arrivals) == 0)
      {
        return (ls)
      }
    
    ## Possibly fix things up if number of arrivals is odd.
    if (length (arrivals) %% 2 == 1)
      {
        rightLength <- length (ls $ union.intersections.df $ right) 
        if ((rightLength > 0)
            && (isTRUE (all.equal (ls $ union.intersections.df $ right [[rightLength]],
                                   ls $ length))))
          {## Pop it off from jumble.
            jumble <- jumble [ - length (jumble) ]
          }
        else
          {## Append length (or duration to jumble)
            jumble <- c (jumble, ls $ length) ## This assumes that
                                              ## jumble is created
                                              ## with ... $ right
                                              ## being last. See
                                              ## above.
          }
      }
    jumble <- sort (jumble)
    stopifnot (length (jumble) %% 2 == 0)
    oddIndices <- seq (from = 1, to = length (jumble), by = 2) 
    left <- jumble [ oddIndices ]
    right <- jumble [ - oddIndices ]
    ls $ union.intersections.df <-  data.frame (left = left, right = right)             
    return (ls)
  }

create.line.segment.observations.from.samples.df <- function (samples.df.fname, xmax = 480, ymax = 480)
  {
    load (samples.df.fname) ## obj
    ## > names (obj)
    ##  [1] "run.id"            "epoch.id"          "stratum.id"       
    ##  [4] "x"                 "y"                 "z"                
    ##  [7] "start.x"           "start.y"           "end.x"            
    ## [10] "end.y"             "time.stamp"        "z.normalized"     
    ## [13] "z.orig"            "z.normalized.orig"
    samples.df <- obj
    ## Split into a list of data.frames based on start.x, start.y, end.x, end.y
    ## Most of the list elements will be empty.
    foo = split (obj, list (obj$start.x, obj$start.y, obj$end.x,  obj$end.y))
    ## Remove the empty (zero length) data.frames from the list.
    isempty.select <- sapply (foo, function (item) { nrow (item) == 0 })
    foo.notempty <- foo
    foo.notempty [ isempty.select ] <- NULL
    proposed.line.segments <-
      lapply (foo.notempty,
              function (df)
              {
                ## Create a line segment object.
                ls <- new.line.segment (x1 = df$start.x [1] / xmax,
                                        y1 = df$start.y [1] / ymax,
                                        x2 = df$end.x [1] / xmax,
                                        y2 = df$end.y [1] / ymax)
                samples.vect <- df$z.normalized
                ## Find the change points
                diff.samples.vect <- diff (samples.vect)
                stopifnot (all (unique (diff.samples.vect) %in% c (-1, 0, 1)))
                ## Prepend and append points to cover cases where the
                ## end points are inside a bomb.
                change.points.vect <- c (samples.vect[1],
                                         diff.samples.vect,
                                         -1 * samples.vect[length (samples.vect)])
                stopifnot (sum (change.points.vect) == 0)
                change.points.select <- (change.points.vect == 1) | (change.points.vect == -1)
                ## Get locations of the change points.
                index.change.points <- (1:length(change.points.vect)) [change.points.select]
                ## Assert that index.change.points is an even number
                ## (even 0)
                stopifnot ((length (index.change.points) %% 2) == 0) 
                if (length (index.change.points) == 0)
                  {
                    return (ls)
                  }
                else
                  {
                    ## Normalize the index (there's a rounding
                    ## error... can't get to zero)
                    scaled.change.points <- index.change.points / length (change.points.vect) * ls$length
                    ## The change points alternate between, open,
                    ## close, open, close...
                    ls$union.intersections.df <- new.union.intersections (left = scaled.change.points[seq(1, length (index.change.points), 2)],
                                                                          right = scaled.change.points[seq(2, length (index.change.points), 2)])
                    return (ls)
                  }
              })
    return (proposed.line.segments)
  }
