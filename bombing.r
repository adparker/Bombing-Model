## library(grid)
## library(lattice)
## library(ggplot2)
## source("utils.r")
#' @include utils.r

draw <- F;
disp <- x11;

## draw <- T;
## disp <- quartz;
if (exists ('draw') && !draw)  { registerDoMC () }

dbmsg <- F; ## Debug messages
global.grow.window.size <- F ## Whether or not to grow the speculative window.

## source("misc.r")
#' @include misc.r
roxygen()



test.green.ratio.generic <- function(real.lambda = 10, gen.lambda = 10, est.lambda = 10, green.ratio.fun, kernel.fun)
{
 ## Generate a sample from the model.
  xmax <- 1
  ymax <- 1
  xlim <- c(0, xmax)
  ylim <- c(0, ymax)
  obsx <- 0.5
  obsy <- 0.5
  scale <- 0.05
  shape <- 1
  point.cost <- 1
  mark.cost.scale <- 1
  excl.scale <- 1
  disagree.scale <- 1
  time.step <- 0
  temp.start <- 1
  annealing.alpha <- 1
  ## The actual bombing is unobserved.
  actual.bombing.df <- new.bombing (lambda = real.lambda, shape = shape, scale = scale, xlim=xlim, ylim=ylim)
  
  ## Generate line segments.
  ls.list <- list()
  ls.list[[1]] <- new.line.segment (0,0, 1,0)
  ls.list[[2]] <- new.line.segment (0,0, 1,1)
  ls.list[[3]] <- new.line.segment (0,0, 0,1)
  list.line.segment.observations <- get.proposed.line.segment.observations (ls.list, actual.bombing.df)
  ## For the purposes of the test, this is some random configuration we're choosing to be the
  ## the current configuration of the MC.
  current.bombing.df <- new.bombing (lambda = gen.lambda, shape = shape, scale = scale, xlim=xlim, ylim=ylim) 
  current.ls.list.bombed <- get.proposed.line.segment.observations (ls.list, current.bombing.df)
  current.interacting.bombs.mat <- new.interacting.bombs.mat (current.bombing.df)

  proposal.list <- kernel.fun (bombing.df = current.bombing.df,
                               ls.list = current.ls.list.bombed,
                               interacting.bombs.mat = interacting.bombs.mat,
                               xlim = xlim,
                               ylim = ylim,
                               shape = shape,
                               scale = scale)
                                        
  ## And now find the potential for the bombings.
  current.potential <- get.potential (list.line.segment.observations = list.line.segment.observations,
                                      proposed.bombing.df = current.bombing.df,
                                      proposed.ls.list = current.ls.list.bombed,
                                      interacting.bombs.mat = current.interacting.bombs.mat,
                                      point.cost = point.cost,
                                      mark.cost.scale = mark.cost.scale,
                                      excl.scale = excl.scale,
                                      disagree.scale = disagree.scale)
  proposed.potential <- get.potential (list.line.segment.observations = list.line.segment.observations,
                                       proposed.bombing.df = proposal.list$bombing.df,
                                       proposed.ls.list = proposal.list$ls.list,
                                       interacting.bombs.mat = proposal.list$interacting.bombs.mat,
                                       point.cost = point.cost,
                                       mark.cost.scale = mark.cost.scale,
                                       excl.scale = excl.scale,
                                       disagree.scale = disagree.scale)
  
  result <- green.ratio.fun (current.bombing.df,
                             current.potential,
                             proposal.list$bombing.df,
                             proposed.potential,
                             area.window = abs((xlim[1]-xlim[2]) * (ylim[1]-ylim[2])),
                             lambda = est.lambda)
  return (result)
}

test.turn.crank <- function () {
  ## Generate a sample from the model.
  xmax <- 1
  ymax <- 1
  xlim <- c(0, xmax)
  ylim <- c(0, ymax)
  ## area.window <- xmax * ymax
  obsx <- 0.5
  obsy <- 0.5
  scale <- 0.05
  shape <- 1
  lambda <- 10
  point.cost <- 1
  mark.cost.scale <- 1
  excl.scale <- 1
  disagree.scale <- 1
  time.step <- 0
  temp.start <- 1
  annealing.alpha <- 1
  
  ## The actual bombing is unobserved.
  actual.bombing.df <- new.bombing (lambda = lambda, shape = shape, scale = scale, xlim=xlim, ylim=ylim)
  ## For the purposes of the test, this is some random configuration we're choosing to be the
  ## the current configuration of the MC.
  current.bombing.df <- new.bombing (lambda = lambda, shape = shape, scale = scale, xlim=xlim, ylim=ylim) 

  ## Generate line segments.
  ls.list <- list()
  ls.list[[1]] <- new.line.segment (0,0, 1,0)
  ls.list[[2]] <- new.line.segment (0,0, 1,1)
  ls.list[[3]] <- new.line.segment (0,0, 0,1)
  list.line.segment.observations <- get.proposed.line.segment.observations (ls.list, actual.bombing.df)
  current.ls.list.bombed <- get.proposed.line.segment.observations (ls.list, current.bombing.df)
  current.interacting.bombs.mat <- new.interacting.bombs.mat (bombing.df = current.bombing.df)
  current.potential <- get.potential (list.line.segment.observations = list.line.segment.observations,
                                      proposed.bombing.df = current.bombing.df,
                                      proposed.ls.list = current.ls.list.bombed,
                                      interacting.bombs.mat = current.interacting.bombs.mat,
                                      point.cost = point.cost,
                                      mark.cost.scale = mark.cost.scale,
                                      excl.scale = excl.scale,
                                      disagree.scale = disagree.scale)
  
  next.list <- turn.crank (list.line.segment.observations = list.line.segment.observations,
                           current.bombing.df = current.bombing.df,
                           current.lines = current.ls.list.bombed,
                           current.potential = current.potential,
                           current.interacting.bombs.mat = current.interacting.bombs.mat,
                           current.removed.bombing.df = NULL,
                           current.added.bombing.df = NULL,
                           annealing.scale = 1,
                           area.window = abs((xlim[1] - xlim[2]) * (ylim[1] - ylim[2])),
                           point.cost = point.cost,
                           mark.cost.scale = mark.cost.scale,
                           excl.scale = excl.scale,
                           disagree.scale = disagree.scale,
                           xlim = xlim,
                           ylim = ylim,
                           shape = shape,
                           scale = scale,
                           lambda = lambda)
  return (next.list)
}
## Return:
## list (
#            actual.bombing.mat = matrix (),        
#            list.line.segment.observations.list = list(),
#            chain.list.list = list(),
#            mean.mat.list = list())
test.raster.sample.n <-
  function (n,
            num.lines.range,
            final.alpha,
            point.cost,
            excl.scale,
            width,
            height,
            parball.fname,
            save.fname,
            samples.fnames = NULL)
{
  stopifnot (any (is.null (num.lines.range), is.null (samples.fnames)),
             !all (is.null (num.lines.range), is.null (samples.fnames)))
  if (draw) { graphics.off() }
  results <-
    {
      list ( # Missing dist.density.raw.list, ls.path.list, pot.field.mat
            actual.bombing.mat = matrix (),
            list.line.segment.observations.list = list(),
            chain.list.list = list(),
            mean.mat.list = list(),
            ls.path.list = list())
    }
  ## This is lame, because I don't know how to create an iterator that
  ## recycles 'NULL', so I have to use 'NA' instead.
  if (is.null (num.lines.range))
    {
      num.lines.range <- recycle (NA, length (samples.fnames)) ## vector that repeats NULL
    }
  else if (is.null (samples.fnames))
    {
      samples.fnames <- recycle (NA, length (num.lines.range)) ## iterator that repeats NA
    }
  foreach (i = icount (max (length (num.lines.range),
                            length (samples.fnames))),
           num.lines.elem = num.lines.range,
           samples.fname.elem = samples.fnames) %do% {
             ## I know, this part is lame.
             num.lines <- if (!is.na (num.lines.elem))
               {
                 num.lines.elem
               } # else NULL
             list.line.segments <- if (!is.na (samples.fname.elem))
               {
                 create.line.segment.observations.from.samples.df (samples.fname.elem)
               } # else NULL
             crank.results <- 
               test.turn.crank.n (n = n,
                                  final.alpha = final.alpha,
                                  disagree.scale = 100,
                                  num.lines = num.lines,
                                  point.cost = point.cost,
                                  excl.scale = excl.scale,
                                  pot.field.width = width,
                                  pot.field.height = height,
                                  parball.fname = parball.fname,
                                  scale = 0.1,
                                  shape = 2,
                                  lambda = 20,
                                  list.line.segment.observations = list.line.segments)
             results $ list.line.segment.observations.list [[i]] <- 
               crank.results $ list.line.segment.observations 
             results $ chain.list.list [[i]] <- crank.results $ chain.list
             results $ mean.mat.list [[i]] <- crank.results $ mean.mat
             results $ ls.path.list [[i]] <- crank.results $ls.path
             if (i == 1) 
               { 
                 results $ actual.bombing.mat <- crank.results $ actual.bombing.mat 
               }
             save (results, file = save.fname)
           }
  return (results)
}

#' Produce a spatial prediction, possibly using the bombing model.
#'
#' Given a set of observed line segments within a boolean random field,
#' produce a spatial prediction, possibly using the bombing model or
#' bilinear interpolation.
#'
#' The bombing model option uses the Metropolis-Hastings (MH) algorithm to
#' generate samples.
#'
#' @param n the number of iterations of the MH algorithm
#' @param final.alpha asdf
#' @param disagree.scale asdf
#' @param num.lines asdf
#' @param empty asdf
#' @param pot.field.width asdf
#' @param pot.field.height asdf
#' @param simple.bombing asdf
#' @param point.cost asdf
#' @param excl.scale asdf 
#' @param mark.cost.scale asdf
#' @param parball.fname asdf
#' @param use.anchors asdf
#' @param scale asdf 
#' @param shape asdf
#' @param lambda asdf
#' @param ls.list asfd
#' @param actual.bombing.mat asdfasdf
#' @param list.line.segment.observations asdf
#' @param xlim asdf
#' @param ylim asdf
#' @return \code{ (list (actual.bombing.mat = actual.bombing.mat,
#'               list.line.segment.observations = list.line.segment.observations,
#'               chain.list = chain.list,
#'               ls.path = ls.path)) }
#' @export
#' @author Andrew Parker \email{adparker@@gmail.com}
#' @examples
#'  circle <- test.turn.crank.n(n = 100,
#'                              pot.field.width = 480,
#'                              pot.field.height = 480,
#'                              final.alpha = 0.5,
#'                              simple.bombing = F,
#'                              point.cost = 3,
#'                              excl.scale = 20,
#'                              num.lines=5,
#'                              parball.fname = "~/experiments/2009.10.09/circle_binary/parball/parball.1.RData");
#' # NOT RUN
#' # image (circle $ mean.mat)
test.turn.crank.n <- function (n = 10,
                               final.alpha = 1,
                               disagree.scale = 100,
                               num.lines = 10,
                               empty = F,
                               pot.field.width = 100,
                               pot.field.height = 100,
                               simple.bombing = F,
                               point.cost = 1,
                               excl.scale = 1,
                               mark.cost.scale = 1,
                               parball.fname = NULL,
                               use.anchors = F,
                               scale = 0.1,
                               shape = 2,
                               lambda = 20,
                               ls.list = NULL,
                               actual.bombing.mat = NULL,
                               list.line.segment.observations = NULL,
                               xlim = c (0, 1),
                               ylim = c (0, 1))
{

  ## Generate a sample from the model.
  ## xmax <- 1
##   ymax <- 1
##   xlim <- c(0, xmax)
##   ylim <- c(0, ymax)
##   obsx <- 0.5
##   obsy <- 0.5
## scale <- 0.05
#  shape <- 1
#  lambda <- 20
  temp.start <- 1
  annealing.alpha <- exp(log(final.alpha)/n)
  ls.path <- NULL
  
  ## The actual bombing is unobserved.
  actual.bombing.mat <-
    if (is.null (actual.bombing.mat))
      {
        get.actual.bombing.mat (parball.fname = parball.fname,
                                lambda = lambda,
                                shape = shape,
                                scale = scale,
                                xlim = xlim,
                                ylim = ylim,
                                simple.bombing = simple.bombing,
                                empty = empty)
      }
    else
      {
        actual.bombing.mat
      }
    
  ## Generate line segments.
  list.line.segment.observations <-
    if (is.null (list.line.segment.observations))
      {
        if (is.null (ls.list))
          {
            ls.path <-
              {
                loop.points <- looplines (0, 0, 1, 1)
                raster.points <- rect.rasterlines (num.lines, 0, 0, 1, 1)
                points.df <- rbind (loop.points, raster.points)
                as.matrix (points.df)
              }
            ls.list <-
              {
                get.list.line.segments.from.ls.path (ls.path)
              }
          }
        stopifnot (diff (dim (actual.bombing.mat)) == 0) ## dimensions are equal.
        get.line.segment.observations.from.mat (ls.list,
                                                actual.bombing.mat)
      }
    else
      {
        list.line.segment.observations
      }
  
  
  if (use.anchors)
    {
      anchors.mat <- get.anchors (list.line.segment.observations)
      anchored.bombs.df <- new.anchored.bombs (anchors.mat)
      prev.list <- list (bombing.df = anchored.bombs.df,
                         potential = 0,
                         interacting.bombs.mat = new.interacting.bombs.mat ())
      browser()
    }
  else
    {
      anchors.mat <- NULL
      prev.list <- NULL
    }
  #display.grob.segments (list.line.segment.observations,
   #                      actual.bombing.df)
  if (draw)
    {
      disp()
      display.grob.segments (list.line.segment.observations)
      disp()
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
  start.at <- round (length (chain.list) / 2)
  mean.mat <-
    if (start.at >= 1)
      {
        get.mean.matrix.from.chain.list (chain.list = chain.list,
                                         width = pot.field.height,
                                         height = pot.field.width,
                                         start.at = start.at)
      }
    else
      {
        matrix (data = 0,
                nrow = pot.field.height,
                ncol = pot.field.width)
      }
  pot.field.mat <- 
      {
        pot.field.mat.raw <- dnorm ((mean.mat - 0.5), sd = 0.447)
        pot.field.mat.centered <- (pot.field.mat.raw - mean (pot.field.mat.raw))
        pot.field.mat.centered
      }
  if (draw)
    {
      disp (title = "pot field")
      image (pot.field.mat,
             zlim = c (dnorm (0.5, sd = 0.447),
               dnorm (0, sd = 0.447)))
      disp (title = "mean.field")
      image (mean.mat,
             zlim = c (0,1))
      disp (title = "potentials")
      potentials <- lapply (chain.list,
                            function (elem) {elem$potential})
      plot (1:length (potentials),
            potentials)
    }
  return (list (actual.bombing.mat = actual.bombing.mat,
                list.line.segment.observations = list.line.segment.observations,
                chain.list = chain.list,
                pot.field.mat = pot.field.mat,
                mean.mat = mean.mat,
                ls.path = ls.path
                ))
}

test.get.grob.segments <- function(num.lines = 10)
{
  
  ## Generate a sample from the model.
  xmax <- 1
  ymax <- 1
  xlim <- c(0, xmax)
  ylim <- c(0, ymax)
  obsx <- 0.5
  obsy <- 0.5
  scale <- 0.05
  shape <- 1
  lambda <- 20
  ## The actual bombing is unobserved.
  actual.bombing.df <- new.bombing (lambda = lambda, shape = shape, scale = scale, xlim=xlim, ylim=ylim)
  
  ## Generate line segments.
  ls.list <- list()
  for (i in (1:num.lines)) {
    y.pos <- 1/(num.lines-1) * (i-1)
    ls.list[[i]] <- new.line.segment (0, y.pos, 1, y.pos)
    cat(paste("y.pos ", y.pos, "\n"))
  }
  list.line.segment.observations <- get.proposed.line.segment.observations (ls.list, actual.bombing.df)
    
  display.bombing(actual.bombing.df)
  disp()
  display.grob.segments(list.line.segment.observations, actual.bombing.df)
  return (0)
}


test.potential.proposed.line.segment <- function (n=10,
                                                  final.alpha=1,
                                                  disagree.scale = 1,
                                                  num.lines = 11,
                                                  empty = F,
                                                  dnorm.sd = 0.447,
                                                  height = 480,
                                                  width = 480) {
  result <- test.turn.crank.n (n=n,
                               final.alpha=final.alpha,
                               disagree.scale=disagree.scale,
                               num.lines=num.lines,
                               empty=empty)
  chain.list <- result$chain.list
  pot.field.mat <- dnorm((get.mean.matrix.from.chain.list (chain.list = chain.list,
                                                           width = width,
                                                           height = height,
                                                           start.at = round (length (chain.list) / 2)) - 0.5),
                         sd=dnorm.sd)
  ## Propose a line segment:
  line.segment <- new.line.segment (0,0, 1,1)
  potential <- get.potential.proposed.line.segment (line.segment,
                                                    pot.field.mat = pot.field.mat,
                                                    dnorm.sd = dnorm.sd)
  return (potential)
}


test.line.turn.crank <- function (n = 10,
                                  xlim = c (0,1),
                                  ylim = c (0,1),
                                  ls.path.angle.scale = 1,
                                  ls.path.point.cost = 1,
                                  ls.path.lambda = 3,
                                  final.alpha = 1)                                  
{
  ls.path.annealing.scale <-  exp (log (final.alpha)/n)

  ## Generate a pot.field.mat
  pot.field.mat <-
    {
      mat <- matrix (0, nrow=100, ncol=100)
      mat[33:66, 33:66] <- 1
      mat
    }

  ## Generate current ls.path
  current.ls.path <- insert.ls.path (ls.path = new.ls.path (0, 0),
                                     x = 1,
                                     y = 1,
                                     position = 1,
                                     pot.field.mat = pot.field.mat)
  
  ## Generate current.ls.path.potential
  current.ls.path.potential <- get.potential.ls.path (ls.path = current.ls.path,
                                                      ls.path.point.cost = ls.path.point.cost,
                                                      ls.path.angle.scale = ls.path.angle.scale)

  ## Show the first part of the path.
  ls.path.gtree <- display.grob.gtree (get.line.segment.observations.from.ls.path (current.ls.path,
                                                                                   data.frame()),
                                       name = "segments")
##################### END SETUP #########################
  next.ls.path.and.potential <- line.turn.crank (pot.field.mat = pot.field.mat,
                                                 current.ls.path = current.ls.path,
                                                 current.ls.path.potential = current.ls.path.potential,
                                                 xlim = xlim,
                                                 ylim = ylim,
                                                 ls.path.angle.scale = ls.path.angle.scale,
                                                 ls.path.point.cost = ls.path.point.cost,
                                                 ls.path.lambda = ls.path.lambda,
                                                 ls.path.annealing.scale = ls.path.annealing.scale)
  ## Show the rest of the path.
  new.ls.path.obs <- get.line.segment.observations.from.ls.path (next.ls.path.and.potential$ls.path,
                                                                 data.frame())
  new.ls.path.grob <- get.grob.segments.single.grob (new.ls.path.obs,
                                                     name = "newseg",
                                                     color = "green")
  grid.add (gPath ("segments"),
            applyEdit (new.ls.path.grob,
                       gEdit (gp = gpar (col = "green"))))
  return (next.ls.path.and.potential)
}


test.line.turn.crank.n <- function (n=10, final.alpha=1, disagree.scale = 1, num.lines = 11, empty = F, dnorm.sd = 0.447, line.n = 10, width=width, height=height)
{

  ## Generate a sample from the model.
  xmax <- 1
  ymax <- 1
  xlim <- c(0, xmax)
  ylim <- c(0, ymax)
  ## area.window <- xmax * ymax
  obsx <- 0.5
  obsy <- 0.5
  scale <- 0.05
  shape <- 1
  lambda <- 20
  point.cost <- 1
  mark.cost.scale <- 1
  excl.scale <- 1
  temp.start <- 1
  annealing.alpha <- exp(log(final.alpha)/n)
  ## The actual bombing is unobserved.
  actual.bombing.df <-
    if (empty) { new.bombing.empty() }
    else { new.bombing (lambda = lambda, shape = shape, scale = scale, xlim=xlim, ylim=ylim) }
  actual.bombing.mat <- get.matrix.from.bombing (boming.df = actual.boming.df,
                                                 width = 480,
                                                 height = 480)

  ## Generate observed line segments.
  ls.list <- list()
  if (num.lines > 0) {
    for (i in (1:num.lines)) {
      y.pos <- 1/(num.lines-1) * (i-1)
      ls.list[[i]] <- new.line.segment (0, y.pos, 1, y.pos)
      cat(paste("y.pos ", y.pos, "\n"))
    }
  }
  list.line.segment.observations <- get.line.segment.observations.from.mat (ls.list,
                                                                            actual.bombing.mat)
  ## Generate field estimates.
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
                              annealing.alpha = annealing.alpha)

  pot.field.mat <- dnorm((get.mean.matrix.from.chain.list (chain.list = chain.list,
                                                           width = width,
                                                           height = height,
                                                           start.at = round (length (chain.list) / 2)) - 0.5),
                         sd=dnrom.sd)
  ##################### END SETUP #########################

  line.chain.list <- line.turn.crank.n (list.line.segment.observations = list.line.segment.observations,
                                        xlim = xlim,
                                        ylim = ylim,
                                        start.x = 0,
                                        start.y = 0,
                                        dnorm.sd = dnorm.sd,
                                        line.n = line.n,
                                        pot.field.mat = pot.field.mat)
  return (line.chain.list)
}

get.actual.bombing.mat <- function (parball.fname, lambda, shape, scale, xlim, ylim, simple.bombing = F, empty)
  {
    if (is.null (parball.fname))
      {
        actual.bombing.df <-
          if (empty)
            {
              new.bombing.empty()
            }
          else if (simple.bombing)
            {
              data.frame (x = 0.5,
                          y = 0.5,
                          radius = 0.2)
            }
          else
            {
              new.bombing (lambda = lambda,
                           shape = shape,
                           scale = scale,
                           xlim = xlim,
                           ylim = ylim)
            }
        get.matrix.from.bombing (actual.bombing.df,
                                 width = 480,
                                 height = 480)
      }
    else
      {
        trimmed.parball <-
          {
            parball <- safe.load (file = parball.fname)
            side <- min (dim (parball[[1]]))
            parball [[1]] [1:side, 1:side]
          }
        if (all(unique (trimmed.parball) %in% c (0, 1)))
          {
            trimmed.parball
          }
        else
          {
            thresh <- otsu (trimmed.parball) $ threshold
            binarize.matrix (mat = trimmed.parball,
                             thresh = thresh)
          }
      }
  }

test.adaptive.sample.n <- function (moves.n = 10,
                                    line.n = 10,
                                    n = 10,
                                    final.alpha = 1,
                                    disagree.scale = 100,
                                    dnorm.sd = 0.447,
                                    width = 100,
                                    height = 100,
                                    ls.path.lambda = 1,
                                    ls.path.final.alpha = 1,
                                    ls.path.angle.scale = 1,
                                    ls.path.point.cost = 1,
                                    ls.path.temp.start = 1,
                                    ls.path.data.scale = 1,
                                    ls.path.interaction.scale = 1,
                                    ls.path.dist.scale = 1,
                                    ls.path.len.scale = 1,
                                    point.cost = 1,
                                    mark.cost.scale = 1,
                                    excl.scale = 1,
                                    temp.start = 1,
                                    parball.fname = NULL,
                                    sample.perimeter = F,
                                    inherit.next.list = F,
                                    use.bombing = T,
                                    use.prc = F,
                                    use.anchors = F) {
  if (draw)
    {
      disp() ## Draws quartz if on a mac.
    }

  ## Generate a sample from the model.
  xmax <- 1
  ymax <- 1
  xlim <- c(0, xmax)
  ylim <- c(0, ymax)
  scale <- 0.1 ## 0.05
  shape <- 2 ## 1
  lambda <- 20
  start.x <- 0
  start.y <- 0
  annealing.alpha <- exp(log(final.alpha)/n)
  ls.path.annealing.alpha <- exp (log (ls.path.final.alpha) / line.n)
  
  ## The actual bombing is unobserved.
  actual.bombing.mat <- get.actual.bombing.mat (parball.fname = parball.fname,
                                                lambda = lambda,
                                                shape = shape,
                                                scale = scale,
                                                xlim = xlim,
                                                ylim = ylim,
                                                simple.bombing = F,
                                                empty = F)
  ## Do the sampling.
  asn.results <- adaptive.sample.n (moves.n = moves.n,
                                    actual.bombing.mat = actual.bombing.mat,
                                    n = n,
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
                                    start.x = start.x,
                                    start.y = start.y,
                                    dnorm.sd = dnorm.sd,
                                    line.n = line.n,
                                    mean.field.width = width,
                                    mean.field.height = height,
                                    ls.path.angle.scale = ls.path.angle.scale,
                                    ls.path.point.cost = ls.path.point.cost,
                                    ls.path.lambda = ls.path.lambda,
                                    ls.path.temp.start = ls.path.temp.start,
                                    ls.path.annealing.alpha = ls.path.annealing.alpha,
                                    ls.path.data.scale = ls.path.data.scale,
                                    ls.path.interaction.scale = ls.path.interaction.scale,
                                    ls.path.dist.scale = ls.path.dist.scale,
                                    ls.path.len.scale = ls.path.len.scale,
                                    sample.perimeter = sample.perimeter,
                                    distance.field.width = width,
                                    distance.field.height = height,
                                    inherit.next.list = inherit.next.list,
                                    use.bombing = use.bombing,
                                    use.prc = use.prc,
                                    use.anchors = use.anchors)
                                    
  
  ##results[[length (results) + 1]] <- actual.bombing.mat
  return (list (asn.results = asn.results,
                actual.bombing.mat = actual.bombing.mat))
}

test.display <- function ()
{
############################
  ## Generate a sample from the model.
  xmax <- 1
  ymax <- 1
  obsx <- 0.5
  obsy <- 0.5
  bombing1.df <- new.bombing (lambda = 100, shape = 1, scale = 0.05, xlim=c(0,xmax), ylim=c(0,ymax))
  bombing2.df <- new.bombing (lambda = 100, shape = 1, scale = 0.05, xlim=c(0,xmax), ylim=c(0,ymax))
  
  ## Create a visual
  grid.newpage ()
  vp0 <- viewport (width=1, height=1, gp=gpar(alpha=1))
  vp1 <- viewport (width=1, height=1, gp=gpar(alpha=0.5))
  vp2 <- viewport (width=1, height=1, gp=gpar(alpha=1))
  pushViewport (vp0)
  ## Draw the frame
  grid.rect (gp=gpar(fill="black"))
  popViewport()

  ## Draw bombing 1
  pushViewport(vp1)
  g1 <- grob.bombing (bombing1.df)
  g1 <- editGrob(g1, gp = gpar (col="blue", fill="blue", alpha=1))
  grid.draw (g1) 
  popViewport()
  return()
  
  ## Draw bombing 2
  pushViewport(vp2)
  g2 <- grob.bombing (bombing2.df)
  ##gt2 <- gTree (gp = gpar (col="red", fill="red", alpha=1), children = gList(g2))
  #grid.draw (g2)
  popViewport()
  ##grid.rect (gp=gpar(col="blue"))
#################################
}

test.get.mean.image <- function (n=10, final.alpha=1, disagree.scale = 1, num.lines = 11, empty = F, width = 480, height = 480)
{
  result <- test.turn.crank.n (n = n,
                               final.alpha = final.alpha,
                               disagree.scale = disagree.scale,
                               num.lines = num.lines,
                               empty = empty)
  mean.mat <- get.mean.matrix.from.chain.list (chain.list = result$chain.list,
                                               width = width,
                                               height = height,
                                               start.at = round (length (result$chain.list) / 2))
  return (mean.mat)
}

test.display.grob.tree <- function (num.lines = 10)
{
  ## Generate a sample from the model.
  xmax <- 1
  ymax <- 1
  xlim <- c(0, xmax)
  ylim <- c(0, ymax)
  obsx <- 0.5
  obsy <- 0.5
  scale <- 0.05
  shape <- 1
  lambda <- 20
  ## The actual bombing is unobserved.
  actual.bombing.mat <-
    {
      actual.bombing.df <- new.bombing (lambda = lambda, shape = shape, scale = scale, xlim=xlim, ylim=ylim)
      get.matrix.from.bombing (actual.bombing.df,
                               width = 480,
                               height = 480)
    }
  ## Generate line segments.
  ls.list <- list()
  for (i in (1:num.lines)) {
    y.pos <- 1/(num.lines-1) * (i-1)
    ls.list[[i]] <- new.line.segment (0, y.pos, 1, y.pos)
    cat(paste("y.pos ", y.pos, "\n"))
  }
  list.line.segment.observations <- get.line.segment.observations.from.mat (ls.list, actual.bombing.mat)
  display.grob.gtree (list.line.segment.observations, name = "foo")
  ## Generate current ls.path
  ## current.ls.path <- insert.ls.path (ls.path = new.ls.path (0, 0),
##                                      x = 1,
##                                      y = 1,
##                                      position = 1,
##                                      pot.field.mat = matrix (0, nrow=10, ncol=10))
  
##   ls.path.gtree <- display.grob.tree (get.line.segment.observations.from.ls.path (current.ls.path,
##                                                                                   actual.bombing.df)
  return (list.line.segment.observations)
}


blah <- function()
  {
    down.sample <- round (seq (1, 480, length.out = 100))
    actual.bombing.ds <- actua.bombing.mat [down.sample, down.sample]
  }
