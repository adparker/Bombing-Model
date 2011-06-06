## source("bombing.r")
#' @include bombing.r
roxygen()

## options(error = recover)

run.tests1 <- function (b.range,
                        i.range = 1:3)
  {
    for (b in b.range)
      for (i in i.range)
        for (use.bombing in c (T, F))
          {
            shape = sprintf ('b%d-%d', b, i)
            if (use.bombing)
              for (use.prc in c (T,F))
                {
                  results <- asn_experiment (shape = shape,
                                             use.bombing = use.bombing,
                                             use.prc = use.prc)
                }
            else
              {
                results <- asn_experiment (shape = shape,
                                           use.bombing = use.bombing,
                                           use.prc = F)
              }
          }
  }

run.tests2 <- function ()
  {
    results1 <- asn_experiment ("bombing2", T)
    results1 <- asn_experiment ("static2", T)
  }

run.tests3 <- function ()
  {
    results1 <- asn_experiment ("bombing3", T)
    results1 <- asn_experiment ("static3", T)
  }

run.tests4 <- function ()
{
  results1 <- asn_experiment ("bombing4", T)
  results1 <- asn_experiment ("bombing5", T)
}

asn_experiment <- 
    function (
        shape, 
        use.bombing, 
        use.prc, 
        use.anchors = F)
{
  parball.fname <- 
      paste (
          "~/experiments/2009.12.07/", 
          shape , 	
          "/parball/parball.1.RData",
          sep = "")	
  save.fname <- 
      paste (
          "~/experiments/2009.12.07/asn_",
          if (use.bombing) { "bombing" } else { "bilinear"},
          if (use.prc) { "_prc" } else { ""},
          "_",
          shape,
          ".RData",
          sep = "")
  results <-
      test.adaptive.sample.n (
          n = 10000, #1000
          line.n = 20000, #5000
          moves.n = 12, # 10
          final.alpha = 0.5,
          point.cost = 3,
          excl.scale = 20,
          width = 100,
          height = 100,
          ls.path.lambda = 20, # 2
          ls.path.final.alpha = 0.5,
          ls.path.angle.scale = 0,
          ls.path.point.cost = 0, # 2
          ls.path.data.scale = 300, ##200, 1000,
          ls.path.interaction.scale = 100,
          ls.path.dist.scale = 100, ## 1/width * SCALE where SCALE = 100
          ls.path.len.scale = 10,
          sample.perimeter = T,
          parball.fname = parball.fname,
          inherit.next.list = T,
          use.bombing = use.bombing,
          use.prc = use.prc,
          use.anchors = use.anchors)
  save (results, file = save.fname)
  return (results)
}

raster_experiment <- function (shape = NULL, parball.fname = NULL, save.fname = NULL, samples.fnames = NULL)
  ## 'b1-3'  
{
  stopifnot (all (!is.null (shape),
                  is.null (parball.fname),
                  is.null (save.fname),
                  is.null (samples.fnames))
             || all (is.null (shape),
                     !is.null (parball.fname),
                     !is.null (save.fname),
                     !is.null (samples.fnames)))
  num.lines.range <- NULL
  if (!is.null (shape))
    {
      parball.fname <- paste ("~/experiments/2009.12.07/", shape,
                              "/parball/parball.1.RData", sep = '')
      save.fname <- paste ("~/experiments/2009.12.07/raster_", '_', shape,
                           'R.Data', sep = '')
      num.lines.range <- 1:10 
    }
  
  results <- 
    test.raster.sample.n (n = 10000,
                          num.lines.range = num.lines.range,
                          final.alpha = 0.5,
                          point.cost = 3,
                          excl.scale = 20,
                          width = 480,
                          height = 480,
                          parball.fname = parball.fname,
                          save.fname = save.fname,
                          samples.fnames = samples.fnames)
  return (results)
}

asn_bilinear_circle <- function()
{
   results <- test.adaptive.sample.n (n = 1000, #1000
                                      line.n = 5000, #5000
                                      moves.n = 20, # 10
                                      final.alpha = 0.5,
                                      point.cost = 3,
                                      excl.scale = 20,
                                      width = 100,
                                      height = 100,
                                      ls.path.lambda = 20, # 2
                                      ls.path.final.alpha = 0.5,
                                      ls.path.angle.scale = 0,
                                      ls.path.point.cost = 0, # 2
                                      ls.path.data.scale = 200, ##1000,
                                      ls.path.interaction.scale = 100,
                                      ls.path.dist.scale = 100, ## 1/width * SCALE where SCALE = 100
                                      ls.path.len.scale = 10,
                                      parball.fname = "~/experiments/2009.10.09/circle_binary/parball/parball.1.RData",
                                      inherit.next.list = T,
                                      use.bombing = F)
   save (results, file = "~/experiments/2009.10.09/asn_bilinear_circle.RData")
  return (results)
}

asn_bombing_circle <- function()
{
   results <- test.adaptive.sample.n (n = 1000, #1000
                                      line.n = 5000, #5000
                                      moves.n = 20, # 10
                                      final.alpha = 0.5,
                                      point.cost = 3,
                                      excl.scale = 20,
                                      width = 100,
                                      height = 100,
                                      ls.path.lambda = 20, # 2
                                      ls.path.final.alpha = 0.5,
                                      ls.path.angle.scale = 0,
                                      ls.path.point.cost = 0, # 2
                                      ls.path.data.scale = 200, ##1000,
                                      ls.path.interaction.scale = 100,
                                      ls.path.dist.scale = 100, ## 1/width * SCALE where SCALE = 100
                                      ls.path.len.scale = 10,
                                      sample.perimeter = T,
                                      parball.fname = "~/experiments/2009.10.09/circle_binary/parball/parball.1.RData",
                                      inherit.next.list = T,
                                      use.bombing = T)
   save (results, file = "~/experiments/2009.10.09/asn_bombing_circle.RData")
  return (results)
}

asn_bilinear_bombing1 <- function()
{
   results <- test.adaptive.sample.n (n = 1000, #1000
                                      line.n = 5000, #5000
                                      moves.n = 20, # 10
                                      final.alpha = 0.5,
                                      point.cost = 3,
                                      excl.scale = 20,
                                      width = 100,
                                      height = 100,
                                      ls.path.lambda = 20, # 2
                                      ls.path.final.alpha = 0.5,
                                      ls.path.angle.scale = 0,
                                      ls.path.point.cost = 0, # 2
                                      ls.path.data.scale = 200, ##1000,
                                      ls.path.interaction.scale = 100,
                                      ls.path.dist.scale = 100, ## 1/width * SCALE where SCALE = 100
                                      ls.path.len.scale = 10,
                                      sample.perimeter = T,
                                      parball.fname = "~/experiments/2009.10.09/bombing1_binary/parball/parball.1.RData",
                                      inherit.next.list = T,
                                      use.bombing = F)
   save (results, file = "~/experiments/2009.10.09/asn_bilinear_bombing1.RData")
  return (results)
}

asn_bombing_bombing1 <- function()
{
   results <- test.adaptive.sample.n (n = 1000, #1000
                                      line.n = 5000, #5000
                                      moves.n = 20, # 10
                                      final.alpha = 0.5,
                                      point.cost = 3,
                                      excl.scale = 20,
                                      width = 100,
                                      height = 100,
                                      ls.path.lambda = 20, # 2
                                      ls.path.final.alpha = 0.5,
                                      ls.path.angle.scale = 0,
                                      ls.path.point.cost = 0, # 2
                                      ls.path.data.scale = 200, ##1000,
                                      ls.path.interaction.scale = 100,
                                      ls.path.dist.scale = 100, ## 1/width * SCALE where SCALE = 100
                                      ls.path.len.scale = 10,
                                      sample.perimeter = T,
                                      parball.fname = "~/experiments/2009.10.09/bombing1_binary/parball/parball.1.RData",
                                      inherit.next.list = T,
                                      use.bombing = T)
   save (results, file = "~/experiments/2009.10.09/asn_bombing_bombing1.RData")
  return (results)
}

asn_bilinear_static3 <- function()
{
   results <- test.adaptive.sample.n (n = 1000, #1000
                                      line.n = 5000, #5000
                                      moves.n = 20, # 10
                                      final.alpha = 0.5,
                                      point.cost = 3,
                                      excl.scale = 20,
                                      width = 100,
                                      height = 100,
                                      ls.path.lambda = 20, # 2
                                      ls.path.final.alpha = 0.5,
                                      ls.path.angle.scale = 0,
                                      ls.path.point.cost = 0, # 2
                                      ls.path.data.scale = 200, ##1000,
                                      ls.path.interaction.scale = 100,
                                      ls.path.dist.scale = 100, ## 1/width * SCALE where SCALE = 100
                                      ls.path.len.scale = 10,
                                      sample.perimeter = T,
                                      parball.fname = "~/experiments/2009.10.09/static3_binary/parball/parball.1.RData",
                                      inherit.next.list = T,
                                      use.bombing = F)
   save (results, file = "~/experiments/2009.10.09/asn_bilinear_static3.RData")
  return (results)
}

asn_bombing_static3 <- function()
{
   results <- test.adaptive.sample.n (n = 1000, #1000
                                      line.n = 5000, #5000
                                      moves.n = 20, # 10
                                      final.alpha = 0.5,
                                      point.cost = 3,
                                      excl.scale = 20,
                                      width = 100,
                                      height = 100,
                                      ls.path.lambda = 20, # 2
                                      ls.path.final.alpha = 0.5,
                                      ls.path.angle.scale = 0,
                                      ls.path.point.cost = 0, # 2
                                      ls.path.data.scale = 200, ##1000,
                                      ls.path.interaction.scale = 100,
                                      ls.path.dist.scale = 100, ## 1/width * SCALE where SCALE = 100
                                      ls.path.len.scale = 10,
                                      sample.perimeter = T,
                                      parball.fname = "~/experiments/2009.10.09/static3_binary/parball/parball.1.RData",
                                      inherit.next.list = T,
                                      use.bombing = T)
   save (results, file = "~/experiments/2009.10.09/asn_bombing_static3.RData")
  return (results)
}

exp_al_bombing1 <- function()
{
  results <- test.adaptive.sample.n (n = 1000, #1000
                                     line.n = 5000, #5000
                                     moves.n = 10, # 10
                                     final.alpha = 0.5,
                                     point.cost = 3,
                                     excl.scale = 20,
                                     width = 100,
                                     height = 100,
                                     ls.path.lambda = 20, # 2
                                     ls.path.final.alpha = 0.5,
                                     ls.path.angle.scale = 0,
                                     ls.path.point.cost = 0, # 2
                                     ls.path.data.scale = 200, ##1000,
                                     ls.path.interaction.scale = 100,
                                     ls.path.dist.scale = 100, ## 1/width * SCALE where SCALE = 100
                                     ls.path.len.scale = 10,
                                     sample.perimeter = T,
                                     parball.fname = "~/experiments/2009.10.09/bombing1_binary/parball/parball.1.RData",
                                     inherit.next.list = T)
  save (results, file = "~/experiments/2009.10.09/adaptive_bombing1.RData")
  browser()

}

exp_al_circle <- function()
{
  results <- test.adaptive.sample.n (n = 1000, #1000
                                     line.n = 5000, #5000
                                     moves.n = 10, # 10
                                     final.alpha = 0.5,
                                     point.cost = 3,
                                     excl.scale = 20,
                                     width = 100,
                                     height = 100,
                                     ls.path.lambda = 20, # 2
                                     ls.path.final.alpha = 0.5,
                                     ls.path.angle.scale = 0,
                                     ls.path.point.cost = 0, # 2
                                     ls.path.data.scale = 200, ##1000,
                                     ls.path.interaction.scale = 100,
                                     ls.path.dist.scale = 100, ## 1/width * SCALE where SCALE = 100
                                     ls.path.len.scale = 10,
                                     sample.perimeter = T,
                                     parball.fname = "~/experiments/2009.10.09/circle_binary/parball/parball.1.RData",
                                     inherit.next.list = T)
  save (results, file = "~/experiments/2009.10.09/adaptive_circle.RData")
  return (results)

}

exp_al_static3 <- function()
{
  results <- test.adaptive.sample.n (n = 1000, #1000
                                     line.n = 5000, #5000
                                     moves.n = 10, # 10
                                     final.alpha = 0.5,
                                     point.cost = 3,
                                     excl.scale = 20,
                                     width = 100,
                                     height = 100,
                                     ls.path.lambda = 20, # 2
                                     ls.path.final.alpha = 0.5,
                                     ls.path.angle.scale = 0,
                                     ls.path.point.cost = 0, # 2
                                     ls.path.data.scale = 200, ##1000,
                                     ls.path.interaction.scale = 100,
                                     ls.path.dist.scale = 100, ## 1/width * SCALE where SCALE = 100
                                     ls.path.len.scale = 10,
                                     sample.perimeter = T,
                                     parball.fname = "~/experiments/2009.10.09/static3_binary/parball/parball.1.RData")
  save (results, file = "~/experiments/2009.10.09/adaptive_static3.RData")
  browser()
  
}

exp_disc_interp <- function ()
{
  circle <- test.turn.crank.n(n = 20000,
                              pot.field.width = 480,
                              pot.field.height = 480,
                              final.alpha = 0.5,
                              simple.bombing = F,
                              point.cost = 3,
                              excl.scale = 20,
                              num.lines=5,
                              parball.fname = "~/experiments/2009.10.09/circle_binary/parball/parball.1.RData");
  save (circle, file = "~/experiments/2009.10.09/circle_20000_alpha_0.5.RData");
  foo <- circle
  actual.bombing.mat = foo$actual.bombing.mat
  chain.list = foo$chain.list
  mean.mat = foo$mean.mat
  bombing.df = chain.list[[length (chain.list)]]$bombing.df
  get.mse.of.bombing.df (bombing.df, t(actual.bombing.mat))
  mean ( (mean.mat - t(actual.bombing.mat))^2)
  browser()
}

exp_b1_interp <- function ()
{
  bombing1 <- test.turn.crank.n(n = 10000,
                              pot.field.width = 480,
                              pot.field.height = 480,
                              final.alpha = 0.5,
                              simple.bombing = F,
                              point.cost = 3,
                              excl.scale = 20,
                              num.lines=5,
                              parball.fname = "~/experiments/2009.10.09/bombing1_binary/parball/parball.1.RData");
  save (bombing1, file = "~/experiments/2009.10.09/bombing1_20000_alpha_0.5.RData");
  foo <- bombing1
  actual.bombing.mat = foo$actual.bombing.mat
  chain.list = foo$chain.list
  mean.mat = foo$mean.mat
  bombing.df = chain.list[[length (chain.list)]]$bombing.df
  bombing.df.mse = get.mse.of.bombing.df (bombing.df, t(actual.bombing.mat))
  mean.mat.mse  = mean ( (mean.mat - t(actual.bombing.mat))^2)
  browser()
}

exp_s3_interp <- function ()
{
  static3 <- test.turn.crank.n(n = 20000,
                              pot.field.width = 480,
                              pot.field.height = 480,
                              final.alpha = 0.5,
                              simple.bombing = F,
                              point.cost = 3,
                              excl.scale = 20,
                              num.lines=5,
                              parball.fname = "~/experiments/2009.10.09/static3_binary/parball/parball.1.RData");
  save (static3, file = "~/experiments/2009.10.09/static3_20000_alpha_0.5.RData");
  foo <- static3
  actual.bombing.mat = foo$actual.bombing.mat
  chain.list = foo$chain.list
  mean.mat = foo$mean.mat
  bombing.df = chain.list[[length (chain.list)]]$bombing.df
  get.mse.of.bombing.df (bombing.df, t(actual.bombing.mat))
  mean ( (mean.mat - t(actual.bombing.mat))^2)
  browser()
}

test.looplines.dist.density <- function ()
{
  rdist.precomp <- create.precomp (max.col = 100,
                                   max.row = 100)
  looplines.dist.density (rdist.precomp = rdist.precomp,
                          xmin = 0,
                          ymin = 0,
                          xmax = 1,
                          ymax = 1)
}

## > get.mse.from.bilinear.with.active.paths(foo$asn.results$ls.path.list, foo$actual.bombing.mat)
##
## INPUT: adpative.results is from adaptive.sample.n(), which is a list of a list of 2: mean.mat and ls.path.
get.mse.from.bilinear.with.active.paths <- function(ls.path.list, true.image, show = T)
{
  mse.vect <- vector()
  ls.path <- get.ls.paths (ls.path.list)
  lengths <- get.ls.path.lengths (ls.path)
  int.points <- get.int.points.from.real.points (ls.path, nrow (true.image), ncol (true.image))
  image.hat <- get.image.hat (int.points, true.image)
  if (show)
    {
      display (title = "true image")
      image (true.image)
      display (title = "image hat")
      image (image.hat)
    }
  mse <- get.image.hat.mse (true.image, image.hat)
  return (mse)
}
  
##   for (i in (5:nrow(ls.path)))
##     {
##       cat (paste ("loop:", i, '/', nrow (ls.path)))
##       int.points <- get.int.points.from.real.points (ls.path[1:i,], nrow (true.image), ncol (true.image))
##       image.hat <- get.image.hat (int.points, true.image)
##       image(image.hat)
##       mse <- get.image.hat.mse (true.image, image.hat)
##       mse.vect <- c (mse.vect, mse)
##       cat (paste (" mse:", mse, '\n'))
##     }
##   browser()
## }



## Stuff for the paper.
## > image.active.paths (foo$asn.results$ls.path.list, foo$actual.bombing.mat)
##
## INPUT: ls.path.list is a list of ls.path things, which are 7 col matrices with names "x", "y", etc.
image.active.paths <- function (ls.path.list, actual.bombing.mat)
{
  ## Smash down the list of matrices into one matrix
  ls.path <- get.ls.paths (ls.path.list)
  ls.list <- get.list.line.segments.from.ls.path (ls.path)
  list.line.segment.observations <- get.line.segment.observations.from.mat (ls.list,
                                                                            actual.bombing.mat)
  display.grob.segments (list.line.segment.observations)
}

## Stuff for the paper.
## image.raster.scan.paths("~/experiments/2009.10.09/circle_binary/parball/parball.1.RData", 5)
image.raster.scan.paths <- function (parball.fname, num.lines)
{
  ## Get bombing as a matrix
  actual.bombing.mat <- get.actual.bombing.mat (parball.fname = parball.fname,
                                                0, 0, 0, 0, 0, F, F)
  ## Generate the line segments.
  ls.list <-
    {
      loop.points <- looplines (0, 0, 1, 1)
      raster.points <- rect.rasterlines (num.lines, 0, 0, 1, 1)
      points.df <- rbind (loop.points, raster.points)
      get.list.line.segments.from.ls.path (as.matrix (points.df))
    }
  stopifnot (diff (dim (actual.bombing.mat)) == 0) ## dimensions are equal.
  list.line.segment.observations <- get.line.segment.observations.from.mat (ls.list,
                                                                            actual.bombing.mat)
  display.grob.segments (list.line.segment.observations)
}

## Image bilinear field estimate with raster scan
image.bilinear.from.raster <- function (parball.fname, num.lines, show = T)
{
  ## Get bombing as a matrix
  actual.bombing.mat <- get.actual.bombing.mat (parball.fname = parball.fname,
                                                0, 0, 0, 0, 0, F, F)
  ls.path <-
    {
      loop.points <- looplines (0, 0, 1, 1)
      raster.points <- rect.rasterlines (num.lines, 0, 0, 1, 1)
      points.df <- rbind (loop.points, raster.points)
      as.matrix (points.df)
    }
  image.hat <- get.image.hat.from.ls.path (ls.path = ls.path,
                                           true.image = actual.bombing.mat,
                                           width = ncol (actual.bombing.mat),
                                           height = nrow (actual.bombing.mat))
  if (show) { image (t (image.hat)) }
  return (image.hat)
}

## Image bombing field estimate with raster scan
## INPUT is from test.turn.crank.n
## INPUT list (actual.bombing.mat = actual.bombing.mat,
##                 list.line.segment.observations = list.line.segment.observations,
##                 chain.list = chain.list,
##                 pot.field.mat = pot.field.mat,
##                 mean.mat = mean.mat
##                 )
image.bombing.from.test.turn.crank.n <- function (results, show = T)
{
  if (show) { image (resutls $ actual.bombing.mat) }
  return (results $ actual.bombing.mat)
}

## Image bilinear field estimate with adaptive sampling results
##
image.bilinear.from.test.adaptive <- function (results, show = T)
{
  ls.path <- get.ls.paths (results $ asn.results $ ls.path.list)
  image.hat <- get.image.hat.from.ls.path (ls.path, results $ actual.bombing.mat)
  if (show) { image (t (image.hat)) }
  return (image.hat)
}

## Image bombing field estimate with active scan
## INPUT
## list (list.line.segment.observations.list =  list (list.line.segment.observations),
##             dist.density.raw.list = list (dist.density.raw),
##             ls.path.list          = list (ls.path.loop),
##             mean.mat.list         = list (),
##             chain.list.list       = list ()
##             pot.field.mat.list    = list ())

image.bombing.from.test.adaptive <- function (results, show = T)
{
  last.mean.mat <- adp.list.last (results $ mean.mat.list)
  if (show) { image (last.mean.mat) }
  return (last.mean.mat)
}

## Plot MSE vs. num Steps (not moves) for bombing using raster (10)
##
## INPUT list (actual.bombing.mat = actual.bombing.mat,
##                 list.line.segment.observations = list.line.segment.observations,
##                 chain.list = chain.list,
##                 pot.field.mat = pot.field.mat,
##                 mean.mat = mean.mat
##                 )
##
## CHAIN.LIST is a list of
## list (bombing.df = proposal.list$bombing.df,
##                                 ls.list = proposal.list$ls.list,
##                                 potential = proposal.potential,
##                                 interacting.bombs.mat = proposal.list$interacting.bombs.mat,
##                                 annealing.scale = annealing.scale,
##                                 removed.bombing.df = proposal.list$removed.bombing.df,
##                                 added.bombing.df = proposal.list$added.bombing.df,
##                                 start.index = 1,
##                                 end.index = 1)

plot.mse.vs.steps.from.test.turn.crank.n <- function (results, step.by, show = T)
{
  ## Look through chain.list and generate a series of mean.mat against which to calculate the MSE.
  
  actual.bombing.mat <- results$actual.bombing.mat
  chain.list <- results$chain.list
  step.first <- round (length (chain.list) / 2)
  ## Generate a mean matrix for every N steps.
  total.steps <- length (chain.list)
  intermediate.steps <- seq (step.first, total.steps, by=step.by)
  end.index.vect <- vector()
  if (total.steps != intermediate.steps [length (intermediate.steps)]) {
    intermediate.steps <- c(intermediate.steps, total.steps)
  }
  mse.vect <- vector()
  
  for (i in 1:length(intermediate.steps))
    {
      mean.mat <- get.mean.matrix.from.chain.list (chain.list[step.first:intermediate.steps[i]],
                                                   width = dim (actual.bombing.mat)[1],
                                                   height = dim (actual.bombing.mat)[2],
                                                   start.at = 1)
      ##mean.mat[i] <- mean.mat * ((intermediate.steps[i-1] - step.first + 1)/(intermediate.steps[i] - step.first + 1) + tmp.mean.mat * (diff.steps)/(intermediate.steps[i] - step.first + 1))
      mse <- mean((mean.mat - t(actual.bombing.mat))^2)
      mse.vect <- c (mse.vect, mse)
      end.index.vect <- c (end.index.vect, chain.list[[intermediate.steps[i]]] $ end.index)
      cat (paste (i, '/', length(intermediate.steps), 'index: ', tail (end.index.vect,1), 'mse: ',  mse, '\n'))
      if (show) { plot (end.index.vect, mse.vect) }
    }
  ## Plot the vectors
  return (list (mse.vect = mse.vect,
                end.index.vect = end.index.vect))
}

get.mse.of.bilinear.from.raster <- function (parball.fname, num.lines, show = T)
{
  image.hat <-  image.bilinear.from.raster (parball.fname, num.lines)
  load(parball.fname)
  mse <- get.image.hat.mse (images[[1]], image.hat)
  if (show)
    {
      disp (title = "true image")
      image (images[[1]])
      disp (title = "image.hat")
      image (image.hat)
    }
  return (mse)
}

## Plot MSE vs. Raster scan path length of bilinear and bombing model estimate
##
plot.mse.vs.length.of.bilinear.from.raster <- function (parball.fname, num.lines.vect, show = T)
{
  mse.vect <- vector()
  len.vect <- vector()
  for (i in num.lines.vect)
    {
      mse. <- get.mse.of.bilinear.from.raster (parball.fname = parball.fname,
                                               num.lines = i,
                                               show = F)
      len <- 4 + i * 2
      mse.vect <- c (mse.vect, mse)
      len.vect <- c (len.vect, len)
      if (show)
        {
          plot (len.vect, mse.vect)
        }
    }
  return (list (mse.vect = mse.vect,
                len.vect = len.vect))
}

## Plot MSE vs. Active path length of bilinear and bombing model estimate
##

image.mean.mat.from.test.adaptive <- function (results, num, show = T)
{
  mean.mat <- results $ asn.results $ mean.mat.list [[num]]
  if (show) image (mean.mat)
  return (mean.mat)
}

image.dist.density.raw.from.test.adaptive <- function (results, num, show = T)
{
  dist.density.raw <- results $ asn.results $ dist.density.raw.list [[num]]
  if (show) image (dist.density.raw)
  return (dist.density.raw)
}

image.dist.density.from.test.adaptive <- function (results, num, show = T)
{
  dist.density.raw <- image.dist.density.from.test.adaptive (results, num, F)
  dist.density <- (dist.density.raw - mean (distn.density.raw)) / max (dist.density.raw)
  if (show) image (dist.density)
  return (dist.density)
}

image.pot.field.mat.from.test.adaptive <- function (results, num, show = T)
{
  pot.field.mat <- results $ asn.results $ pot.field.mat.list [[num]]
  if (show) image (pot.field.mat)
  return (pot.field.mat)
}


test.create.x.y.cover.matrices.from.chain.list <- function (image.size)
  {
    load ("~/experiments/2009.10.09/asn_bombing_circle.RData") # results
    chain.list <- results $ asn.results $ chain.list.list [[5]]
    list.circle.matrices <- create.list.circle.matrices (image.size)
    list.giant.mat <- create.x.y.cover.matrices.from.chain.list (chain.list = chain.list,
                                                                 image.size = image.size,
                                                                 list.circle.matrices = list.circle.matrices,
                                                                 start.at = 1)
    wig.vect.x <- apply (list.giant.mat$giant.mat.x, 2, function (v) { sd (v, na.rm = T) * (sum (is.na (v)) / length (v)) })
    wig.vect.y <- apply (list.giant.mat$giant.mat.y, 2, function (v) { sd (v, na.rm = T) * (sum (is.na (v)) / length (v)) })
    wig.mat.x <- matrix (wig.vect.x, ncol = image.size, nrow = image.size)
    wig.mat.y <- matrix (wig.vect.y, ncol = image.size, nrow = image.size)
    return (list (list.giant.mat = list.giant.mat,
                  list.wig.mat = list (wig.mat.x = wig.mat.x,
                    wig.mat.y = wig.mat.y)))
  }

get.list.last.bombing.mat.from.chain.list.list <- function (chain.list.list, trans.actual.bombing.mat)
  {
    width <- dim (trans.actual.bombing.mat) [2]
    height <- dim (trans.actual.bombing.mat) [1]
    list.last.bombing.mat <- lapply (chain.list.list,
                                    function (chain.list)
                                    {
                                      get.matrix.from.bombing (bombing.df = tail (chain.list, 1) [[1]] $ bombing.df,
                                                               width = width,
                                                               height = height)
                                      
                                    })
    return (list.last.bombing.mat)
  }

create.test.bombs <- function (b = NULL, i = NULL)
  {
    fformat <- '~/experiments/2009.12.07/b%d-%d/parball/parball.1.RData'
    helper <- function (b, i)
      {
        fname <- sprintf (fformat, b, i)
        cat (c (fname,'\n'))
        df <- new.bombing (lambda = 1,
                           shape = 1,
                           scale = 0.05,
                           xlim = c(0, 1),
                           ylim = c(0, 1),
                           num.points = b)
        mat <- get.matrix.from.bombing (bombing.df = df,
                                        width = 480,
                                        height = 480)
        images <- list (mat)
        save (images, file = fname)
      }
    
    if (is.null (b) || is.null (i))
      {
        for (b in 1:9) for (i in 1:3)
          {
            helper (b, i)
          }
      }
    else
      {
        helper (b, i)
      }
  }
