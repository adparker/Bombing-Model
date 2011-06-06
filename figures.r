## library(ggplot2)
## source("test.r")
#' @include test.r
roxygen()

## options(error=recover)
## theme_set(theme_bw(base_size = 18))
## quartz()

circle.fname <- "~/experiments/2009.10.09/circle_binary/parball/parball.1.RData"
circle.dir <- "~/experiments/2009.10.09/circle_binary"
asn.bilinear.circle.results.fname <- "~/experiments/2009.10.09/asn_bilinear_circle.RData"
asn.bombing.circle.results.fname <- "~/experiments/2009.10.09/asn_bombing_circle.RData"
raster.bombing.circle.results.fname <- "~/experiments/2009.10.09/circle_20000_alpha_0.5.RData"

bombing1.fname <- "~/experiments/2009.10.09/bombing1_binary/parball/parball.1.RData"
bombing1.dir <- "~/experiments/2009.10.09/bombing1_binary"
asn.bilinear.bombing1.results.fname <- "~/experiments/2009.10.09/asn_bilinear_bombing1.RData"
asn.bombing.bombing1.results.fname <- "~/experiments/2009.10.09/asn_bombing_bombing1.RData"
raster.bombing.bombing1.results.fname <- "~/experiments/2009.10.09/bombing1_20000_alpha_0.5.RData"

static3.fname <- "~/experiments/2009.10.09/static3_binary/parball/parball.1.RData"
static3.dir <- "~/experiments/2009.10.09/static3_binary"
asn.bilinear.static3.results.fname <- "~/experiments/2009.10.09/asn_bilinear_static3.RData"
asn.bombing.static3.results.fname <- "~/experiments/2009.10.09/asn_bombing_static3.RData"
raster.bombing.static3.results.fname <- "~/experiments/2009.10.09/static3_20000_alpha_0.5.RData"

###


static1.fname <- "~/experiments/2009.10.09/static1_binary/parball/parball.1.RData"
static1.dir <- "~/experiments/2009.10.09/static1_binary"
asn.bilinear.static1.results.fname <- "~/experiments/2009.10.09/asn_bilinear_static1.RData"
asn.bombing.static1.results.fname <- "~/experiments/2009.10.09/asn_bombing_static1.RData"
raster.bombing.static1.results.fname <- "~/experiments/2009.10.09/static1_20000_alpha_0.5.RData"

static2.fname <- "~/experiments/2009.10.09/static2_binary/parball/parball.1.RData"
static2.dir <- "~/experiments/2009.10.09/static2_binary"
asn.bilinear.static2.results.fname <- "~/experiments/2009.10.09/asn_bilinear_static2.RData"
asn.bombing.static2.results.fname <- "~/experiments/2009.10.09/asn_bombing_static2.RData"
raster.bombing.static2.results.fname <- "~/experiments/2009.10.09/static2_20000_alpha_0.5.RData"


bombing2.fname <- "~/experiments/2009.10.09/bombing2_binary/parball/parball.1.RData"
bombing2.dir <- "~/experiments/2009.10.09/bombing2_binary"
asn.bilinear.bombing2.results.fname <- "~/experiments/2009.10.09/asn_bilinear_bombing2.RData"
asn.bombing.bombing2.results.fname <- "~/experiments/2009.10.09/asn_bombing_bombing2.RData"
raster.bombing.bombing2.results.fname <- "~/experiments/2009.10.09/bombing2_20000_alpha_0.5.RData"

bombing3.fname <- "~/experiments/2009.10.09/bombing3_binary/parball/parball.1.RData"
bombing3.dir <- "~/experiments/2009.10.09/bombing3_binary"
asn.bilinear.bombing3.results.fname <- "~/experiments/2009.10.09/asn_bilinear_bombing3.RData"
asn.bombing.bombing3.results.fname <- "~/experiments/2009.10.09/asn_bombing_bombing3.RData"
raster.bombing.bombing3.results.fname <- "~/experiments/2009.10.09/bombing3_20000_alpha_0.5.RData"

bombing4.fname <- "~/experiments/2009.10.09/bombing4_binary/parball/parball.1.RData"
bombing4.dir <- "~/experiments/2009.10.09/bombing4_binary"
asn.bilinear.bombing4.results.fname <- "~/experiments/2009.10.09/asn_bilinear_bombing4.RData"
asn.bombing.bombing4.results.fname <- "~/experiments/2009.10.09/asn_bombing_bombing4.RData"
raster.bombing.bombing4.results.fname <- "~/experiments/2009.10.09/bombing4_20000_alpha_0.5.RData"


bombing5.fname <- "~/experiments/2009.10.09/bombing5_binary/parball/parball.1.RData"
bombing5.dir <- "~/experiments/2009.10.09/bombing5_binary"
asn.bilinear.bombing5.results.fname <- "~/experiments/2009.10.09/asn_bilinear_bombing5.RData"
asn.bombing.bombing5.results.fname <- "~/experiments/2009.10.09/asn_bombing_bombing5.RData"
raster.bombing.bombing5.results.fname <- "~/experiments/2009.10.09/bombing5_20000_alpha_0.5.RData"


raw.circle.image.grey <- function ()
  {
    load (circle.fname)
    image (images[[1]], col=grey (0:1))
  }

raster.of.circle <- function ()
  {
    image.raster.scan.paths (circle.fname, 5)
  }

bilinear.interp.of.circle.from.raster <- function(i=5)
  {
    foo <- t(image.bilinear.from.raster (circle.fname, i, F))
    ##image (t(foo), col = grey (seq(0,1,length.out=256)))
    ncl <- ncol (foo)
    nrw <- nrow (foo)
    df <- expand.grid (x = 1:ncl, y = 1:nrw)
    df$z <- as.vector (foo)
    p <- ggplot (df, aes(x=x, y=y))
    p + geom_tile (aes (fill=z)) + labs (fill = "") + opts (title = "Field Estimate Using Bilinear Interpolation") + scale_fill_gradient (low = 'black', high = 'white')

  }

circle.plot.mse.vs.num.raster.lines <- function ()
  {
    load(paste(circle.dir,'results','akima.li.df',sep='/'))
    qplot (raster.lines * 2 + 1,
           akima.li.mse,
           data = obj,
           geom = c ('point','line'),
           #size = 5,
           main = 'Interpolation of Circle Field',
           ylab = 'Mean Squared Error',
           xlab = 'Number of Raster Lines') 
  }

raw.bombing1.image.grey <- function ()
  {
    load (bombing1.fname)
    image (t(images[[1]]), col=grey(0:1))
  }

raster.of.bombing1 <- function()
  {
    image.raster.scan.paths (bombing1.fname, 5)
  }

bombing1.plot.mse.vs.num.raster.lines <- function ()
  {
    load (paste (bombing1.dir, 'results', 'akima.li.df', sep = '/'))
    qplot (raster.lines * 2 + 1,
           akima.li.mse,
           data = obj,
           geom = c ('point', 'line'),
           main = 'Interpolation of Synthetic Field',
           ylab = 'Mean Squared Error',
           xlab = 'Number of Raster Lines')
  }

raw.static3.image.grey <- function ()
  {
    load (static3.fname)
    image (t(images[[1]]), col=grey (0:1))
  }
raster.of.static3 <- function (i=5)
  {
    image.raster.scan.paths (static3.fname, 5)
  }

static3.plot.mse.vs.num.raster.lines <- function ()
  {
    load (paste (static3.dir, 'results', 'akima.li.df', sep = '/'))
    qplot (raster.lines * 2 + 1,
           akima.li.mse,
           data = obj,
           geom = c ('point', 'line'),
           main = 'Interpolation of Natural Field',
           ylab = 'Mean Squared Error',
           xlab = 'Number of Raster Lines')
  }

################## Bilinear driven active paths ############################
asn_bilinear_circle_active_path_example <- function (i=0)
  {
    
    load ("~/experiments/2009.10.09/asn_bilinear_circle.RData") ## results
    ## Draw some active paths on top of true image.
    ls.path.list <- results $ asn.results $ ls.path.list
    actual.bombing.mat <- results $ actual.bombing.mat
    if (i == 0) { i <- length (ls.path.list) }
    image.active.paths (ls.path.list [1:i], actual.bombing.mat)
  }

asn_bilinear_circle_dist_density_example <- function (i)
  {
    load ("~/experiments/2009.10.09/asn_bilinear_circle.RData") ## results
    dist.density.raw.list <- results $ asn.results $dist.density.raw.list
    ##image (t (dist.density.raw.list [[i]]), col = grey (seq (0, 1, length.out = 256)))
    ddr <- dist.density.raw.list[[i]]
    ncl <- ncol (ddr)
    nrw <- nrow (ddr)
    df <- expand.grid (x = 1:ncl, y = 1:nrw)
    df$z <- as.vector (ddr) / ncl
    p <- ggplot (df, aes(x=x, y=y))
    p + geom_tile (aes (fill=z)) + labs (fill = "Distance") + opts (title = "Distance to Nearest Observation")
  }

asn_bilinear_circle_mean <- function (i = 0)
  {
    load ("~/experiments/2009.10.09/asn_bilinear_circle.RData") ## results
    mean.mat.list <- results $ asn.results $ mean.mat.list
    mean.mat <- if (i > 0) { mean.mat.list [[i]] } else { adp.list.last (mean.mat.list) }
    ncl <- ncol (mean.mat)
    nrw <- nrow (mean.mat)
    df <- expand.grid (x = 1:ncl, y = 1:nrw)
    df$z <- as.vector (mean.mat)
    p <- ggplot (df, aes(x=x, y=y))
    p + geom_tile (aes (fill=z)) + labs (fill = "") + opts (title = "Field Estimate Using Bilinear Interpolation") + scale_fill_gradient (low = 'black', high = 'white')
  }

asn_bilinear_circle_pot_field <- function (i)
  {
    load ("~/experiments/2009.10.09/asn_bilinear_circle.RData") ## results
    pot.field.mat.list <- results $ asn.results $ pot.field.mat.list
    pot.field.mat <- pot.field.mat.list [[i]]
    ncl <- ncol (pot.field.mat)
    nrw <- nrow (pot.field.mat)
    df <- expand.grid (x = 1:ncl, y = 1:nrw)
    df$z <-  as.vector (pot.field.mat)
    p <- ggplot (df, aes(x=x, y=y))
    p + geom_tile (aes (fill=z)) + labs (fill = "Reward") + opts (title = "Field Data Reward") + scale_fill_gradient (low = 'blue', high = 'red')

  }

plot_mse_vs_length_from_bilinear_raster <- function (raster.dir)
  {
    load(paste(raster.dir,'results','akima.li.df',sep='/')) # obj
    df <- 
        subset (
            transform (
                obj, 
                length = raster.lines * 2 + 3,
                mse = akima.li.mse, 
                variable = 'Bilinear : Raster'), 
            select = c('mse', 'length', 'variable'))
    return (df)
  }

## plot.mse.vs.length.of.bombing.from.test.adaptive (...)
plot_mse_vs_length_from_bilinear_asn <-
  function (asn.bilinear.results.fname)
  {
    res <- plot_mse_vs_length_asn (asn.results.fname = asn.bilinear.results.fname)
    return (cbind (res, variable = 'Bilinear : Active Paths'))
  }


plot_mse_vs_length_from_bombing_asn <-
  function (asn.bombing.results.fname,
            variable = "Bombing : Active Paths",
            f.tweak = iden)
  {
    res <- 
        plot_mse_vs_length_asn (
            asn.results.fname = asn.bombing.results.fname,
            f.tweak = f.tweak)
    return (cbind (res, variable = variable))
  }

plot_mse_vs_length_asn <-
  function (asn.results.fname,
            f.tweak = iden)
  {
    load (asn.results.fname) ## results
    mean.mat.list <- 
        if ('asn.results' %in% names (results))
        {
          results $ asn.results $ mean.mat.list
        }
        else if ('mean.mat.list' %in% names (results))
        {
          results $ mean.mat.list
        }
        else
        {
          stop ("I don't know how to get mean.mat.list.")
        }
    ls.path.list <-
        if ('asn.results' %in% names (results))
        {
          results $ asn.results $ ls.path.list
        }
        else if ('ls.path.list' %in% names (results))
        {
          results $ ls.path.list
        }
        else
        {
          stop ("I don't know how to get ls.path.list.")
        }
    res <- 
        plot.mse.vs.length.of.bombing.from.test.adaptive (
            mean.mat.list = mean.mat.list,
            ls.path.list = ls.path.list,
            actual.bombing.mat = results $ actual.bombing.mat,
            show = F,
            f.tweak = f.tweak)
    return (data.frame (mse = res$mses, length = res$lengths))
  }

plot_mse_mode_vs_length_from_bombing_asn <- function (asn.results.fname)
  {
    load (asn.results.fname) ## results
    mat.list <- get.list.last.bombing.mat.from.chain.list.list (chain.list.list = results $ asn.results $ chain.list.list,
                                                                trans.actual.bombing.mat = t (results $ actual.bombing.mat))
    res <- plot.mse.vs.length.of.bombing.from.test.adaptive (mean.mat.list = mat.list,
                                                             ls.path.list = results $ asn.results $ ls.path.list,
                                                             actual.bombing.mat = results $ actual.bombing.mat,
                                                             show = F)
    df <- data.frame ( mse = res$mses, length = res$lengths)
    return (cbind (df, variable = 'Bombing : Active Paths : Mode'))
  }
  
############# To Create Graphs
## plot_rmse_vs_length_from_bilinear_compare_asn_to_raster (circle.dir, asn.bilinear.circle.results.fname)
## ggsave("~/IPSN images/plot_rmse_vs_length_from_bilinear_compare_asn_to_raster_of_circle.pdf", height = 7, width = 11)
##
## plot_rmse_vs_length_from_bilinear_compare_asn_to_raster (bombing1.dir, asn.bilinear.bombing1.results.fname)
## ggsave("~/IPSN images/plot_rmse_vs_length_from_bilinear_compare_asn_to_raster_of_bombing1.pdf")
##
## plot_rmse_vs_length_from_bilinear_compare_asn_to_raster (static3.dir, asn.bilinear.static3.results.fname)
## ggsave("~/IPSN images/plot_rmse_vs_length_from_bilinear_compare_asn_to_raster_of_static3.pdf")

plot_rmse_vs_length_from_bilinear_compare_asn_to_raster <-
  function (raster.dir,
            asn.bilinear.results.fname)
  {
    df.raster <- plot_mse_vs_length_from_bilinear_raster (raster.dir)
    df.asn <- plot_mse_vs_length_from_bilinear_asn (asn.bombing.results.fname = asn.bilinear.results.fname)
    df <- rbind (df.raster, df.asn)
    p <- qplot(length,
               sqrt(mse),
               data=df,
               ##geom = c('point','line'),
               colour = variable,
               shape =  variable,
               #ylim=c(0,0.25),
               )
    print (p
           + geom_point (size = 4)
           + geom_line (size = 1)
           + opts (title = "Field Estimate Comparison: Raster vs. Active Path Observations")
           + labs (colour = "Observation Strategy",
                   shape = "Observation Strategy",
                   x = 'Total Path Length',
                   y = 'Root Mean Square Error'))
      }

############# To Create Graphs
## plot_rmse_vs_length_compare_asn_to_raster (bombing1.dir, asn.bilinear.bombing1.results.fname, asn.bombing.bombing1.results.fname)
## ggsave("~/IPSN images/plot_rmse_vs_length_from_bilinear_compare_asn_to_raster_of_circle.pdf", height = 7, width = 11)

plot_rmse_vs_length_compare_asn_to_raster <- 
    function (
        experiment.title,
        raster.dir = NULL,
        asn.bilinear.results.fname = NULL,
        asn.bombing.results.fname = NULL,
        asn.bombing.prc.results.fname = NULL,
        bilinear.from.bombing.fname = NULL,
        bilinear.from.bombing.prc.fname = NULL,
        round.results.fname = NULL,
        asn.bombing.raster.results.fname = NULL,
              ...)
{
  df <- data.frame()
  if (!is.null (raster.dir))
    {
      df.raster <- 
        plot_mse_vs_length_from_bilinear_raster (raster.dir)
      df <- rbind (df, df.raster)
    }
  if (!is.null (asn.bilinear.results.fname))
    {
      df.bilinear.asn <- 
        plot_mse_vs_length_from_bilinear_asn (
            asn.bilinear.results.fname = asn.bilinear.results.fname)
      df <- rbind (df, df.bilinear.asn)
    }
  if (!is.null (asn.bombing.results.fname))
    {
      df.bombing.asn <- 
        plot_mse_vs_length_from_bombing_asn (
           asn.bombing.results.fname = asn.bombing.results.fname)
      df <- rbind (df, df.bombing.asn)
    }
  #df.bombing.mode.asn <- 
  #    plot_mse_mode_vs_length_from_bombing_asn (
  #        asn.results.fname = asn.bombing.results.fname)
  if (!is.null (asn.bombing.raster.results.fname))
    {
      df.bombing.raster <- {
        df.temp <- 
          plot_mse_vs_length_from_bombing_asn (
                                               asn.bombing.results.fname = asn.bombing.raster.results.fname,
                                               variable = "Bombing : Raster")
        ## The minus 1 is to fix an overcount.
        transform (
                   df.temp, 
                   length = c(length[1], diff (length)) - 1)
      }
      df <- rbind (df, df.bombing.raster)
    }
  #df <- 
  #    rbind (
  #        df.raster, 
  #        df.bilinear.asn, 
  #       df.bombing.asn, 
  #        #df.bombing.mode.asn,
  #        df.bombing.raster)
  if (!is.null(bilinear.from.bombing.fname))
  {
    load (bilinear.from.bombing.fname) ## results
    df.bilinear.from.bombing.lines <- 
        plot.mse.vs.length.of.bilinear.from.test.adaptive (results = results,
                                                           variable = 'Bilinear from Bombing : Active Paths')
    df <- 
        rbind (df, df.bilinear.from.bombing.lines)
  }
  if (!is.null(bilinear.from.bombing.prc.fname))
  {
    load (bilinear.from.bombing.prc.fname) ## results
    df.bilinear.from.bombing.prc.lines <- 
        plot.mse.vs.length.of.bilinear.from.test.adaptive (results = results,
                                                           variable = 'Bilinear from Bombing : PRC Active Paths')
    df <- 
        rbind (df, df.bilinear.from.bombing.prc.lines)
  }
  if (!is.null (asn.bombing.prc.results.fname))
  {
    df.bombing.prc.asn <- 
        plot_mse_vs_length_from_bombing_asn (
            asn.bombing.prc.results.fname,
            variable = 'Bombing : PRC Active Paths')
    df <- 
        rbind (df, df.bombing.prc.asn)
  }
  if (!is.null (round.results.fname))
  {
    df.bombing.round.asn <- 
        plot_mse_vs_length_from_bombing_asn (
            round.results.fname,
            variable = "Rounded Images",
            f.tweak = round)
    df <- 
        rbind (df, df.bombing.round.asn)
  }
  
  p <- 
      qplot(
          length,
          sqrt(mse),
          data=df,
          ##geom = c('point','line'),
          colour = variable,
          shape =  variable,
      ##ylim=c(0,0.25),
      )
  print (p
          + geom_point (size = 4)
          + geom_line (size = 1)
          + opts (title = paste ("Sampling and Estimation Strategies:", experiment.title))
          + labs (colour = "Observation Strategy",
              shape = "Observation Strategy",
              x = 'Total Path Length',
              y = 'Root Mean Square Error')
         # + scale_y_log2()
         )
  return (df)
}


asn_bilinear_active_path_example <- function (asn.bilinear.results.fname, i=0)
  {
    
    load (asn.bilinear.results.fname) ## results
    ## Draw some active paths on top of true image.
    ls.path.list <- results $ asn.results $ ls.path.list
    actual.bombing.mat <- results $ actual.bombing.mat
    if (i == 0) { i <- length (ls.path.list) }
    image.active.paths (ls.path.list [1:i], actual.bombing.mat)
  }

asn_bilinear_circle_dist_density_example <- function (asn.bilinear.results.fname, i)
  {
    load (asn.bilinear.results.fname) ## results
    dist.density.raw.list <- results $ asn.results $dist.density.raw.list
    ##image (t (dist.density.raw.list [[i]]), col = grey (seq (0, 1, length.out = 256)))
    ddr <- dist.density.raw.list[[i]]
    ncl <- ncol (ddr)
    nrw <- nrow (ddr)
    df <- expand.grid (x = 1:ncl, y = 1:nrw)
    df$z <- as.vector (ddr) / ncl
    p <- ggplot (df, aes(x=x, y=y))
    p + geom_tile (aes (fill=z)) + labs (fill = "Distance") + opts (title = "Distance to Nearest Observation")
  }

asn_bilinear_mean <- function (asn.bilinear.results.fname, i = 0)
  {
    load (asn.bilinear.results.fname) ## results
    mean.mat.list <- results $ asn.results $ mean.mat.list
    mean.mat <- if (i > 0) { mean.mat.list [[i]] } else { adp.list.last (mean.mat.list) }
    ncl <- ncol (mean.mat)
    nrw <- nrow (mean.mat)
    df <- expand.grid (x = 1:ncl, y = 1:nrw)
    df$z <- as.vector (mean.mat)
    p <- ggplot (df, aes(x=x, y=y))
    p + geom_tile (aes (fill=z)) + labs (fill = "") + opts (title = "Field Estimate Using Bilinear Interpolation") + scale_fill_gradient (low = 'black', high = 'white')
  }

bilinear.interp.from.raster <- function(image.fname, i=5)
  {
    foo <- t(image.bilinear.from.raster (image.fname, i, F))
    ##image (t(foo), col = grey (seq(0,1,length.out=256)))
    ncl <- ncol (foo)
    nrw <- nrow (foo)
    df <- expand.grid (x = 1:ncl, y = 1:nrw)
    df$z <- as.vector (foo)
    p <- ggplot (df, aes(x=x, y=y))
    p + geom_tile (aes (fill=z)) + labs (fill = "") + opts (title = "Field Estimate Using Bilinear Interpolation") + scale_fill_gradient (low = 'black', high = 'white')

  }


## > foo = image.final.bombing.from.raster (raster.bombing.bombing1.results.fname)
## foo
## ggsave("~/IPSN images/image.final.bombing.from.raster.of.bombing1.pdf")

image.final.bombing.from.raster <- function (raster.results.fname)
  {
    load (raster.results.fname) ## results
    results <- NULL
    try (assign("results",circle), silent = T)
    if (is.null(results)) try (assign ("results", bombing1), silent = T)
    if (is.null(results)) try (assign ("results", static3), silent = T)
    stopifnot (!is.null(results))
    foo <- results $ mean.mat
    ncl <- ncol (foo)
    nrw <- nrow (foo)
    df <- expand.grid (x = 1:ncl, y = 1:nrw)
    df$z <- as.vector (foo)
    p <- ggplot (df, aes(x=x, y=y))
    p + geom_tile (aes (fill=z)) + labs (fill = "") + opts (title = "Field Estimate Using the Bombing Model") + scale_fill_gradient (low = 'black', high = 'white')
    
  }


pretty.plot.mse.vs.steps.from.bombing.of.raster <- function (raster.results.fname)
  {
    load (raster.results.fname) ## results
    results <- NULL
    try (assign("results",circle), silent = T)
    if (is.null(results)) try (assign ("results", bombing1), silent = T)
    if (is.null(results)) try (assign ("results", static3), silent = T)
    stopifnot (!is.null(results))
    foo = plot.mse.vs.steps.from.test.turn.crank.n (results, step.by = 5)
    return (foo)
  }

asdfasdf <- function (foo, bilinear.raster.mse) {
  p <- qplot (foo$end.index.vect,
              sqrt (foo$mse.vect),
              geom = c ('point', 'line'),
              #ylim = c (0, 0.27),
              main = 'Bombing Model Estimate Given Raster Observations',
              ylab = 'Root Mean Square Error',
              xlab = 'Bombing Model Simulation Steps')
  return (p
          + geom_point (size = 4)
          + geom_line (size = 1)
          + geom_hline (yintercept = bilinear.raster.mse, color = "red", size = 2)
          + geom_text(aes(x = 20000,
                          y = bilinear.raster.mse + 0.0025,
                          label = paste ('RMSE of Bilinear Estimate =',  bilinear.raster.mse)),
                          size = 5))
}
                

plot.mse.vs.length.of.bilinear.from.test.adaptive <- function (results, show = F, variable = 'Bilinear from Bombing : Active Paths')
{
  ls.path.list <- results $ asn.results $ ls.path.list
  ls.path.lengths <-get.lengths.from.ls.path.list (results $ asn.results $ ls.path.list)
  ##ls.path <- get.ls.paths (results $ asn.results $ ls.path.list)
  actual.bombing.mat <- results $ actual.bombing.mat
  mse.vect <- vector()
  for (i in 1:length (ls.path.list))
    {
      cat (paste ("loop: ", i, '/', length (ls.path.list), '\n'))
      mse <- get.mse.from.bilinear.with.active.paths (ls.path.list [1:i], actual.bombing.mat, show)
      mse.vect <- c (mse.vect, mse)
    }
  return (data.frame (mse = mse.vect, length = ls.path.lengths, variable = variable))
}

get.asn.fname <- function (b, i, fit = c('bilinear', 'bombing'), use.prc)
  {
    fit <- match.arg (fit)
    use.prc.str <- if (use.prc) { '_prc' } else { '' }
    # asn_bombing_prc_b1-1.RData
    fname <- paste ('~/experiments/2009.12.07/asn_', fit, use.prc.str, '_b', b, '-', i, '.RData', sep = '')
    return (fname)
  }

get.raster.dirname <- function (b, i)
  {
    paste ('~/experiments/2009.12.07/b', b, '-', i, sep = '')
  }

get.raster.fname <- function (b, i)
{
  ## raster__b1-3R.Data
  paste ('~/experiments/2009.12.07/raster__b', b, '-', i, 'R.Data', sep = '')
}

plot_wrapper <- function (b, i, fname = NULL, use.bilinear = T, use.bombing = F, use.prc = F, use.bomb.raster = F, do_ggsave = T)
  {
    df <-
      plot_rmse_vs_length_compare_asn_to_raster (experiment.title = paste ('Bombs: ', b, 'Config:', i),
                                                 raster.dir = get.raster.dirname (b, i),
                                                 asn.bilinear.results.fname = if (use.bilinear) { get.asn.fname (b, i, 'bilinear', use.prc = F)}, 
                                                 asn.bombing.results.fname =  if (use.bombing)  { get.asn.fname (b, i, 'bombing', use.prc = F) },
                                                 asn.bombing.prc.results.fname = if (use.prc)   { get.asn.fname (b, i, 'bombing', use.prc = T) },
                                                 ## bilinear.from.bombing.fname = get.asn.fname (b, i, 'bombing', use.prc = F),
                                                 ## bilinear.from.bombing.prc.fname = get.asn.fname (b, i, 'bombing', use.prc = T)                    
                                                 ## round.results.fname = get.asn.fname (b, i, 'bombing', use.prc = T),
                                                 asn.bombing.raster.results.fname = if (use.bomb.raster) { get.raster.fname (b, i) }
                                                 )
    if (do_ggsave)
      {
        fname <- 
          if (is.null (fname)) 
            { 
              paste('~/Desktop/b', b, '-', i, '.pdf', sep = '') 
            } 
          else 
            { 
              fname 
            }
        ggsave (fname, height = 7, width = 11)
      }
    return (df)
  }

fix.mean.mat <-
  function (asn.results.fname)
  {
    redo.mean.mat.width <- 480
    load (asn.results.fname) ## results
    results $ asn.results $ mean.mat.list <-
      foreach (chain.list = results $ asn.results $ chain.list.list) %do%
    {
      mean.mat <- get.mean.matrix.from.chain.list (chain.list = chain.list,
                                                   width = redo.mean.mat.width,
                                                   height = redo.mean.mat.width,
                                                   start.at = round (length (chain.list) / 2));
    }
    save (results, file = asn.results.fname)
  }
    
