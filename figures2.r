lazy.create.list.circle.matrices <- function (mean.field.width)
  {
    stopifnot (is.numeric (mean.field.width))
    fname <- paste ('/tmp/circle.matrices.', mean.field.width, '.RData', sep = '')
    ret <- NULL
    try(
        {
          ret <- safe.load (fname)
        },
        TRUE
        )
    if (is.null (ret))
      {
        ret <- create.list.circle.matrices (mean.field.width)
        save (ret, file = fname)
      }
    return (ret)
  }


## RETURNS a list (p1, p2), with p1 being the PRC field, and p2 being the center field.
## EXAMPLE:
## l <- create.prc.and.center.fields ('~/experiments/2009.12.07/asn_bombing_prc_b3-1.RData', 1)
create.prc.df <- function (fname, n, mean.field.height = 100, mean.field.width = 100, dnorm.sd = 0.447, chain.frac = 0.5)
  {
    chain.list <-
      {
        results <- safe.load (fname)
        results$asn.results$chain.list.list [[n]]
      }

    prc.mat <-
      {
        list.giant.mat <-
          {
            list.circle.matrices <- create.list.circle.matrices (mean.field.width)
            create.x.y.cover.matrices.from.chain.list (chain.list = chain.list,
                                                       image.size = mean.field.width,
                                                       list.circle.matrices = list.circle.matrices,
                                                       start.at = ceiling (chain.frac * length (chain.list)))
            ##start.at = 1)
          }
        create.prcomp.vect.mat (list.giant.mat $ giant.mat.x,
                                list.giant.mat $ giant.mat.y)
      }
    rm (list.giant.mat);                                           
    rm (list.circle.matrices)
    gc()

    mean.mat <-
      get.mean.matrix.from.chain.list (chain.list = chain.list,
                                       width = mean.field.width,
                                       height = mean.field.height,
                                       start.at = ceiling (chain.frac * length (chain.list)))
    ##start.at = 1)

    pot.field.mat <-
      {
        ## mean.thresh <- otsu (mean.mat) $ threshold
        mean.thresh <- 0.5
        pot.field.mat.raw <- dnorm ((mean.mat - mean.thresh),
                                    sd = dnorm.sd)
        pot.field.mat.centered <- (pot.field.mat.raw - mean (pot.field.mat.raw))
        sdev.scale.mat <- t (matrix (prc.mat ['sdev', ],
                                     ncol = mean.field.width,
                                     nrow = mean.field.width)) / mean (prc.mat ['sdev', ], na.rm = T)
        sdev.scale.mat [ is.na (sdev.scale.mat) ] <- 0
        sdev.scale.mat * pot.field.mat.centered 
      }

    ## Create PRC field:
    ## prc.mat (created above)
    ## pot.field.mat (
    prc.df <- transform (get.prc.df (prc.mat),
                         pot.field = as.vector (t (pot.field.mat)))
    return (prc.df)
  }

ggplot.prc.df <- function (prc.df, min.cover = 0.25, max.cover = 0.75, rm.filtered = F, mean.field.width = 1, xlimit = NULL, ylimit = NULL, expand = c (0,0))
  {
    mask.out <- (prc.df$cover.prob < min.cover) | (prc.df$cover.prob > max.cover)
    if (!rm.filtered)
      {
        prc.df [ mask.out,
                c ('rot.x', 'rot.y', 'unit.rel.center.x', 'unit.rel.center.y') ]  <- NA
      }
    else
      {
        prc.df <- prc.df [!mask.out, ] 
      }
    prc.df <- transform (prc.df,
                         x = x / mean.field.width,
                         y = y / mean.field.width,
                         rot.x = rot.x / mean.field.width,
                         rot.y = rot.y / mean.field.width,
                         center.x = center.x / mean.field.width,
                         center.y = center.y / mean.field.width,
                         sdev = sdev / mean.field.width,
                         rel.center.x = rel.center.x / mean.field.width,
                         rel.center.y = rel.center.y / mean.field.width,
                         center.dist = center.dist / mean.field.width,
                         unit.rel.center.x = unit.rel.center.x / mean.field.width,
                         unit.rel.center.y = unit.rel.center.y / mean.field.width)
                                                  
    sdev.scale <- 0.01
    p <- ggplot (prc.df, aes (x = x, y = y))
    if (!is.null (xlimit))
      { # http://had.co.nz/ggplot2/scale_continuous.html
        p <- p + scale_x_continuous (limits = xlimit, expand = expand)
      }
    if (!is.null (ylimit))
      {
        p <- p + scale_y_continuous (limits = ylimit, expand = expand)
      }
    p1 <- p + geom_segment (aes (xend = x + rot.x * pot.field * 10,
                                 yend = y + rot.y * pot.field * 10),
                            arrow = arrow(length=unit(0.1,"cm")))

    ## Create rel center field
    p2 <- p + geom_segment (aes (xend = x + unit.rel.center.x,
                                 yend = y + unit.rel.center.y))

    return (list (p1 = p1, p2 = p2))
  }
