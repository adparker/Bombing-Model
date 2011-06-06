## For Raster Bilinear, gather together all of the data, and plot everything out.
## ~/2009.12.07/b2-1/results/akima.li/akima.li.df
##
source ('sourceall.r')


############# raster - linear ###########
## for (b in 1:9) for (i in 1:3)
## {
##   fname <- paste ('~/experiments/2009.12.07/', 'b', b, '-', i, '/results/akima.li.df', sep = '')
##   load (fname)
##   temp.df <- subset (transform (obj,
##                                 length = raster.lines * 2 - 1 + 4,
##                                 mse = akima.li.mse,
##                                 experiment = 'raster-linear'),
##                      select = c('length', 'mse', 'experiment'))
##   df <- rbind (df, temp.df)
## }

########### adaptive+bilinear, adaptive+bombing, adaptive+prc, raster+bombing, raster+bilinear#########
df <- data.frame()
for (b in 1:9) for (i in 1:3)
  {
    fname <- paste ('~/experiments/2009.12.07/asn_bilinear_b',b,'-',i,'.RData', sep = '')
    load (fname)
    temp.df <- plot_wrapper (b, i, use.bilinear = T, use.bombing = T, use.prc = F, use.bomb.raster = T, do_ggsave = F)
    df <- rbind (df, temp.df)
  }
save (df, file = '~/experiments/2009.12.07/summary_df2.RData')
## I need to put max y of 0.05.
df2 <- transform (df, RMSE = sqrt(mse))

c <- ggplot (df2 [ df$variable == 'Bilinear : Raster' | df$variable == 'Bombing : Active Paths', ], aes (y = RMSE, x = length, colour = variable))
c + geom_point(alpha = 0.3) +
  stat_smooth(se=F, size = 1, method = 'loess') ## +
  #scale_y_continuous (limits = c(0, 0.4)) +
  ##scale_y_continuous (formatter = 'percent') +
  ## scale_x_continuous (limits = c(4, 23), )
