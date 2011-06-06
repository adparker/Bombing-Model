##source ('sourceall.r')

do_raster_exp2 <- function ()
  {
    parball.prefix.dir <- '~/experiments/2010.09.23'
    samples.prefix.dir <- parball.prefix.dir
    save.prefix.dir <- parball.prefix.dir ## '/tmp'
    foo <- expand.grid (experiment = c ( ## 'circle_binary',
                          'static1_binary'),
                          ##'static2_binary',
                          ##'static3_binary'),
                        noise = c(
                          'clean',
                          '10_500',
                          '20_240'
                          ))
    
    ##lines <- c ('04', '05', '06', '07', '08', '09', '10')
    lines <- 10
    
    experiments <-
      foreach (experiment = foo$experiment,
               noise = foo$noise) %do% {
                 samples.fnames <-
                   foreach (line = lines) %do% {
                     return (sprintf ('%s/%s_%s/results/samples/raster.lines.000%s.RData',
                                      samples.prefix.dir,
                                      experiment,
                                      noise,
                                      line))
                   }
                 parball.fname <-
                   sprintf ('%s/%s_%s/parball/parball.1.RData',
                            parball.prefix.dir,
                            experiment,
                            noise)
                 save.fname <-
                   sprintf ('%s/raster__%s_%s.RData',
                            save.prefix.dir,
                            experiment,
                            noise)
                 return (list (parball.fname = parball.fname,
                               save.fname = save.fname,
                               samples.fnames = samples.fnames))
               }
    
    for (experiment in experiments) {
      results <- raster_experiment (shape = NULL,
                                    parball.fname = experiment $ parball.fname,
                                    save.fname = experiment $ save.fname,
                                    samples.fnames = experiment $ samples.fnames)
    }
    return ()
  }
