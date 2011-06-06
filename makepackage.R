## CD into the ~/Projects/phd/bombing/ directory.
library (roxygen)
system ('rm -rf bombing.roxygen/')
system ('rm -rf bombing/')
package.skeleton ('bombing',
                  code_files = c('bombing-package.R',
                    'bombing.r',
                    'figures.r',
                    'misc.r',
                    'test.r',
                    'utils.r'),
                  force = T)

## $ cp DESCRIPTION bombing/DESCRIPTION
system ('cp DESCRIPTION bombing/DESCRIPTION')
system ('cp NAMESPACE bombing/NAMESPACE')
system ('rm bombing/man/*.Rd')
roxygenize ('bombing')
## cp the *.Rd files for each function you are exporting.
## and make sure that this is reflected in NAMESPACE
system ('cp bombing.roxygen/man/test.turn.crank.n.Rd bombing/man/test.turn.crank.n.Rd')
system ('cp bombing.roxygen/man/new.line.segment.Rd bombing/man/new.line.segment.Rd')
system ('cp bombing.roxygen/man/get.line.segment.observations.from.mat.Rd bombing/man/get.line.segment.observations.from.mat.Rd')
system ('cp bombing.roxygen/man/createCTMCFunction.Rd bombing/man/createCTMCFunction.Rd')
system ('cp bombing.roxygen/man/addCTMCNoise.Rd bombing/man/addCTMCNoise.Rd')
system ('cp bombing.roxygen/man/find.abs.coords.Rd bombing/man/find.abs.coords.Rd')
system ('cp bombing.roxygen/man/is.line.segment.Rd bombing/man/is.line.segment.Rd')

## $ R CMD check bombing
system ('R CMD check bombing')

## $ R CMD build bombing
system ('R CMD build bombing')

## INSTALL IT
## system ('R CMD INSTALL bombing')
$ sudo R CMD INSTALL bombing

## For some reason, the following doesn't work:
## install.packages ('bombing_0.2.tar.gz', repos = NULL)
