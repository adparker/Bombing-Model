## library (roxygen)
#' A spatial predictor using the bombing model.
#'
#' \tabular{ll}{
#' Package: \tab bombing\cr
#' Type: \tab Package\cr
#' Version: \tab 0.2\cr
#' Date: \tab 2010-06-10\cr
#' Title: \tab A collection of bombing functions.\cr
#' Author: \tab Andrew Parker <adparker@@gmail.com>\cr
#' Depends: \tab grid, fields, lattice, ggplot2, doMC, akima\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' A spatial predictor that uses the bombing model, conditioned on
#' line segment observations of boolean random fields.
#'
#' \code{\link{test.turn.crank.n}} is the main function to use.
#'
#' @name bombing-package
#' @aliases bombing
#' @docType package
#' @title A spatial predictor using the bombing model
#' @author Andrew Parker \email{adparker@@gmail.com}
#' @keywords package
#' @seealso \code{\link{test.turn.crank.n}}
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
roxygen()
