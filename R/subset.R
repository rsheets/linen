##' Subset a rexcelr sheet based on a rectangular selection based
##' on a cellranger \code{cell_limits} object.
##'
##' @title Subset a excel sheet
##' @param dat A excel sheet
##' @param xr A \code{cell_limits} object indicating the extent to
##'   extract.
##' @export
jailbreak_subset <- function(dat, xr) {
  if (!inherits(xr, "cell_limits")) {
    stop("xr must be a cell_limits object")
  }

  x <- dat
  x$dim <- dim(xr)
  i <- (dat$pos[, 1L] >= xr$ul[[1L]] &
        dat$pos[, 1L] <= xr$lr[[1L]] &
        dat$pos[, 2L] >= xr$ul[[2L]] &
        dat$pos[, 2L] <= xr$lr[[2L]])

  x$cells <- lapply(dat$cells, function(x) x[i])

  ## Need to recompute the value here; rows and columns have both
  ## possibly shifted.
  x$pos <- dat$pos[i, , drop=FALSE] - rep(xr$ul - 1L, each=sum(i))

  ## The lookups need recomputing.  This is potentially quite nasty to
  ## do.
  j <- which(i)
  ir <- xr$ul[[1L]] : xr$lr[[1L]]
  ic <- xr$ul[[2L]] : xr$lr[[2L]]
  x$lookup <- dat$lookup[ir, ic]
  x$lookup2 <- dat$lookup2[ir, ic]
  x$lookup[] <- match(x$lookup, j)
  x$lookup2[] <- match(abs(x$lookup2), j) * sign(x$lookup2)

  ## Merged comes next.
  f <- function(el) {
    ok <- all(el$ul >= xr$ul & el$lr <= xr$lr)
    if (ok) {
      el$ul <- el$ul - xr$ul + c(1L, 1L)
      el$lr <- el$lr - xr$ul + c(1L, 1L)
      el
    } else {
      NULL
    }
  }
  merged <- lapply(x$merged, f)
  x$merged <- merged[!vlapply(merged, is.null)]

  x
}
