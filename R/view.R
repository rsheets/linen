## Create a "view" of a sheet.  This is a window we can look through.
## It will handle only a offset by a range.
##
## Once Jenny works through some of the spreadsheet taxonomy bits it
## will probably be good to add special treatment of things like
## "header", "footer" and "rownames" so that these can exist outside
## of the view.
##
## TODO: Need, in cellranger, a way of determining that a range falls
## within another range.  This can be done by testing both corners,
## fairly easily.

##' Create worksheet "view"
##' @title Create worksheet view
##' @param sheet A worksheet object, created by
##'   \code{\link{worksheet}} (possibly via the \code{googlesheets} or
##'   \code{rexcel} packages).
##' @param xr A \code{cell_limits} object describing the range of the
##'   worksheet to view.
##' @export
worksheet_view <- function(sheet, xr) {
  .R6_worksheet_view$new(sheet, xr)
}

.R6_worksheet_view <- R6::R6Class(
  "worksheet_view",
  public=list(
    sheet=NULL,
    xr=NULL,
    idx=NULL,

    initialize=function(sheet, xr) {
      assert_inherits(xr, "cell_limits")
      self$sheet <- sheet
      self$xr <- xr
      self$idx <- xr_to_idx(xr)
    }
  ))

##' @export
print.worksheet_view <- function(x, ...) {
  cat(sprintf("<worksheet_view: %d x %d (of %d x %d)>\n",
              x$dim[[1L]], x$dim[[2L]], x$sheet$dim[[1L]], x$sheet$dim[[2L]]))
  print_sheet(x$sheet, x$xr)
}
