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
##' @param filter filter
##' @param xr A \code{cell_limits} object describing the range of the
##'   worksheet to view.
##' @param header Header data
##' @param data Additional data fields, as a list.  This can be
##'   anything but is passed by value not by reference so clever
##'   things won't work well here.  Currently I'm using this in
##'   \code{jailbreakr::split_metadata} to hold a reference to cells
##'   that contain metadata about the sheet.
##' @export
worksheet_view <- function(sheet, xr = NULL, filter = NULL, header = NULL,
                           data = NULL) {
  R6_worksheet_view$new(sheet, xr, filter, header, data)
}

R6_worksheet_view <- R6::R6Class(
  "worksheet_view",
  public=list(
    sheet = NULL,
    xr = NULL,
    header = NULL,
    data = NULL,

    idx = NULL,
    dim = NULL,

    initialize = function(sheet, xr, filter, header, data) {
      assert_inherits(xr, "cell_limits")
      ## TODO: validation here.
      ##
      ## * headers; should be allowed to be:
      ##   - character vector
      ##   - integer (or integer range)
      ##   - cell_limits
      self$sheet <- sheet
      self$xr <- xr
      self$header <- header
      self$data <- data
      self$idx <- xr_to_idx(xr)
      self$dim <- lengths(self$idx, FALSE)
    },

    values = function() {
      to_values(self)
    },

    table = function(col_names = TRUE, ...) {
      worksheet_view_to_table(self, col_names, ...)
    }
  ),

  ## It's not clear here which of these we want to pass through as
  ## active binding methods.  I'm avoiding writing these out as actual
  ## data because that seems wasteful, but we can always switch out to
  ## doing that if need be.
  active = list(
    lookup = function() {
      self$sheet$lookup[self$idx$r, self$idx$c]
    },
    lookup2 = function() {
      self$sheet$lookup2[self$idx$r, self$idx$c]
    },
    cells = function() {
      self$sheet$cells
    },
    merged = function() {
      self$sheet$merged
    }
  ))

##' @export
print.worksheet_view <- function(x, ...) {
  cat(sprintf("<worksheet_view: %d x %d (of %d x %d)>\n",
              x$dim[[1L]], x$dim[[2L]], x$sheet$dim[[1L]], x$sheet$dim[[2L]]))
  if (!is.null(x$header)) {
    cat("(with headers)\n")
  }
  if (!is.null(x$data)) {
    cat(sprintf("(with data: %s)\n", paste(names(x$data), collapse=", ")))
  }

  ## TODO: this is both wasteful and duplicates code elsewhere...
  fg <- bg <- NULL
  if (crayon::has_color()) {
    style <- style_lookup(x, fg="font/color", bg="fill/fg")
    if (!is.null(style)) {
      fg <- style$fg[x$cells$style]
      bg <- style$bg[x$cells$style]
    }
  }

  print_sheet(x$sheet, x$xr, bg, fg)
}
