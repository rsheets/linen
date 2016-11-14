##' Read a worksheet (from Excel or googlesheets) the same way as
##' readxl, but slower.  Assumes a well behaved table of data.
##'
##' @param dat A linen \code{\link{worksheet}} object
##' @param col_names TRUE (the default) indicating we should use the
##'   first row as column names, FALSE, indicating we should generate
##'   names (X1, X2, ..., Xn) or a character vector of names to apply.
##' @param col_types Either NULL (the default) indicating we should
##'   guess the column types or a vector of column types (values must
##'   be "blank", "numeric", "date" or "text").
##' @param na Values indicating missing values (if different from
##'   blank).  Not yet used.
##' @param skip Number of rows to skip.
##' @export
worksheet_to_table <- function(dat, col_names = TRUE, col_types = NULL,
                               na = "", skip = 0) {
  if (!identical(na, "")) {
    .NotYetUsed("na") # TODO -- passed in to the type inference stuff
  }

  tmp <- table_col_names(dat, col_names, skip)
  col_names <- tmp$col_names
  skip <- tmp$skip
  keep <- tmp$keep
  xr <- cellranger::cell_limits(c(skip + 1, 1), dat$dim)
  v <- worksheet_view(dat, xr)
  ret <- v$table(col_names = col_names, col_types = col_types, na = na)
  if (all(keep)) ret else ret[, keep, drop = FALSE]
}

worksheet_view_to_table <- function(dat, col_names = TRUE, ...) {
  col_names <- table_col_names(dat, col_names)$col_names
  view_data_to_table(dat, col_names, ...)
}

view_data_to_table <- function(dat, col_names, col_types = NULL, na = "") {
  if (!identical(na, "")) {
    .NotYetUsed("na") # TODO -- passed in to the type inference stuff
  }

  stopifnot(inherits(dat, "worksheet_view"))
  if (is.null(col_names)) {
    col_names <- sprintf("X%d", seq_len(dat$dim[[2]]))
  } else {
    stopifnot(length(col_names) == dat$dim[[2]])
  }

  lookup <- dat$lookup

  cell_types <- c(blank = 0, bool = 1, date = 2, number = 3, text = 4)
  type <- array(unname(cell_types[dat$cells$type[c(lookup)]]), dim(lookup))
  type <- apply(rbind(cell_types[["blank"]], type), 2, max, na.rm = TRUE)
  type <- names(cell_types)[match(type, cell_types)]

  ret <- data.frame(array(NA, dim(lookup)))

  tr <- list(number = as.numeric,
             bool = as.numeric,
             date = unlist_times,
             text = function(x) vapply(x, as.character, character(1L)),
             blank = as.numeric)
  for (i in seq_along(type)) {
    t <- type[[i]]
    j <- lookup[, i]
    k <- !is.na(j)
    if (t == "date") {
      ## FFS. http://i.giphy.com/arz9UYo8bCo4E.gif
      ret[[i]] <- as.POSIXct(ret[[i]])
    }
    col <- dat$cells$value[j[k]]
    col[vlapply(col, is.null)] <- list(NA)
    ret[k, i] <- tr[[t]](col)
  }

  names(ret) <- col_names
  ret
}

## TODO: this needs to change to accomodate reading the header
## information more generally, and for the case where we *do* want the
## header information read from the first row of a view.
table_col_names <- function(dat, col_names, skip = 0L) {
  nc <- dat$dim[[2]]
  skip <- 0L
  keep <- rep(TRUE, nc)
  if (is.logical(col_names)) {
    if (col_names) {
      if (inherits(dat, "worksheet_view")) {
        if (is.character(dat$header) && length(dat$header) == dat$dim[[2]]) {
          col_names <- dat$header
        } else {
          stop("header information not convertable to col_names")
        }
      } else {
        col_names <- rep(NA_character_, nc)
        i <- dat$lookup[skip + 1L, ]
        keep <- !is.na(i)
        nms <- dat$cells$value[i[keep]]
        j <- !vlapply(nms, is.null)
        col_names[keep][j] <- vcapply(nms[j], as.character)
        skip <- skip + 1L
      }
    } else {
      col_names <- NULL
    }
  } else if (is.character(col_names)) {
    if (length(col_names) != nc) {
      stop(sprintf("Expected %d col_names", nc))
    }
  } else {
    stop("`col_names` must be a logical or character vector")
  }

  list(col_names = col_names,
       skip = skip,
       keep = keep)
}

## The R time objects really want me poke my eyes out.  Perhaps there
## is a better way of doing this?  Who knows?
unlist_times <- function(x) {
  if (length(x) == 0L) {
    structure(numeric(0), class = c("POSIXct", "POSIXt"), tzone = "UTC")
  } else {
    tmp <- unlist(x)
    attributes(tmp) <- attributes(x[[1L]])
    tmp
  }
}
