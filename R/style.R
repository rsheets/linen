## Formatting information is this bottomless pit that never stops;
## it's just not possible to store all the information that might
## possibly be used in a spreadsheet and at the same time allow it to
## be used in a consistent way.  This is particularly the case before
## get a full set of data out of google sheets, because the set of
## information is not really known.

## In Excel, formatting information is stored in a series of tables:

## * A cell has an index to a "style"
##
## * That style includes lookups to a number of other tables of
##   information on fonts, fills, borders, etc.
##
## * The underlying tables have a bunch of information about colours
##   and the like

## Let's rationalise this a little.  I think that expanding the
## `cells$style` column out to 3 columns (fonts, fills, borders) would
## be worthwhile.  Or we impose a new lookup between so that we have a
## `lookup` table within the style information that cells point at.

## OTOH, some form of extensibility and tidiness is nice; we can
## probably keep the general approch with some effort, and provide a
## nice lookup facility.
linen_style <- function(lookup, ...) {
  tables <- list(...)
  nms <- names(tables)
  if (is.null(nms) || any(nms == "") || any(duplicated(nms))) {
    stop("Formatting information must be unique and non-empty")
  }

  msg <- setdiff(names(lookup), nms)
  if (length(msg) > 0L) {
    stop("Missing style information: ", paste(msg, collapse=", "))
  }
  msg <- setdiff(nms, names(lookup))
  if (length(msg) > 0L) {
    stop("Missing lookup information: ", paste(msg, collapse=", "))
  }

  if (!inherits(lookup, "tbl_df")) {
    stop("Expected lookup to be a tbl_df")
  }
  if (!all(vlapply(tables, inherits, "tbl_df"))) {
    stop("Expected all style information to be a tbl_df")
  }

  for (v in nms) {
    r <- range(lookup[[v]], na.rm=TRUE)
    if (r[[1L]] < 1 || r[[2L]] > nrow(tables[[v]])) {
      stop(sprintf("Out of range lookup for %s (should be in [1,%d]",
                   v, nrow(tables[[v]])))
    }
  }

  ret <- list(lookup=lookup, ...)
  class(ret) <- "linen_style"
  ret
}

get_style <- function(x) {
  if (inherits(x, "workbook")) {
    style <- x$style
  } else if (inherits(x, "worksheet")) {
    style <- x$workbook$style
  } else if (inherits(x, "linen_style")) {
    style <- x
  } else {
    stop("Not sure.")
  }
  style
}

style_valid <- function(x) {
  style <- get_style(x)
  nms <- setdiff(names(style), "lookup")
  unlist(lapply(nms, function(x) paste(x, names(style[[x]]), sep="/")))
}

style_lookup <- function(x, ...) {
  style <- get_style(x)
  if (is.null(style)) {
    return(NULL)
  }

  ## TODO: Deal with invalid entry request (will throw via tibble for now)
  els <- list(...)
  stopifnot(all(vlapply(els, is.character)))
  els <- strsplit(unlist(els), "/", fixed=TRUE)
  stopifnot(all(lengths(els) == 2))

  f <- function(el) {
    table <- el[[1L]]
    style[[table]][[el[[2L]]]][style$lookup[[table]]]
  }

  ret <- lapply(els, f)
  nms <- names(ret)
  if (is.null(nms)) {
    names(ret) <- as.character(list(...))
  } else if (!all(nzchar(nms))) {
    nms[!nzchar(nms)] <- as.character(list(...))[!nzchar(nms)]
    names(ret) <- nms
  }

  tibble::as_data_frame(ret)
}
