vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
viapply <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
}
vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

A1_to_matrix <- function(x) {
  if (length(x) > 0L) {
    ## TODO: this is oddly too slow when there are c. 100k references;
    ## it takes 11s to process!
    ##    ca <- cellranger::as.cell_addr_v(x, strict = FALSE)
    ##    cbind(row = cellranger::addr_row(ca),
    ##          col = cellranger::addr_col(ca))
    ## The workaround below could be ported back to cellranger
    ##
    ## With the approach here, it takes ~0.24s (so ~45x speed up and
    ## ~400k per second) but this is something that might be better
    ## done in C?
    re <- "^([A-Z]+)([0-9]+)$"
    if (!all(grepl(re, x))) {
      stop("Unexpected cell references (probably a bug)")
    }

    ## The row reference is nice and easy:
    row <- as.integer(sub(re, "\\2", x))

    ## Still too slow:
    ##   col <- cellranger::letter_to_num(sub(re, "\\1", x))
    ## Here's one approach; we could possibly to better than justify
    ## if I could work out how to unpack a ragged list a bit better.
    col_char <- format(sub(re, "\\1", x), justify = "right")
    n <- nchar(col_char[[1]])
    tmp <- matrix(match(unlist(strsplit(col_char, NULL)), LETTERS), n)
    tmp[is.na(tmp)] <- 0
    m <- 26^(rev(seq_len(n) - 1L))
    col <- colSums(tmp * m)
    cbind(row = row, col = col)
  } else {
    cbind(row = integer(0), col = integer(0))
  }
}

assert_list <- function(x, name=deparse(substitute(x))) {
  if (!is.list(x)) {
    stop(sprintf("%s must be a list", name), call. = FALSE)
  }
}

assert_logical <- function(x, name=deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("%s must be logical", name), call. = FALSE)
  }
}

assert_integer <- function(x, strict=FALSE, name=deparse(substitute(x))) {
  if (!(is.integer(x))) {
    usable_as_integer <-
      !strict && is.numeric(x) && (max(abs(as.integer(x) - x)) < 1e-8)
    if (!usable_as_integer) {
      stop(sprintf("%s must be integer", name), call. = FALSE)
    }
  }
}

assert_character <- function(x, name=deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("%s must be character", name), call. = FALSE)
  }
}

assert_length <- function(x, n, name=deparse(substitute(x))) {
  if (length(x) != n) {
    stop(sprintf("%s must have %d elements", name, n), call. = FALSE)
  }
}

assert_inherits <- function(x, what, name=deparse(substitute(x))) {
  if (!inherits(x, what)) {
    stop(sprintf("%s must be a %s", name,
                 paste(what, collapse = " / ")), call. = FALSE)
  }
}

xr_to_idx <- function(xr) {
  list(r = xr$ul[[1L]]:xr$lr[[1L]],
       c = xr$ul[[2L]]:xr$lr[[2L]])
}

crayon_col <- function(fg, bg) {
  crayon::combine_styles(
    crayon::make_style(fg),
    crayon::make_style(bg, bg = TRUE))
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
