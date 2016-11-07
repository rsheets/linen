print_sheet <- function(x, xr=NULL, bg=NULL, fg=NULL, ...) {
  ## Helper for the merged cells.
  print_merge <- function(el) {
    anc <- "\U2693"
    left <- "\U2190"
    up <- "\U2191"
    ul <- "\U2196"

    d <- dim(el)
    anchor <- el$ul
    loc <- loc_merge(el)
    if (d[[1]] == 1L) {
      str <- rep(left, d[[2L]])
    } else if (d[[2L]] == 1L) {
      str <- rep(up, d[[1L]])
    } else {
      str <- matrix(ul, d[[1]], d[[2]])
      str[1L, ] <- left
      str[, 1L] <- up
    }
    str[[1L]] <- anc
    list(loc=loc, str=str)
  }

  pos <- x$pos
  dim <- x$dim

  ## It would definitely be useful to be able to skip out on doing
  ## this for _all_ cells if we're working with a view / non-NULL xr,
  ## but I'm not sure that's worth the effort...

  m <- matrix(NA, dim[[1]], dim[[2]])

  for (el in x$merged) {
    tmp <- print_merge(el)
    m[tmp$loc] <- tmp$str
  }

  m[pos[x$cells$is_formula & x$cells$is_number, , drop=FALSE]] <- "="
  m[pos[x$cells$is_formula & x$cells$is_text,   , drop=FALSE]] <- "$"
  m[pos[x$cells$is_formula & x$cells$is_bool,   , drop=FALSE]] <- "!"
  m[pos[x$cells$is_formula & x$cells$is_date,   , drop=FALSE]] <- "#"
  m[pos[x$cells$is_value   & x$cells$is_number, , drop=FALSE]] <- "0"
  m[pos[x$cells$is_value   & x$cells$is_text,   , drop=FALSE]] <- "a"
  m[pos[x$cells$is_value   & x$cells$is_bool,   , drop=FALSE]] <- "b"
  m[pos[x$cells$is_value   & x$cells$is_date,   , drop=FALSE]] <- "d"
  empty_row <- apply(is.na(m), 1, all)
  empty_col <- apply(is.na(m), 2, all)
  m[is.na(m)] <- " "

  ## TODO: This probably generalises out at some point as this is
  ## pretty general; apply ansi colour to a table based on vectors of
  ## colour.
  if ((!is.null(bg) || !is.null(fg)) && crayon::num_colors() > 1L) {
    col <- cbind(if (is.null(bg)) NA else bg, if (is.null(fg)) NA else fg)
    col[is.na(col[, 1L]), 1L] <- "#FFFFFF"
    col[is.na(col[, 2L]), 2L] <- "#000000"
    col_tmp <- paste(col[, 1L], col[, 2L], sep="/")
    col_idx <- match(col_tmp, unique(col_tmp))
    col_uniq <- unique(col)
    col_funs <- apply(col_uniq, 1, function(x) crayon_col(x[[2]], x[[1]]))

    use <- matrix(NA, dim[[1]], dim[[2]])
    use[pos] <- col_idx
    for (el in x$merged) {
      tmp <- loc_merge(el)
      use[tmp[-1, , drop=FALSE]] <- use[tmp[1, , drop=FALSE]]
    }

    m <- m
    for (i in seq_along(col_funs)) {
      j <- which(use == i)
      m[j] <- col_funs[[i]](m[j])
    }

    j <- is.na(use)
    m[j] <- crayon_col("#000000", "#FFFFFF")(m[j])
  }

  clab <- rep(LETTERS, length.out=dim[[2]])
  rlab <- seq_len(dim[[1]])

  ## Here, I do want to check for a bunch of totally blank cells
  ## around the edges and clip them off.
  if (is.null(xr)) {
    xr <- cellranger::cell_limits(c(1, 1),
                                  c(max(which(!empty_row)),
                                    max(which(!empty_col))))
  }
  idx <- xr_to_idx(xr)
  m <- m[idx$r, idx$c, drop=FALSE]
  clab <- clab[idx$c]
  rlab <- rlab[idx$r]

  cat(paste(sprintf(
    "%s: %s\n",
    format(c("", rlab)),
    apply(rbind(clab, m), 1, paste, collapse="")), collapse=""))
}
