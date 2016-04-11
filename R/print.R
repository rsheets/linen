print_sheet <- function(x, xr=NULL, ...) {
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
  m[pos[x$cells$is_formula & x$cells$is_date,   , drop=FALSE]] <- "d"
  m[is.na(m)] <- " "

  clab <- rep(LETTERS, length.out=dim[[2]])
  rlab <- seq_len(dim[[1]])

  if (!is.null(xr)) {
    idx <- xr_to_idx(xr)
    m <- m[idx$r, idx$c]
    clab <- clab[idx$c]
    rlab <- rlab[idx$r]
  }

  cat(paste(sprintf(
    "%s: %s\n",
    format(c("", rlab)),
    apply(rbind(clab, m), 1, paste, collapse="")), collapse=""))
}
