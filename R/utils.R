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

## Could contribute to cellranger?
A1_to_matrix <- function(x) {
  char0_to_NA <- cellranger:::char0_to_NA
  rm_dollar_signs <- cellranger:::rm_dollar_signs

  stopifnot(is.character(x))

  x <- rm_dollar_signs(x)

  m <- regexec("[[:digit:]]*$", x)
  m <- regmatches(x, m)
  row_part <- as.integer(vapply(m, char0_to_NA, character(1)))
  row_part <- ifelse(row_part > 0, row_part, NA)

  m <- regexec("^[[:alpha:]]*", x)
  m <- regmatches(x, m)
  col_part <- cellranger::letter_to_num(vapply(m, char0_to_NA, character(1)))

  cbind(row=row_part, col=col_part)
}

assert_list <- function(x, name=deparse(substitute(x))) {
  if (!is.list(x)) {
    stop(sprintf("%s must be a list", name), call.=FALSE)
  }
}

assert_logical <- function(x, name=deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("%s must be logical", name), call.=FALSE)
  }
}

assert_integer <- function(x, strict=FALSE, name=deparse(substitute(x))) {
  if (!(is.integer(x))) {
    usable_as_integer <-
      !strict && is.numeric(x) && (max(abs(as.integer(x) - x)) < 1e-8)
    if (!usable_as_integer) {
      stop(sprintf("%s must be integer", name), call.=FALSE)
    }
  }
}

assert_character <- function(x, name=deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("%s must be character", name), call.=FALSE)
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
                 paste(what, collapse=" / ")), call.=FALSE)
  }
}

xr_to_idx <- function(xr) {
  list(r=xr$ul[[1L]]:xr$lr[[1L]],
       c=xr$ul[[2L]]:xr$lr[[2L]])
}
