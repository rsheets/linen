## This is the stupid exporter.  We'll get a decent exporter
## implemented later, but there's no great rush.  And things like
## TableToLongForm actually want a big table of text values so that's
## all cool.

to_values <- function(x) {
  ## This is duplicated from the unmerge_headers function but we'll
  ## want to separate that out at some point.
  i <- x$lookup
  array(lapply(sheet$cells$value[i], function(x) x %||% NA),
        dim(i))
}
