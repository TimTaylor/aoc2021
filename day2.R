day2 <- function(f="input2") {
  x <- scan(f, sep=" ", what = list("",1),quiet=TRUE)
  change <- integer(length(x[[1]]))
  change[x[[1]]=="down"] <- x[[2]][x[[1]]=="down"]
  change[x[[1]]=="up"] <- -x[[2]][x[[1]]=="up"]
  aim <- cumsum(change)
  idx <- x[[1]]=="forward"
  tmp <- x[[2]][idx]
  horizontal <- cumsum(tmp)
  depth <- cumsum(aim[idx]*tmp)
  list(
    sum(change)*sum(tmp),
    horizontal[length(horizontal)]*depth[length(depth)]
  )
}

