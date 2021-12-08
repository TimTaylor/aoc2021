day3 <- function(f="input3") {
  width <- nchar(readLines(f,n=1L))
  dat <- scan(f,quiet=TRUE)
  y <- 10L^(width:1L)
  z <- y/10L
  dat <- vapply(dat, function(x) (x %% y) %/% z, double(width))
  idx <- rowSums(dat) > ncol(dat)/2
  gamma <- strtoi(paste(as.integer(idx),collapse=""),2L)
  epsilon <- strtoi(paste(as.integer(!idx),collapse=""),2L)
  a <- gamma*epsilon

  tmp <- dat
  tmpidx <- idx
  i <- 1
  while(ncol(tmp)>1) {
    cols <- tmp[i,]==tmpidx[i]
    tmp <- tmp[,cols,drop=FALSE]
    i <- i+1
    tmpidx <- rowSums(tmp) >= ncol(tmp)/2
  }
  oxygen <- strtoi(paste(tmp,collapse=""),2L)
  tmp <- dat
  tmpidx <- idx
  i <- 1
  while(ncol(tmp)>1) {
    cols <- !tmp[i,]==tmpidx[i]
    tmp <- tmp[,cols,drop=FALSE]
    i <- i+1
    tmpidx <- rowSums(tmp) >= ncol(tmp)/2
  }
  c02 <- strtoi(paste(tmp,collapse=""),2L)
  b <- oxygen*c02
  list(a,b)

}



#
# fun3 <- function() {
#   width <- nchar(readLines("input3",n=1L))
#   dat <- scan("input3",quiet=TRUE)
#   y <- 10L^(width:1L)
#   z <- y/10L
#   tmp <- vapply(dat, function(x) (x %% y) %/% z, double(width))
#   idx <- rowSums(tmp) > ncol(tmp)/2
#   sum(2^(which(rev(idx))-1)) * sum(2^(which(rev(!idx))-1))
# }
