day5 <- function(f = "input5") {
  dat <- readLines(f)
  dat <- strcapture("(\\d+),(\\d+) -> (\\d+),(\\d+)", dat, proto = data.frame(x1=1,y1=1,x2=1,y2=1), perl=TRUE)

  line <- function(x1,y1,x2,y2) data.frame(x=seq.int(from=x1,to=x2), y=seq.int(from=y1,to=y2))

  tmp <- .mapply(line, dots=dat, MoreArgs = NULL)
  tmp <- do.call(rbind,tmp)
  b=nrow(unique(tmp[duplicated(tmp),]))

  dat <- dat[dat[[1]] == dat[[3]] | dat[[2]] == dat[[4]],]
  tmp=.mapply(line, dots=dat, MoreArgs = NULL)
  tmp <- do.call(rbind,tmp)
  a=nrow(unique(tmp[duplicated(tmp),]))

  list(a,b)
}

# based on @mpjdem's approach but altered a bit
# see https://twitter.com/mpjdem/status/1467421764384870402 and
#     https://twitter.com/_TimTaylor_/status/1467834552622231552
library(data.table)
day5dt <- function(f="input5") {
  input <- sub(pattern=" -> ", replacement = ",", x=readLines(f), fixed=TRUE)

  DT <- scan(text=input, what=integer(),sep=",",quiet=TRUE) |>
    matrix(ncol=4L,byrow=TRUE,dimnames = list(NULL,c("x1","y1","x2","y2"))) |>
    as.data.table() |>
    set(j="id",value=seq_along(input))

  a <- DT[x1 == x2 | y1 == y2, .(x = x1:x2, y = y1:y2), by = .(id)][,id:=NULL]
  a <- uniqueN(a[duplicated(a)])

  b <- DT[, .(x = x1:x2, y = y1:y2), by = .(id)][,id:=NULL]
  b <- uniqueN(b[duplicated(b)])

  list(a,b)
}

