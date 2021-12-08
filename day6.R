day6 <- function(f="input6") {
  input = scan(f, sep = ",",quiet=TRUE)
  tbl <- as.numeric(tabulate(input+1, 9L))

  m <- matrix(0,nrow=9L,ncol=9L)
  m2=matrix(c(1L:9L,2L:9L,1L),nrow=9L)
  m[m2] <- 1L
  m[7,1] <- 1L

  for (i in seq_len(80L)) tbl <- (m %*% tbl)
  part1 <- sum(tbl)
  for (j in 81:256) tbl <- (m %*% tbl)
  part2 <- sum(tbl)

  list(part1, part2)
}

# Tweaking Quentin Leclerc's
day6ql <- function(f="input6") {
  input = scan(f, sep = ",",quiet=TRUE)
  tbl <- as.numeric(tabulate(input+1, 9L))
  rotation <- c(2L:9L,1L)
  for (i in seq_len(80L)) {
    tbl[8] <- tbl[8] + tbl[1]
    tbl <- tbl[rotation]
  }
  part1 <- sum(tbl)
  for (i in 81:256) {
    tbl[8] <- tbl[8] + tbl[1];
    tbl <- tbl[rotation]
  }
  part2 <- sum(tbl)
  list(part1,part2)
}




#
# library(flexiblas)
# ignore <- "__FALLBACK__"
# backends <- setdiff(flexiblas_list(), ignore)
# idx <- flexiblas_load_backend(backends)
# timings <- lapply(idx, function(i) {
#   flexiblas_switch(i)
#   bench::mark(day6(),day6b()) # warmup
#   res <- bench::mark(day6(),day6ql(),iterations=1000)
#   tibble::tibble(res$expression, res$min, res$median, res$mem_alloc)
# })
# out <- tibble::as_tibble(do.call(rbind, timings))
# out <- cbind(rep(backends, each=2L), out)
#
#
