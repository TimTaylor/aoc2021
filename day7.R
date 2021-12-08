day7 <- function(f="input7") {
  x <- scan(f, what=1L, sep=",", quiet=TRUE)
  possible <- min(x):max(x)
  part1 <- vapply(possible, function(y) sum(abs(y-x)), double(1L)) |> min()
  part2 <- vapply(possible, function(y) {tmp <- abs(y-x); sum(tmp*(tmp+1)/2)}, double(1L)) |> min()
  list(part1, part2)
}

# TODO - add a median solution







