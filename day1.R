# the optimised answer ----------------------------------------------------
day1 <- function(f="input1") {
  x=scan(f,what=integer(),quiet=TRUE)
  list(
    sum((x[-1]-x[-length(x)])>0),
    sum((x[4:length(x)] - x[seq_len(length(x)-3L)])>0)
  )
}

# the nice answer ---------------------------------------------------------

# x=scan("input1")
# sum(diff(x)>0)
# sum(diff(x,3)>0)

