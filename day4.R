day4 <- function(f="input4") {
  input <- scan(f,what=1L,sep=",",nlines=1L,quiet=TRUE)
  boards <- scan(f,what=1L,skip=2L,quiet=TRUE)

  dummy <- match(boards, input)
  n <- length(boards)/25L
  boards <- array(boards,c(5L,5L,n))
  dummy <- array(dummy,c(5L,5L,n))
  res <- apply(dummy,3L, function(x) min(apply(x,1L,max), apply(x,2L,max)))

  first_board_idx <- which.min(res)
  first_board <- boards[,,first_board_idx]
  last_input_idx <- res[first_board_idx]
  inputs <- input[seq_len(last_input_idx)]
  a <- sum(first_board[!first_board %in% inputs])*input[last_input_idx]

  last_board_idx <- which.max(res)
  last_board <- boards[,,last_board_idx]
  last_input_idx <- res[last_board_idx]
  inputs <- input[seq_len(last_input_idx)]
  b <- sum(last_board[!last_board %in% inputs])*input[last_input_idx]

  list(a,b)
}

# day4loopy <- function(filepath) {
#   input <- scan(filepath,what=1L,sep=",",nlines=1L,quiet=TRUE)
#   boards <- scan(filepath,what=1L,skip=2L,quiet=TRUE)
#   n <- length(boards)/25L
#   boards <- array(boards,c(5L,5L,n))
#   dummy <- array(FALSE, c(5L,5L,n))
#   results <- numeric(n)
#   wins <- 0
#
#   for (i in input) {
#     if (wins==n) break
#     dummy[which(boards==i)] <- TRUE
#     rs <- apply(dummy,3L,rowSums)
#     cs <- apply(dummy,3L,colSums)
#     cond <- rs == 5L | cs == 5L
#     if (any(cond)) {
#       winners <- ((which(cond)-1L) %/% 5L) +1L
#       for (w in seq_along(winners)) {
#         wins <- wins+1
#         results[wins] <- sum(boards[,,winners[w]][!dummy[,,winners[w]]])*i
#       }
#       boards <- boards[,,-winners,drop=FALSE]
#       dummy <- dummy[,,-winners,drop=FALSE]
#     }
#   }
#   list(a=results[1], b=results[wins])
# }
