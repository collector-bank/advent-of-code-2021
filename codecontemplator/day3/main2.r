input <- read.table("input.txt", header=FALSE)

getMostFrequent <- function(vec) {
  xs <- sort(table(vec), decreasing = TRUE)
  if (var(xs) == 0) return (1)
  as.numeric(names(xs)[[1]])
}

getLeastFrequent <- function(vec) {
  xs <- sort(table(vec), decreasing = FALSE)
  if (var(xs) == 0) return (0)
  as.numeric(names(xs)[[1]])
}

binVectorToNumeric <- function(bv) {
  pv <- 2^(0:(length(bv)-1))
  sum(rev(bv) * pv)
}

solve <- function(getPivot) {
  rowMask <- rep(1,nrow(input))
  for(i in 1:ncol(input)) {
    if (sum(rowMask) == 1) 
      break;

    currentColumn <- input[, i]
    v <- getPivot(currentColumn[rowMask != 0])
    rowMask <- rowMask * (currentColumn == v)
  }

  d <- input[rowMask != 0,]
  binVectorToNumeric(d)
}

solve(getMostFrequent) * solve(getLeastFrequent)  # 4375225
