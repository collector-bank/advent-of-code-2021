input <- read.table("input.txt", header=FALSE)

getMostFrequent <- function(vec) {
  as.numeric(names(sort(table(vec), decreasing = TRUE))[[1]])
}

getLeastFrequent <- function(vec) {
  as.numeric(names(sort(table(vec), decreasing = FALSE))[[1]])
}

binVectorToNumeric <- function(bv) {
  pv <- 2^(0:(length(bv)-1))
  sum(rev(bv) * pv)
}

xv <- apply(input, 2, getMostFrequent)
x <- binVectorToNumeric(xv)

yv <- apply(input, 2, getLeastFrequent)
y <- binVectorToNumeric(yv)

# xv
# yv

x*y  # 3885894
