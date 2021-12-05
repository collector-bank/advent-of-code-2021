input <- read.table("input.txt", header = FALSE)

n <- dim(input)[1]
xs <- input[c(1, 3)]
xs <- xs - min(xs) + 1
ys <- input[c(2, 4)]
ys <- ys - min(ys) + 1

m <- matrix(0, max(ys), max(xs))

for (i in 1:n) {
    x1 <- xs[i, 1]
    x2 <- xs[i, 2]
    y1 <- ys[i, 1]
    y2 <- ys[i, 2]

    cs <- cbind(y1:y2, x1:x2)
    m[cs] <- m[cs] + 1
}

sum(m > 1) # 16793
