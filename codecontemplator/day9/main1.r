print("hello world")
d <- read.table("input.txt", header = FALSE)
d

nr <- dim(d)[1]
nc <- dim(d)[2]

sright <- cbind(rep(10,nr), d[,1:nc-1])
sleft <- cbind(d[,2:nc], rep(10,nr))

sup <- rbind(d[2:nr,], rep(10,nc))
sdown <- rbind(rep(10,nc), d[1:nr-1,])
sright
sleft
sup
sdown

print("-------------")

sum(d[d < sup & d < sdown & d < sright & d < sleft]+1)