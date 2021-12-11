take_step <- function(m) {
    # increment each element as specified
    m <- m + 1

    # keep track of flashed cells
    mask <- m
    mask[, ] <- 1
    flashc <- 0

    # repeat until no more flashes
    repeat {
        # get new flashes
        flash <- (m > 9) * mask

        # update mask so that each cell only flashes at most once
        mask <- mask * !flash

        # get indices for new flashes
        flashi <- which(flash == TRUE, arr.ind = TRUE)
        flashc <- flashc + nrow(flashi)

        # are we done?
        if (nrow(flashi) == 0)
            break;

        # flash! - increment neigbours of flashing cells
        dv <- expand.grid(-1:1, -1:1)  # includes [0,0] but it does not matter
        row <- rowSums(expand.grid(flashi[, 1], dv[, 1]))
        col <- rowSums(expand.grid(flashi[, 2], dv[, 2]))
        ns <- cbind(row, col)
        rows <- ns[, 1] >= 1 &
                ns[, 1] <= nrow(m) &
                ns[, 2] >= 1 &
                ns[, 2] <= ncol(m)
        ns <- ns[rows, ]

        # m[ns] < m[ns] + 1 does not work since ns contains duplicates
        for (i in 1:nrow(ns)) {
            v <- ns[i, ]
            m[v[1], v[2]] <- m[v[1], v[2]] + 1
        }
    }

    # reset flashed cells
    m[mask == 0] <- 0

    # return state
    return(list(m = m, flashc = flashc))
}

solve_part_1 <- function(seed_m) {
    m <- seed_m
    num_steps <- 100
    flashc <- 0

    for (i in 1:num_steps) {
        r <- take_step(m)
        m <- r$m        
        flashc <- flashc + r$flashc
    }

    return(flashc)
}

solve_part_2 <- function(seed_m) {
    m <- seed_m
    step <- 0
    repeat {
        step <- step + 1
        r <- take_step(m)
        m <- r$m
        if (sum(m != 0) == 0)
            break;
    }
    return(step)
}

m <- read.table("input.txt")

solve_part_1(m) # 1673
solve_part_2(m) # 279
