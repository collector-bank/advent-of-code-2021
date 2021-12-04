library(tidyverse)
library(rlist)

mark_value <- function(board, v) {
    board$mask <- board$mask * (board$values != v)
    board
}

is_done <- function(board) {
    sum(c(apply(board$mask, 1, sum), apply(board$mask, 2, sum)) == 0) > 0
}

get_result <- function(board, v) {
    sum(board$mask * board$values) * v
}

solve <- function(num_seq, boards) {
    winners <- rep(1, length(boards))
    for (v in num_seq) {
        print(v)
        for (i in 1:length(boards)) {
            if (winners[i] == 1) {
                boards[[i]] <- mark_value(boards[[i]], v)
                if (is_done(boards[[i]])) {
                    winners[i] <- 0
                    print(sprintf("winner %d", i));
                    if (sum(winners) == 0) {
                        return(get_result(boards[[i]], v))
                    }
                }
            }
        }
    }
    return(-1)
}

lines <- read_lines("input.txt")

num_seq <- as.integer(unlist(strsplit(lines[1], ",")))

board_rows <- lines[-1]
board_chunks <- split(board_rows, ceiling(seq_along(board_rows) / 6))
boards <- lapply(board_chunks, function(board_chunk) {
    board_data <- paste(board_chunk, collapse = "\r\n")
    values <- read_table(board_data, col_names = FALSE)
    list(
        values = values,
        mask = values * 0 + 1   # 1 = unmarked, 0 = marked
    )
});

solve(num_seq, boards)  # 26936