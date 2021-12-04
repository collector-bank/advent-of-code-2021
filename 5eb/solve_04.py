from functools import partial

import numpy as np
from numpy import array


npsum = partial(np.sum, keepdims=True, dtype=int)
zeros = partial(np.zeros, dtype=int)
ones = partial(np.ones, dtype=int)


def parse_input(fn):
    with open(fn) as fh:
        numbers_line = fh.readline()
        board_txt = fh.read()
    numbers = numbers_line.strip().split(',')
    boards = [
        [line.split() for line in board.split('\n')]
        for board in board_txt.strip().split('\n\n')]
    return array(numbers).astype(int), array(boards).astype(int)


def filter_eq(match, arr):
    return (arr == match) * 1


def bingo(numbers, boards):
    winning_board = zeros(boards.shape)
    marked = zeros(boards.shape, dtype=int)
    for number in numbers:
        marked += filter_eq(number, boards)
        rows = npsum(marked, axis=2)
        cols = npsum(marked, axis=1)
        winning_board += boards * npsum(filter_eq(5, rows), axis=1)
        winning_board += boards * npsum(filter_eq(5, cols), axis=2)
        if winning_board.any():
            return number, winning_board, marked


def calc_score(number, winning_board, marked):
    left_on_board = (1 - marked) * winning_board
    score = np.sum(left_on_board) * number
    return score


def part1(filename):
    return calc_score(*bingo(*parse_input(filename)))


def filter_any(arr):
    return (arr > 0) * 1


def find_last_bingo(numbers, boards):
    marked = zeros(boards.shape)
    for number in numbers:
        marked += filter_eq(number, boards)
        row_sums = npsum(marked, axis=2)
        bingo_by_rows = filter_any(npsum(filter_eq(5, row_sums), axis=1))
        col_sums = npsum(marked, axis=1)
        bingo_by_cols = filter_any(npsum(filter_eq(5, col_sums), axis=2))
        has_won = bingo_by_rows | bingo_by_cols
        # Activate to see a nice matrix flow of boards winning:
        # print(''.join(has_won.reshape(boards.shape[0]).astype(str)))
        has_not_won = 1 - has_won
        if np.sum(has_not_won) == 1:
            last_board = has_not_won * boards
            return last_board


def part2(filename):
    numbers, boards = parse_input(filename)
    last_board = find_last_bingo(numbers, boards)
    return calc_score(*bingo(numbers, last_board))


def main():
    test_score1 = part1('input04_test')
    assert test_score1 == 4512
    print(part1('input04'))
    test_score2 = part2('input04_test')
    assert test_score2 == 1924
    print(part2('input04'))


if __name__ == '__main__':
    main()
