from functools import partial

import numpy
from numpy import array


npsum = partial(numpy.sum, keepdims=True, dtype=int)
zeros = partial(numpy.zeros, dtype=int)


def parse_input(fn):
    with open(fn) as fh:
        numbers_line = fh.readline()
        board_txt = fh.read()
    numbers = [int(n) for n in numbers_line.strip().split(',')]
    boards = [
        [
            [int(n) for n in line.split()]
            for line in board.split('\n')]
        for board in board_txt.strip().split('\n\n')]
    return numbers, array(boards)


def filter_eq(match, arr):
    return (arr == match) * 1


def bingo(numbers, boards):
    winning_board = zeros(boards.shape)
    marked = zeros(boards.shape, dtype=int)
    for iteration, number in enumerate(numbers):
        marked += filter_eq(number, boards)
        rows = npsum(marked, axis=2)
        cols = npsum(marked, axis=1)
        winning_board += boards * npsum(filter_eq(5, rows), axis=1)
        winning_board += boards * npsum(filter_eq(5, cols), axis=2)
        if winning_board.any():
            return iteration, number, winning_board, marked


def calc_score(iteration, number, winning_board, marked):
    left_on_board = (1 - marked) * winning_board
    score = numpy.sum(left_on_board) * number
    return score


def part1(filename):
    return calc_score(*bingo(*parse_input(filename)))


def main():
    test_score1 = part1('input04_test')
    assert test_score1 == 4512
    print(part1('input04'))


if __name__ == '__main__':
    main()
