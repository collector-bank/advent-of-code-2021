from collections import deque
from functools import partial

import numpy
from numpy.lib import stride_tricks


array = partial(numpy.array, dtype=int)
zeros = partial(numpy.zeros, dtype=int)
nditer = partial(numpy.nditer, flags=['multi_index'])


def read_input(filename):
    with open(filename) as filehandle:
        lines = filehandle.read().splitlines()
    return array([list(line) for line in lines]).astype(int)


splash = array([
    [1, 1, 1],
    [1, 0, 1],
    [1, 1, 1],
])


def filter_gte(than, arr):
    return (arr >= than) * 1


def filter_eq(match, arr):
    return (arr == match) * 1


def neighbour_coords(row, col, shape):
    rows, cols = shape
    for vrel in [-1, 0, 1]:
        vertical_index = row + vrel
        if vertical_index not in range(rows):
            continue
        for hrel in [-1, 0, 1]:
            horizontal_index = col + hrel
            if horizontal_index not in range(cols):
                continue
            if hrel == vrel == 0:
                continue
            yield vertical_index, horizontal_index


def flashes(arr):
    result = arr.copy()
    rows, cols = shape = arr.shape
    stack = deque([(row, col) for row in range(rows) for col in range(cols)])
    while stack:
        row, col = stack.pop()
        result[row, col] += 1
        if result[row, col] == 10:
            stack.extendleft(neighbour_coords(row, col, shape))
    return result


def step_forward(arr, steps):
    total_flashes = 0
    for _ in range(steps):
        arr = flashes(arr)
        arr = numpy.where(arr < 10, arr, 0)
        yield arr


def count_flashes(arr):
    return numpy.sum(filter_eq(0, arr))


def part1(filename, iterations):
    octo_map = read_input(filename)
    total_flashes = sum(count_flashes(arr) for arr in step_forward(octo_map, iterations))
    return total_flashes


def part2(filename, max_iterations):
    octo_map = read_input(filename)
    for i, arr in enumerate(step_forward(octo_map, max_iterations), start=1):
        if count_flashes(arr) == 100:
            break
    return i


def main():
    print(part1('input11', 100))
    print(part2('input11', 1000))


if __name__ == '__main__':
    main()
