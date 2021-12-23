import functools
from collections import deque
from pprint import pprint

import numpy
from numpy.lib import stride_tricks


array = functools.partial(numpy.array, dtype=int)
zeros = functools.partial(numpy.zeros, dtype=int)
nditer = functools.partial(numpy.nditer, flags=['multi_index'])


def read_input(filename):
    with open(filename) as filehandle:
        indata = filehandle.read()
    return indata


def parse_indata(txt):
    return array([array(list(s)) for s in txt.splitlines()]).astype(int)


@functools.lru_cache()
def neighbour_indexes(row, col, shape):
    rows, cols = shape
    indexes = []
    for rel in [1]:  # No backtracking
        vertical_index = row + rel
        if vertical_index in range(rows):
            indexes.append((vertical_index, col))
        horizontal_index = col + rel
        if horizontal_index in range(cols):
            indexes.append((row, horizontal_index))
    return indexes


def find_paths(arr, path_walked, end):
    visited = set(path_walked)
    position = path_walked[-1]
    if position == end:
        cost = sum(arr[p] for p in path_walked)
        yield cost, path_walked
    row, col = position
    ni = neighbour_indexes(row, col, arr.shape)
    for index in sorted(list(set(ni) - visited), reverse=True, key=lambda i: arr[i]):
        yield from find_paths(arr, path_walked + [index], end)


def part1(filename):
    indata = read_input(filename)
    # arr = parse_indata(indata)[:3, :3]
    arr = parse_indata(indata)
    start = (0, 0)
    lowest, rightmost = arr.shape
    end = (lowest - 1, rightmost - 1)
    print(arr)
    print(arr[start], arr[end])
    pprint(min(find_paths(arr, [start], end)))
    return arr


def part2(filename):
    indata = read_input(filename)
    return


def main():
    print(part1('input15_test1'))
    print(part1('input15'))
    # print(part2('input13'))


if __name__ == '__main__':
    main()
