from collections import deque
from functools import partial

import numpy
from numpy.lib import stride_tricks


zeros = partial(numpy.zeros, dtype=int)
nditer = partial(numpy.nditer, flags=['multi_index'])


def read_input(filename):
    with open(filename) as filehandle:
        indata = filehandle.read()
    dot_block, fold_block = indata.split('\n\n')
    dot_lines = [tuple(dot_line.split(',')) for dot_line in dot_block.splitlines()]
    fold_instructions = [tuple(fold_line.split()[-1].split('=')) for fold_line in fold_block.splitlines()]
    return dot_lines, fold_instructions


def draw_dots(dot_lines):
    coords = [(int(y), int(x)) for (x, y) in dot_lines]
    rows = max(y for y, _ in coords) + 1
    cols = max(x for _, x in coords) + 1
    rows += int(not rows % 2)  # Last row needed was empty of dots
    arr = zeros((rows, cols))
    for coord in coords:
        arr[coord] = 1
    return arr


def fold_up(arr, along):
    upper = arr[:along, :]
    lower = arr[along + 1:, :]
    return upper | lower[::-1]


def fold_left(arr, along):
    left = arr[:, :along]
    right = arr[:, along + 1:]
    return left | right[:, ::-1]


def fold(arr, fold_orientation, along):
    if fold_orientation == 'y':
        arr = fold_up(arr, int(along))
    elif fold_orientation == 'x':
        arr = fold_left(arr, int(along))
    return arr


def vis(arr, fold_orient='', fold_line='0'):
    marking_char = {'y': '-', 'x': '|'}.get(fold_orient, '.')
    for r, row in enumerate(arr):
        empty_r = marking_char if int(fold_line) == r and fold_orient == 'y' else '.'
        for c, num in enumerate(row):
            empty_char = marking_char if int(fold_line) == c and fold_orient == 'x' else empty_r
            print('#' * num or empty_char, end='')
        print()
    print()


def part1(filename):
    dot_lines, fold_instructions = read_input(filename)
    arr = draw_dots(dot_lines)
    for i, fold_instruction in enumerate(fold_instructions):
        # vis(arr, *fold_instruction)
        arr = fold(arr, *fold_instruction)
        break
    # vis(arr, *fold_instruction)
    return numpy.sum(arr)


def part2(filename):
    dot_lines, fold_instructions = read_input(filename)
    arr = draw_dots(dot_lines)
    for i, fold_instruction in enumerate(fold_instructions):
        arr = fold(arr, *fold_instruction)
    vis(arr)
    return numpy.sum(arr)

def main():
    print(part1('input13_test1'))
    print(part1('input13'))
    print(part2('input13'))


if __name__ == '__main__':
    main()
