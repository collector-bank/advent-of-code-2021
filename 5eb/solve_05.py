from functools import partial

import numpy as np


zeros = partial(np.zeros, dtype=int)
eye = partial(np.eye, dtype=int)


def parse_coords(txtline):
    startstr, stopstr = txtline.split(' -> ')
    start = tuple(int(n) for n in startstr.split(','))
    stop = tuple(int(n) for n in stopstr.split(','))
    return start, stop


def read_input(fn):
    with open(fn) as fh:
        input_lines = fh.read().splitlines()
    return input_lines


def is_horizontal(coords):
    (_, y1), (_, y2) = coords
    return y1 == y2


def is_vertical(coords):
    (x1, _), (x2, _) = coords
    return x1 == x2


def find_span(coords):
    xs = [x1 for (x1, _), (_, _) in coords] + [x2 for (_, _), (x2, _) in coords]
    ys = [y1 for (_, y1), (_, _) in coords] + [y2 for (_, _), (_, y2) in coords]
    return max(xs), max(ys)


def filter_gte(than, arr):
    return (arr >= than) * 1


def part1(filename):
    coords = [parse_coords(line) for line in read_input(filename)]
    xmax, ymax = find_span(coords)
    board = zeros((ymax + 1, xmax + 1))
    for coord in coords:
        (x1, y1), (x2, y2) = coord
        if is_horizontal(coord):
            board[y1, min(x1, x2):max(x1, x2) + 1] += 1
        if is_vertical(coord):
            board[min(y1, y2):max(y1, y2) + 1, x1] += 1
    return np.sum(filter_gte(2, board))


def part2(filename):
    coords = [parse_coords(line) for line in read_input(filename)]
    xmax, ymax = find_span(coords)
    board = zeros((ymax + 1, xmax + 1))
    for coord in coords:
        (x1, y1), (x2, y2) = coord
        lower_x = min(x1, x2)
        upper_x = max(x1, x2) + 1
        lower_y = min(y1, y2)
        upper_y = max(y1, y2) + 1
        if is_horizontal(coord):
            board[y1, lower_x:upper_x] += 1
        elif is_vertical(coord):
            board[lower_y:upper_y, x1] += 1
        else:
            diagonal = eye(upper_x - lower_x)
            if not ((x1 < x2 and y1 < y2) or (x1 > x2 and y1 > y2)):
                diagonal = diagonal[::-1]
            board[lower_y:upper_y, lower_x:upper_x] += diagonal
    return np.sum(filter_gte(2, board))


def main():
    assert part1('input05_test') == 5
    print(part1('input05'))
    assert part2('input05_test') == 12
    print(part2('input05'))


if __name__ == '__main__':
    main()
