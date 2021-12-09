import functools
import operator

import numpy
from numpy import array


def read_input(filename):
    with open(filename) as filehandle:
        lines = filehandle.read().splitlines()
    return lines


def parse_input(lines):
    return array([array(list(line)) for line in lines]).astype(int)


def neighbour_indexes(row, col, shape):
    rows, cols = shape
    indexes = []
    for rel in [-1, 1]:
        vertical_index = row + rel
        if vertical_index in range(rows):
            indexes.append((vertical_index, col))
        horizontal_index = col + rel
        if horizontal_index in range(cols):
            indexes.append((row, horizontal_index))
    return indexes


def find_low_points(height_map):
    low_points = numpy.zeros(height_map.shape, dtype=int)
    for level in range(10):
        iterator = numpy.nditer(height_map, flags=['multi_index'])
        for point in iterator:
            if point == level:
                row, col = iterator.multi_index
                neighbours = []
                for nb_index in neighbour_indexes(row, col, height_map.shape):
                    neighbours.append(height_map[nb_index])
                if (array(neighbours) > level).all():
                    low_points[row, col] = 1
    return low_points


def total_risk_level(height_map):
    low_points = find_low_points(height_map)
    risk_level = low_points * (height_map + 1)
    return numpy.sum(risk_level)


def part1(filename):
    height_map = parse_input(read_input(filename))
    return total_risk_level(height_map)


def find_basins(height_map):
    map_shape = height_map.shape
    basin_map = (height_map < 9).astype(int)
    low_points = find_low_points(height_map)

    basins = []
    iterator = numpy.nditer(low_points, flags=['multi_index'])
    for low_point in iterator:
        if not low_point:
            continue
        basin = basin_map.copy()
        indicator = 2
        here = iterator.multi_index

        stack = {here}
        while stack:
            position = stack.pop()
            if basin[position] == 1:
                basin[position] = indicator
                stack |= set(neighbour_indexes(*position, map_shape))
        basins.append((basin == indicator).astype(int))
    return basins


def part2(filename):
    height_map = parse_input(read_input(filename))
    basins = find_basins(height_map)
    basin_sizes = [numpy.sum(basin) for basin in basins]
    three_biggest = sorted(basin_sizes, reverse=True)[:3]
    return functools.reduce(operator.mul, three_biggest)


def main():
    print(part1('input09'))
    print(part2('input09'))


if __name__ == '__main__':
    main()
