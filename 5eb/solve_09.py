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


def find_low_points(height_map):
    low_points = numpy.zeros(height_map.shape, dtype=bool)
    for level in range(10):
        for i, line in enumerate(height_map):
            for j, point in enumerate(line):
                if point == level:
                    neighbours = []
                    for nb_index in [-1, 1]:
                        vertical_index = i + nb_index
                        if vertical_index in range(height_map.shape[0]):
                            vertical_nb = height_map[vertical_index, j]
                            neighbours.append(vertical_nb)
                        horizontal_index = i + nb_index
                        if horizontal_index in range(height_map.shape[1]):
                            vertical_nb = height_map[i + nb_index, j]
                            neighbours.append(vertical_nb)
                    if (array(neighbours) > level).all():
                        low_points[i, j] = True
    return low_points


def total_risk_level(height_map):
    low_points = find_low_points(height_map)
    risk_level = low_points * (height_map + 1)
    return numpy.sum(risk_level)


def part1(filename):
    height_map = parse_input(read_input(filename))
    return total_risk_level(height_map)



def part2(filename):
    height_map = parse_input(read_input(filename))
    low_points = find_low_points(height_map)
    basin_sizes = [9, 14, 9]
    return functools.reduce(operator.mul, basin_sizes)


def main():
    print(part1('input09'))
    # print(part1('input09'))
    # print(part2('input09'))


if __name__ == '__main__':
    main()
