import pytest

from solve_09 import *


test_input1 = (
    '2199943210\n'
    '3987894921\n'
    '9856789892\n'
    '8767896789\n'
    '9899965678\n'
).splitlines()


def test_parse_input():
    arr = parse_input(test_input1)
    assert arr.shape == (5, 10)
    assert arr[0, 0] == 2
    assert arr[-1, -1] == 8

class TestFindLowPoint:

    def test_find_low_points_contained(self):
        height_map = array([
            [2, 2, 2],
            [2, 1, 2],
            [2, 2, 2],
        ])
        expected = array([
            [0, 0, 0],
            [0, 1, 0],
            [0, 0, 0],
        ])
        assert (find_low_points(height_map) == expected).all()

    def test_find_low_points_edge(self):
        height_map = array([
            [1, 2],
            [2, 2],
        ])
        expected = array([
            [1, 0],
            [0, 0],
        ])
        assert (find_low_points(height_map) == expected).all()

    def test_find_low_points_no_diagonal_leaking(self):
        height_map = array([
            [1, 3, 1],
            [3, 2, 3],
            [1, 3, 1],
        ])
        expected = array([
            [1, 0, 1],
            [0, 1, 0],
            [1, 0, 1],
        ])
        assert (find_low_points(height_map) == expected).all()


def test_total_risk_level_input1():
    height_map = (parse_input(test_input1))
    assert total_risk_level(height_map) == 15


class TestIntegration:

    def test_part1(self):
        assert part1('input09_test1') == 15

    def test_part2(self):
        assert part2('input09_test1') == 1134
