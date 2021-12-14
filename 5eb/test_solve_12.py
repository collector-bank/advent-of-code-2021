import pytest

from solve_12 import *


class TestIntegration:

    def test_part1(self):
        assert part1('input12_test1') == 10
        assert part1('input12_test2') == 19
        assert part1('input12_test3') == 226

    def test_part2(self):
        assert part2('input12_test1') == 36
        assert part2('input12_test2') == 103
        assert part2('input12_test3') == 3509
