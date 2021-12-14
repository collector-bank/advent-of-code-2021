import pytest

from solve_11 import *


class TestIntegration:

    def test_part1(self):
        assert part1('input11_test1', 100) == 1656
