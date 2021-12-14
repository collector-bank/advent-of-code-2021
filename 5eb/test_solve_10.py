import pytest

from solve_10 import *


class TestFindCorrupted:

    def test_find_wrong_paren(self):
        assert find_wrong_paren('{([(<{}[<>[]}>{[]{[(<()>') == {'expected': ']', 'found':'}'}
        assert find_wrong_paren('[[<[([]))<([[{}[[()]]]') == {'expected': ']', 'found':')'}
        assert find_wrong_paren('[{[{({}]{}}([{[{{{}}([]') == {'expected': ')', 'found':']'}
        assert find_wrong_paren('[<(<(<(<{}))><([]([]()') == {'expected': '>', 'found':')'}
        assert find_wrong_paren('<{([([[(<>()){}]>(<<{{') == {'expected': ']', 'found':'>'}


    def test_find_wrong_paren(self):
        assert find_wrong_paren('[({(<(())[]>[[{[]{<()<>>') == {'closed_by': '}}]])})]'}
        assert find_wrong_paren('[(()[<>])]({[<{<<[]>>(') == {'closed_by': ')}>]})'}
        assert find_wrong_paren('(((({<>}<{<{<>}{[]{[]{}') == {'closed_by': '}}>}>))))'}
        assert find_wrong_paren('{<[[]]>}<{[{[{[]{()[[[]') == {'closed_by': ']]}}]}]}>'}
        assert find_wrong_paren('<{([{{}}[<[[[<>{}]]]>[]]') == {'closed_by': '])}>'}


class TestCompletionScore:

    def test_calc_completion_score(self):
        assert calc_completion_score('}}]])})]') == 288957
        assert calc_completion_score(')}>]})') == 5566
        assert calc_completion_score('}}>}>))))') == 1480781
        assert calc_completion_score(']]}}]}]}>') == 995444
        assert calc_completion_score('])}>') == 294


class TestIntegration:

    def test_part1(self):
        assert part1('input10_test1') == 26397

    def test_part2(self):
        assert part2('input10_test1') == 288957
