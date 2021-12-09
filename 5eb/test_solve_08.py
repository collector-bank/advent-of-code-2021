import pytest

from solve_08 import (
    read_input,
    parse_line,
    guess_digit_from_len,
    part1,
    translate_output,
    part2,
    decode_patterns,
)


def test_parse_line():
    line = 'acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf'
    expected = (
        [
            'acedgfb',  'cdfbe',  'gcdfa',  'fbcad',  'dab',
           'cefabd',  'cdfgeb',  'eafb',  'cagedb',  'ab',
        ], ['cdfeb',  'fcadb',  'cdfeb',  'cdbaf'])
    assert parse_line(line) == expected


def test_guess_digit():
    assert guess_digit_from_len('ab') == 1
    assert guess_digit_from_len('dab') == 7
    assert guess_digit_from_len('eafb') == 4
    assert guess_digit_from_len('dgebacf') == 8


def test_part1():
    assert part1('input08_test2') == 26


class TestPart2:

    def test_decode_patterns_normal_segments(self):
        pattern = ['abcefg', 'cf', 'acdeg', 'acdfg', 'bcdf', 'abdfg', 'abdefg', 'acf', 'abcdefg', 'abcdfg']
        expected = {
            'abcefg': 0,
            'cf': 1,
            'acdeg': 2,
            'acdfg': 3,
            'bcdf': 4,
            'abdfg': 5,
            'abdefg': 6,
            'acf': 7,
            'abcdefg': 8,
            'abcdfg': 9,
        }
        result = decode_patterns(pattern)
        assert result == expected

    def test_decode_patterns_example1(self):
        pattern = ['acedgfb', 'cdfbe', 'gcdfa', 'fbcad', 'dab', 'cefabd', 'cdfgeb', 'eafb', 'cagedb', 'ab']
        expected = {
            'acedgfb': 8,
            'cdfbe': 5,
            'gcdfa': 2,
            'fbcad': 3,
            'dab': 7,
            'cefabd': 9,
            'cdfgeb': 6,
            'eafb': 4,
            'cagedb': 0,
            'ab': 1,
        }
        result = decode_patterns(pattern)
        assert result == expected

    def test_translate_output(self):
        unique_signal_patterns, output_value = [
            parse_line(line)
            for line in read_input('input08_test1')
        ][0]
        assert translate_output(unique_signal_patterns, output_value) == 5353

    def test_part2(self):
        assert part2('input08_test2') == 61229
