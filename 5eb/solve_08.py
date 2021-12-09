from collections import defaultdict
import pytest


def read_input(filename):
    with open(filename) as filehandle:
        indata = filehandle.read().splitlines()
    return indata


digit_segments_table = {
    0: 'abc efg',
    1: '  c  f ',
    2: 'a cde g',
    3: 'a cd fg',
    4: ' bcd f ',
    5: 'ab d fg',
    6: 'ab defg',
    7: 'a c  f ',
    8: 'abcdefg',
    9: 'abcd fg',
}


digit_segments = {
    0: 'abcefg',
    1: 'cf',
    2: 'acdeg',
    3: 'acdfg',
    4: 'bcdf',
    5: 'abdfg',
    6: 'abdefg',
    7: 'acf',
    8: 'abcdefg',
    9: 'abcdfg',
}
segment_digits = {frozenset(segs): d for (d, segs) in digit_segments.items()}

dark_segments = {d: ''.join(set('abcdefg') - set(segs)) for (d, segs) in digit_segments.items()}
{
    0: 'd',
    1: 'adgbe',
    2: 'bf',
    3: 'be',
    4: 'eag',
    5: 'ce',
    6: 'c',
    7: 'bedg',
    8: '',
    9: 'e',
}

lenseg_digits = defaultdict(set)
for (d, segments) in digit_segments.items():
    lenseg_digits[len(segments)].add(d)
lenseg_digits = dict(lenseg_digits)


def parse_line(line):
    return tuple(part.split() for part in line.split('|'))


def guess_digit_from_len(input_segments):
    candidate_digits = lenseg_digits[len(input_segments)]
    if len(candidate_digits) == 1:
        candidate_digit, = candidate_digits
        return candidate_digit


def part1(filename):
    indata = [parse_line(line) for line in read_input(filename)]
    total_identified = 0
    for unique_signal_patterns, output_value in indata:
        for digit in output_value:
            total_identified += int(bool(guess_digit_from_len(digit)))
    return total_identified



def decode_patterns(unique_signal_patterns):
    translator = {}
    reverse = {}
    distinct_lengths = {
        2: 1,
        3: 7,
        4: 4,
        7: 8,
    }
    possible_by_length = {
        6: {0, 6, 9},
        5: {2, 3, 5},
    }

    for pattern in unique_signal_patterns:
        pattern_length = len(pattern)
        if pattern_length in distinct_lengths:
            translator[pattern] = distinct_lengths[pattern_length]
            reverse[distinct_lengths[pattern_length]] = pattern

    cf = set(reverse[1])
    acf  = set(reverse[7])
    a = acf - cf
    bcdf = set(reverse[4])
    bd = bcdf - cf

    for pattern in unique_signal_patterns:
        if pattern in translator:
            continue
        possible_digits = possible_by_length[len(pattern)]
        pat = set(pattern)
        if len(pattern) == 6:  # {0, 6, 9}
            if (bcdf | a).issubset(pat):
                translator[pattern] = 9
            elif cf.issubset(pat):
                translator[pattern] = 0
            elif bd.issubset(pat):
                translator[pattern] = 6
        elif len(pattern) == 5:  # {2, 3, 5}
            if cf.issubset(pat):
                translator[pattern] = 3
            elif bd.issubset(pat):
                translator[pattern] = 5
            else:
                translator[pattern] = 2
    return translator


def translate_output(unique_signal_patterns, output_value):
    translator = decode_patterns(unique_signal_patterns)
    set_translator = {frozenset(segs): d for (segs, d) in translator.items()}
    set_output = [frozenset(segs) for segs in output_value]
    return int(''.join(str(set_translator[segments]) for segments in set_output))


def part2(filename):
    indata = [parse_line(line) for line in read_input(filename)]
    total = 0
    for unique_signal_patterns, output_value in indata:
        total += translate_output(unique_signal_patterns, output_value)
    return total


def main():
    print(part1('input08'))
    print(part2('input08_test1'))
    print(part2('input08_test2'))
    print(part2('input08'))


if __name__ == '__main__':
    main()
