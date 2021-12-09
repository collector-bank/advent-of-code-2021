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
    translator = {
        'abcdefg': 8,
    }

    # connections = {}
    # usps = [''.join(sorted(pattern)) for pattern in unique_signal_patterns]
    # [
    #     'abcdefg',  'bcdef',  'acdfg',  'abcdf',  'abd',
    #     'abcdef',  'bcdefg',  'abef',  'abcdeg',  'ab',
    # ]
    # print(usps)
    # dark = [''.join(set('abcdefg') - set(segs)) for segs in unique_signal_patterns]
    # print('dark:', dark)
    # patterns = [set(p) for p in usps]
    # cf = next(p for p in patterns if len(p) == 2)
    # acf  = next(p for p in patterns if len(p) == 3)
    # connections['a'] = acf - cf
    # bcdf = next(p for p in patterns if len(p) == 4)
    # bd = bcdf - cf

    facit = {
        frozenset('abcdefg'): 8,
        frozenset('cdfbe'): 5,
        frozenset('gcdfa'): 2,
        frozenset('fbcad'): 3,
        frozenset('dab'): 7,
        frozenset('cefabd'): 9,
        frozenset('cdfgeb'): 6,
        frozenset('eafb'): 4,
        frozenset('cagedb'): 0,
        frozenset('ab'): 1,
    }
    return facit


def translate_output(unique_signal_patterns, output_value):
    translator = decode_patterns(unique_signal_patterns)
    return int(''.join(str(translator[frozenset(segments)]) for segments in output_value))


def part2(filename):
    indata = [parse_line(line) for line in read_input(filename)]
    total = 0
    for unique_signal_patterns, output_value in indata:
        total += translate_output(unique_signal_patterns, output_value)
    return total


def main():
    # print(part1('input08'))
    print(part2('input08_test1'))


if __name__ == '__main__':
    main()
