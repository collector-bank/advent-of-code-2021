def read_input(filename):
    with open(filename) as filehandle:
        indata = filehandle.read().splitlines()
    return indata


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


def parse_line(line):
    return tuple(part.split() for part in line.split('|'))


def guess_digit_from_len(input_segments):
    digit = distinct_lengths.get(len(input_segments))
    return digit


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

    for pattern in unique_signal_patterns:
        digit = distinct_lengths.get(len(pattern))
        if digit:
            translator[pattern] = digit
            reverse[digit] = pattern

    cf = set(reverse[1])
    bcdf = set(reverse[4])
    bd = bcdf - cf

    for pattern in unique_signal_patterns:
        if pattern in translator:
            continue
        pat = set(pattern)
        if len(pattern) == 5:  # {2, 3, 5}
            if cf.issubset(pat):
                translator[pattern] = 3
            elif bd.issubset(pat):
                translator[pattern] = 5
            else:
                translator[pattern] = 2
        if len(pattern) == 6:  # {0, 6, 9}
            if (bcdf).issubset(pat):
                translator[pattern] = 9
            elif cf.issubset(pat):
                translator[pattern] = 0
            elif bd.issubset(pat):
                translator[pattern] = 6
    return translator


def sort_segments(patterns):
    return [''.join(sorted(pattern)) for pattern in patterns]


def translate_output(unique_signal_patterns, output_value_patterns):
    unique_signal_patterns = sort_segments(unique_signal_patterns)
    output_value_patterns = sort_segments(output_value_patterns)
    translator = decode_patterns(unique_signal_patterns)
    return int(''.join(str(translator[segments]) for segments in output_value_patterns))


def part2(filename):
    indata = [parse_line(line) for line in read_input(filename)]
    total = 0
    for unique_signal_patterns, output_value in indata:
        total += translate_output(unique_signal_patterns, output_value)
    return total


def main():
    print(part1('input08'))
    print(part2('input08'))


if __name__ == '__main__':
    main()
