import math
from statistics import mean
from functools import partial


def get_input(fn):
    with open(fn) as fh:
        indata = fh.read().splitlines()
    return indata


bint = partial(int, base=2)


def bit_lines(lines):
    return [[int(char) for char in line] for line in lines]


def bit_cols(lines):
    return bit_lines(zip(*lines))


def common_bits(cols):
    return [round(mean(col)) for col in cols]


def join_bits(bit_arr):
    return bint(''.join(str(bit) for bit in bit_arr))


def complement(num):
    return bint('1' * num.bit_length()) ^ num


def part_1(lines):
    gamma = join_bits(common_bits(bit_cols(lines)))
    epsilon = complement(gamma)
    return gamma, epsilon, gamma * epsilon


assert part_1(get_input('input03_test')) == (22, 9, 198)
print(part_1(get_input('input03')))


def school_round(floating):
    floor = math.floor(floating)
    return floor if floating - floor < 0.5 else floor + 1


def find_line(all_lines, neg=False):
    filtered_lines = all_lines
    for i in range(len(all_lines[0])):
        bit = school_round(mean(
            list(zip(*filtered_lines))[i]
        ))
        if neg:
            bit = int(not bit)
        filtered_lines = [line for line in filtered_lines if line[i] == bit]
        if len(filtered_lines) == 1:
            break
    only_line, = filtered_lines
    return only_line


def part_2(lines):
    diagnostics_report = bit_lines(lines)
    oxygen_rating = join_bits(find_line(bit_lines(lines)))
    scrubber_rating = join_bits(find_line(bit_lines(lines), neg=True))
    return oxygen_rating, scrubber_rating, oxygen_rating * scrubber_rating


assert part_2(get_input('input03_test')) == (23, 10, 230)
print(part_2(get_input('input03')))
