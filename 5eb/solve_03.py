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
    return [[int(char) for char in col] for col in zip(*lines)]

def common_bits(cols):
    return [round(mean(col)) for col in cols]

def join_bits(bit_arr):
    return bint(''.join(str(bit) for bit in bit_arr))

def complement(num):
    return bint('1' * num.bit_length()) ^ num

def part_1(lines):
    gamma = join_bits(common_bits(bit_cols(lines)))
    epsilon = complement(gamma)
    return gamma, epsilon

assert part_1(get_input('input03_test')) == (22, 9)
print(part_1(get_input('input03')))


def complement_list(bits):
    return [int(not bit) for bit in bits]


def filter_lines(all_lines, bits):
    filtered_lines = all_lines
    for i, bit in enumerate(bits):
        filtered_lines = [line for line in filtered_lines if line[i] == bit]
        print(filtered_lines)
        if len(filtered_lines) == 1:
            break
    only_line, = filtered_lines
    return only_line


def part_2(lines):
    diagnostics_report = bit_lines(lines)
    gamma_bits = common_bits(bit_cols(lines))
    epsilon_bits = complement_list(gamma_bits)
    # print(list(filter_lines(bit_lines(lines), gamma_bits)))
    # print(filter_lines(bit_lines(lines), epsilon_bits))
    oxygen_rating = join_bits(filter_lines(bit_lines(lines), gamma_bits))
    scrubber_rating = join_bits(filter_lines(bit_lines(lines), epsilon_bits))
    return oxygen_rating, scrubber_rating, oxygen_rating * scrubber_rating


print(part_2(get_input('input03_test')))
# assert part_2(get_input('input03_test')) == 230
