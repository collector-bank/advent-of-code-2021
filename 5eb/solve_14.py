from itertools import pairwise
from collections import Counter
from functools import partial


def read_input(filename):
    with open(filename) as filehandle:
        indata = filehandle.read()
    first_line, rest = indata.split('\n\n')
    rest.splitlines()
    return list(first_line), rest.splitlines()


def parse_insertion_rule(line):
    """
    NN -> C
    to
    (('N', 'N'), 'C')
    """
    two, new = line.split(' -> ')
    return tuple(two), new


def polymerize(pair_insertion_rules, polymer):
    for pair in pairwise(polymer + ['']):
        first, _ = pair
        yield first
        if pair in pair_insertion_rules:
            yield pair_insertion_rules[pair]


def part1(filename, iterations):
    polymer_template, pair_insertion_lines = read_input(filename)
    pair_insertion_rules = dict(parse_insertion_rule(line) for line in pair_insertion_lines)
    polymer = polymer_template
    for i in range(iterations):
        polymer = list(polymerize(pair_insertion_rules, polymer))
    monomers = Counter(polymer).most_common()
    most = monomers[0][1]
    least = monomers[-1][1]
    return most - least


def polymer_count_up(pair_insertion_rules, polymer_pairs):
    new_dimers = Counter()
    new_monomers = Counter()
    for pair, count in polymer_pairs.items():
        first, second = pair
        middle = pair_insertion_rules[pair]
        new_dimers[(first, middle)] += count
        new_dimers[(middle, second)] += count
        new_monomers[middle] += count
    return new_monomers, new_dimers


def part2(filename, iterations):
    polymer_template, pair_insertion_lines = read_input(filename)
    pair_insertion_rules = dict(parse_insertion_rule(line) for line in pair_insertion_lines)
    monomers = Counter(polymer_template)
    dimers = Counter(pairwise(polymer_template))
    for i in range(iterations):
        new_monomers, dimers = polymer_count_up(pair_insertion_rules, dimers)
        monomers += new_monomers
        # print(i, monomers.most_common())
    sorted_monomers = monomers.most_common()
    most = sorted_monomers[0][1]
    least = sorted_monomers[-1][1]
    return most - least


def main():
    print(part1('input14_test1', 10))
    print(part1('input14', 10))
    assert part2('input14_test1', 10) == part1('input14_test1', 10)
    print(part2('input14_test1', 40))
    print(part2('input14', 40))


if __name__ == '__main__':
    main()
