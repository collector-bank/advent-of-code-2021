from collections import Counter


def read_input(fn):
    with open(fn) as fh:
        indata = fh.read()
    return [int(n) for n in indata.strip().split(',')]


def new_fish(old_fish):
    new = []
    for fish in old_fish:
        if fish == 0:
            new.append(8)
    return new


def log(fish, day):
    print(f'After {day: >4} days:', repr(fish).strip('[]'))


def part1(filename, days):
    fishies = read_input(filename)
    log(fishies, 0)
    for day in range(days):
        fishies = [fishie - 1 for fishie in fishies]
        fishies.extend(Counter(fishies)[0] * [8])
        fishies = [fishie or 6 for fishie in fishies]
        log(fishies, day + 1)
    return len(fishies)


def main():
    print(part1('input06_test', 18))
    # assert part1('input06_test', 18) == 26
    # assert part1('input06_test', 80) == 5934
    # print(part1('input06'))
    # assert part2('input06_test') == 12
    # print(part2('input06'))


if __name__ == '__main__':
    main()
