from collections import Counter, deque


def read_input(fn):
    with open(fn) as fh:
        indata = fh.read()
    return [int(n) for n in indata.strip().split(',')]


def log(fish, day):
    print(f'After {day: >4} days:', repr(fish))


def part1(filename, days):
    fishies = read_input(filename)
    log(fishies, 0)
    for day in range(days):
        fishies = [fish - 1 for fish in fishies]
        fishies.extend(Counter(fishies)[-1] * [8])
        fishies = [fish * (fish >= 0) + (fish < 0) * 6 for fish in fishies]
        print(fishies, day + 1)
        log(fishies, day + 1)
    return len(fishies)


def part2(filename, days):
    fishies = read_input(filename)
    first_count = Counter(fishies)
    by_age = deque([first_count[age] for age in range(9)])
    log(by_age, 0)
    for day in range(days):
        by_age.rotate(-1)
        by_age[6] += by_age[8]
        log(by_age, day + 1)
    return sum(by_age)


def main():
    # print(part1('input06_test', 18))
    # assert part1('input06_test', 18) == 26
    # assert part1('input06_test', 80) == 5934
    # print(part1('input06', 80))
    assert part2('input06_test', 18) == 26
    assert part2('input06_test', 80) == 5934
    assert part2('input06_test', 256) == 26984457539
    print(part2('input06', 256))


if __name__ == '__main__':
    main()
